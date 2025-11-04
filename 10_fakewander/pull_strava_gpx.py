from strava2gpx import strava2gpx
from stravalib.client import Client
from joblib import Parallel, delayed
import webbrowser  
import asyncio

import pandas as pd

import re
import os
import shutil
import time
import tqdm

import geopandas as gpd
from shapely.geometry import Point, Polygon

import gpxpy
import gpxpy.gpx



def read_gpx_file(file_name,path="./export_4778598/activities/"):
    '''
    Reading in individual GPX file using gpxpy package and turn into a dataframe
    for further geographic intersection calculations.
    '''

    gpx_file_path = path + file_name

    with open(gpx_file_path, 'r') as gpx_file:
        # Parse the GPX data
        gpx = gpxpy.parse(gpx_file)

    points_data = []
    for track in gpx.tracks:
        for segment in track.segments:
            for point in segment.points:
                points_data.append({
                    'time': point.time,
                    'latitude': point.latitude,
                    'longitude': point.longitude,
                    'elevation': point.elevation,
                })

    df = pd.DataFrame.from_records(points_data)
    
    return(df)

def match_to_blocks(gpx_df,block_centroids):
    '''
    Take GPX data from Strava and match to nearest block centorid (using data sourced from DCOpenData)
    Using 450 foot radius as a max for nearest possible match.
    '''

    #geometry = gpd.points_from_xy(block_centroids['LONGITUDE'], block_centroids['LATITUDE'])

    #gdf1 = gpd.GeoDataFrame(block_centroids,geometry = geometry, crs="EPSG:4326")
    gdf1 = gpd.GeoDataFrame(block_centroids, crs="EPSG:4326")

    geometry = gpd.points_from_xy(gpx_df['longitude'], gpx_df['latitude'])
    gdf2 = gpd.GeoDataFrame(gpx_df, geometry = geometry, crs="EPSG:4326")

    gdf1_proj = gdf1.to_crs("EPSG:3857")
    gdf2_proj = gdf2.to_crs("EPSG:3857")

    ## find intersections

    #joined_gdf = gdf2_proj.sjoin_nearest(gdf1_proj, how="inner", max_distance=450,distance_col='distance')
    joined_gdf = gdf2_proj.sjoin_nearest(gdf1_proj, how="inner", max_distance=25,distance_col='distance')

    return(joined_gdf)

def process_gpx_files(gpx_list):
    '''
    Main function to both read and match gpx data to block centroids for paralellization
    '''

    all_data_df = pd.DataFrame()

    for gpx_file in tqdm.tqdm(gpx_list):

        try:

            gpx_df = read_gpx_file(gpx_file)
                
            #block_centroids = pd.read_csv('Block_Centroids.csv')
            block_centroids = gpd.read_file("Street_Centerlines_1999.geojson")

            gpx_matched_df = match_to_blocks(gpx_df,block_centroids=block_centroids)
            gpx_matched_df['file_name'] = gpx_file

            ## sometimes thematching goes haywire at intersections, so we fix this by removing streets that just appear
            ## randomly in the middle of another street

            for i in range(0,2):

                gpx_matched_df['prev_street'] = gpx_matched_df['ST_NAME'].shift(1)
                gpx_matched_df['next_street'] = gpx_matched_df['ST_NAME'].shift(-1)

                gpx_matched_df = gpx_matched_df[(gpx_matched_df['prev_street'] == gpx_matched_df['ST_NAME']) & (gpx_matched_df['next_street'] == gpx_matched_df['ST_NAME'])]

            all_data_df = pd.concat([all_data_df,gpx_matched_df])
            
        except Exception as e:
            print("Error with {}: {}".format(gpx_file,e))

    return(all_data_df)

def strava_authentication(cred_auth):

    client = Client()

    auth_url = client.authorization_url(
        client_id=cred_auth['CLIENT_ID'],
        redirect_uri="http://localhost:8000/authorization",
        scope=["read", "activity:read_all"]
    )

    webbrowser.open(auth_url)

    refresh_code = input("Enter code from browser: ")

    token_response = client.exchange_code_for_token(client_id=cred_auth['CLIENT_ID'], client_secret=cred_auth['CLIENT_SECRET'], code=refresh_code)

    return(token_response)

async def get_activities_list(cred_auth,token_response):

    client_id = cred_auth['CLIENT_ID']
    refresh_token = token_response['refresh_token']
    client_secret = cred_auth['CLIENT_SECRET']

    # create an instance of strava2gpx
    s2g = strava2gpx(client_id, client_secret, refresh_token)

    # connect to the Strava API
    await s2g.connect()

    # get a list of all user's Strava activities
    activities_list = await s2g.get_activities_list()

    return(activities_list,s2g)

async def pull_activity_gpx(export_df,export_path,activities_list,s2g):

    bad_files = []
    preserve_export_files = False

    start_time = time.time()
    iteration_count = 0
    time_limit_seconds = 15 * 60

    for activity in activities_list:

        file_name = str(activity[1])
        file_exists = False

        if (activity[3] == 'Ride'):

            for old_file in os.listdir(export_path):
                if file_name in old_file:
                    file_exists = True
                if preserve_export_files:
                    if int(file_name) in export_df['Activity ID'].to_list():
                        file_exists = True

            if not file_exists:

                ## so time checks to avoid API rate-limit errors

                current_time = time.time()
                elapsed_time = current_time - start_time

                iteration_count += 1

                if (elapsed_time < time_limit_seconds) & (iteration_count >= 100):
                    print("API limit hit ... gonna go to sleep for 15 mins")
                    time.sleep(15*60)

                try:
                    print("Pulling GPX data for: {}".format(str(activity[0])))
                    await s2g.write_to_gpx(activity[1],file_name)

                    ## moving file to processing directory

                    shutil.move(os.path.join('./{}.gpx'.format(file_name)), os.path.join('{}/{}.gpx'.format(export_path,file_name)))

                except Exception as e:
                    print(e)
                    if "Failed to get activity" in str(e):
                        exit
                    else:
                        bad_files.append(activity[1])

def gpx_to_streets(export_path):

    gpx_file_list = []

    for file in os.listdir(export_path):
        if 'gpx' in file:
            gpx_file_list.append(file)

    n_iterations = (os.cpu_count() // 2) - 2 ## this can be played with a bit here, anything higher than 5 tends to clog up compute and your computer may catch on fire
    chunk_size = round(len(gpx_file_list) / n_iterations) ## creating the dataframe chunk sizes based on n_iterations -- more iterations --> smaller chunks

    try:
        chunks = [gpx_file_list[i:i + chunk_size] for i in range(0, len(gpx_file_list), chunk_size)]
        df_list = chunks[0:n_iterations]
        
    except Exception as e:
        print(e)
        df_list = [gpx_file_list]

    results = Parallel(n_jobs=n_iterations, prefer="threads")(delayed(process_gpx_files)(gpx_list) for gpx_list in df_list)

    all_results = pd.DataFrame()

    for i in results:
        all_results = pd.concat([all_results,i])

    ## Aggregate coverage

    stats = all_results.groupby(['OBJECTID'])['file_name'].nunique().reset_index()

    centroids = pd.read_csv('Street_Centerlines_1999.csv')

    centroids_w_stats = pd.merge(centroids,stats,on='OBJECTID',how='left')

    centroids_w_stats.to_csv("geocoded_results_20251103.csv",index=False)

async def main():

    export_path = "./export_4778598/activities/"
    export_df = pd.read_csv("export_4778598/activities.csv")

    cred_auth = eval(open("strava_credentials.txt").read())

    token_response = strava_authentication(cred_auth)

    activities_list,s2g = await get_activities_list(cred_auth,token_response)

    await pull_activity_gpx(export_df,export_path,activities_list,s2g)

    gpx_to_streets(export_path)

if __name__ == '__main__':  
    asyncio.run(main())