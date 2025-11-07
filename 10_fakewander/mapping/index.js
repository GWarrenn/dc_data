mapboxgl.accessToken = 'pk.eyJ1IjoiZ3dhcnJlbm4iLCJhIjoiY2p4d294Z2xhMGh4czNub2N1c202dnNvdCJ9.iRGx2PURnTzXBHgRIH2zKg';

map_style = 'mapbox://styles/gwarrenn/ck0sl05xqetj01crs3u4prvyb'

function changeMapType(){
	currentMode = document.querySelector('input[name="maptype"]:checked').value;
	if (currentMode === 'Minimal') {
		map_style = 'mapbox://styles/gwarrenn/ck0sl05xqetj01crs3u4prvyb'
	}	
	if (currentMode === 'Detailed') {
		map_style = 'mapbox://styles/mapbox/dark-v10'
	}	

	plot()
}

// Mapping

const plot = async () => {

	const map = new mapboxgl.Map({
		container: 'map',
		style: map_style,
		center: [-77.030034, 38.92],
		showZoom: true,
		//pitch: 40,
		zoom: 11
	});

	map.addControl(new MapboxGeocoder({
		accessToken: mapboxgl.accessToken,
		mapboxgl: mapboxgl
	}));

	map.addControl(new mapboxgl.NavigationControl());

	map.on('load', function() {
		map.addLayer({
			"id": 'cycling-results',
			"type": "line",
			"source": {
				type: 'vector',
				url: 'mapbox://gwarrenn.4vcdgr42'
			},
			"source-layer": "output-bxyjc6",
			"layout": {
				"line-join": "round",
				"line-cap": "round"
			},
			"paint": {
				"line-color": [
              'interpolate',
              		['linear'],
						['get', 'file_name'], // Get the 'value' property
							0, '#440154',   // Start of viridis range (dark purple)
                            25, '#414487',
                            50, '#2A7886',
                            100, '#5AD06E',
                            125, '#A0DA39',
                            250, '#FDE725'    // End of viridis range (bright yellow)
							],
				"line-width": 3,
				"line-opacity": 1
			},
		})
	});
}

plot()