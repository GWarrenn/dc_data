library(tidyverse)

places_of_worship <- read.csv("C:/Users/augus/Desktop/religion_in_dc/Places_of_Worship.csv")

head(places_of_worship$NAME)

##############################################################
##
## pattern matching bits of church name to get denomination
##
##############################################################

## lord help me with this ifelse

places_of_worship$denomination <- ifelse(grepl(x=places_of_worship$NAME,pattern = "BAPTIST|BAPT|GOSPEL|NEW ABUNDANT LIFE MISSIONARY",ignore.case = T)==T,"BAPTIST",
                                         ifelse(grepl(x=places_of_worship$NAME,pattern = "LUTHERAN",ignore.case = T)==T,"LUTHERAN",
                                                ifelse(grepl(x=places_of_worship$NAME,pattern = "EPISCOPAL|CHURCH OF THE EPIPHANY",ignore.case = T)==T,"EPISCOPAL",
                                                       ifelse(grepl(x=places_of_worship$NAME,pattern = "UNITARIAN|UNIVERSAL",ignore.case = T)==T,"UNITARIAN/UNIVERSALISTS",
                                                              ifelse(grepl(x=places_of_worship$NAME,pattern = "METHODIST|MTHDST|CME| AME |A M E| AME$|MILES MEMORIAL CHRISTIAN|ISRAEL METROPOLITAN CHRISTIAN")==T,"METHODIST",
                                                                     ifelse(grepl(x=places_of_worship$NAME,pattern = "PRESBYTERIAN",ignore.case = T)==T,"PRESBYTERIAN",
                                                                            ifelse(grepl(x=places_of_worship$NAME,pattern = "ORTHODOX",ignore.case = T)==T & grepl(x=places_of_worship$NAME,pattern = "ETHIOPIAN")==F,"EASTERN ORTHODOX",
                                                                                   ifelse(grepl(x=places_of_worship$NAME,pattern = "ORTHODOX",ignore.case = T)==T & grepl(x=places_of_worship$NAME,pattern = "ETHIOPIAN")==T,"ETHIOPIAN ORTHODOX",
                                                                                          ifelse(grepl(x=places_of_worship$NAME,pattern = "CATHOLIC|BASILICA-THE NATIONAL SHRINE|CATHEDRAL|IMMACULATE|Shrine of the Sacred Heart|St Aloysius |SAINT PETERS|ST MARYS MOTHER OF GOD|ST STEPHEN MARTYR CHURCH|ST ALOYSIUS PARISH|ST DOMINICS CHURCH|OUR LADY QUEEN-THE AMERICAS|HOLY ROSARY CHURCH|HOLY COMFORTER-ST CYPRIAN CHR|FRANCISCAN MONASTERY",ignore.case = T)==T,"CATHOLIC",
                                                                                                        ifelse(grepl(x=places_of_worship$NAME,pattern = "PENTACOST|PENTECOST|APOSTOLIC|APSTLC",ignore.case = T)==T,"PENTACOST",
                                                                                                               ifelse(grepl(x=places_of_worship$NAME,pattern = "TABERNACLE|EVANGEL|EVANGLICAL",ignore.case = T)==T,"EVANGELICAL",
                                                                                                                ifelse(grepl(x=places_of_worship$NAME,pattern = "SEVENTH|ADVENTIST| SDA ",ignore.case = T)==T,"SEVENTH DAY ADVENTIST",
                                                                                                                       ifelse(grepl(x=places_of_worship$NAME,pattern = "JEHOVAH",ignore.case = T)==T,"JEHOVAH'S WITNESS",
                                                                                                                              ifelse(grepl(x=places_of_worship$NAME,pattern = " UCC |UNITED CHURCH| UCC$|REFORMED",ignore.case = T)==T,"PROTESTANT - UNITED CHURCH",""))))))))))))))
##############################################################
##
## that's about as good as it's going to get 
## without further investigation beyond name
## sigh...
##
##############################################################

manual_look_ups <- places_of_worship %>%
  filter(denomination == "" & RELIGION == "CHRISTIAN") %>%
  select(NAME,WEB_URL,ADDRESS)

write.csv(file = "C:/Users/augus/Desktop/religion_in_dc/manual_lookups.csv",x = manual_look_ups)
  