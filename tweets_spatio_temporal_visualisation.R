# In this script I visulaise the arrival of gasoline-shortage tweets in space and time (classified in previous stage) 

library(data.table)
library(dplyr)
library(ggplot2)
library(ggmap)
library(chron)

# Getting data that indicate gasoline shortage from the data that is talking about gasoline 
dfgas_short <- dfgas %>% filter(label == 1)


# Building heatamp of tweets


# To build a heatmap latitude and longitude (geo-location) of each tweet is required 
# A lot of the has a bounding box for the tweets and not the exact geolocation.
# For tweets with bounding boxes, we use the center of the bounding-boxes as the geolocation for 
# building the heatmap
dfgas_short<- dfgas_short %>% arrange(LONGITUDE1)
dfgas_short[dfgas_short$PLACE_BBOX_LB_LON < -82.79 & dfgas_short$PLACE_NAME=="Florida, USA",]$PLACE_BBOX_LB_LON <- -82.79
dfgas_short <- dfgas_short %>% mutate(LATITUDE1 = ifelse(is.na(LATITUDE)==FALSE, 
                                                         LATITUDE,(PLACE_BBOX_LB_LAT+ PLACE_BBOX_LT_LAT)/2),
                                      LONGITUDE1= ifelse(is.na(LONGITUDE)==FALSE, LONGITUDE,
                                                         (PLACE_BBOX_LB_LON+ PLACE_BBOX_RT_LON)/2))


# Point distribution of shortage tweets 

map2 <- ggmap(map, extent = "device") + geom_point(aes(x = LONGITUDE1, y = LATITUDE1), colour = "red", 
                                                   alpha = 1, size = 2, data = dfgas_short)

# Heatmap of shortage tweets with scales of alpha 

map3 <- ggmap(map, extent = "device") + geom_density2d(data = dfgas_short, 
                                                       aes(x = LONGITUDE1, y = LATITUDE1), size = 0.3) + stat_density2d(data = dfgas_ethan_yes, 
                                                                                                                        aes(x = LONGITUDE1, y = LATITUDE1, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                        bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

map4 <- ggmap(map, extent = "device") + geom_density2d(data = dfgas_short, 
                                                       aes(x = LONGITUDE1, y = LATITUDE1), size = 0.3) + stat_density2d(data =dfgas_ethan_yes, 
                                                                                                                        aes(x = LONGITUDE1, y = LATITUDE1, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                        bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") + scale_alpha(range = c(0.5, 1), guide = FALSE)

map5 <- ggmap(map, extent = "device") + geom_density2d(data = dfgas_short, 
                                                       aes(x = LONGITUDE1, y = LATITUDE1), size = 0.3) + stat_density2d(data =dfgas_ethan_yes, 
                                                                                                                        aes(x = LONGITUDE1, y = LATITUDE1, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                        bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") + scale_alpha(range = c(0, 1), guide = FALSE)

# Heat map evolution with Date and Time

dfgas_short$TIME<- chron(times=dfgas_short$TIME)


# Between advisories
gsept6_1 <- dfgas %>% filter(DATE=="9/6/2017", TIME >=chron(times="03:00:00"), TIME <=chron(times="09:00:00"))
gsept6_2 <- dfgas %>% filter(DATE=="9/6/2017", TIME >=chron(times="09:00:00"), TIME <=chron(times="15:00:00"))
gsept6_3 <- dfgas %>% filter(DATE=="9/6/2017", TIME >=chron(times="15:00:00"), TIME <=chron(times="21:00:00"))
gsept6_4 <- rbind(dfgas %>% filter(DATE=="9/6/2017", TIME >=chron(times="21:00:00"), TIME <=chron(times="23:59:59")),
                  dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="00:00:00"), TIME <=chron(times="03:00:00")))


gsept7_1 <- dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="03:00:00"), TIME <=chron(times="09:00:00"))
gsept7_2 <- dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="09:00:00"), TIME <=chron(times="15:00:00"))
gsept7_3 <- dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="15:00:00"), TIME <=chron(times="21:00:00"))
gsept7_4 <- rbind(dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="21:00:00"), TIME <=chron(times="23:59:59")),
                  dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="00:00:00"), TIME <=chron(times="03:00:00")))


gsept8_1 <- dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="03:00:00"), TIME <=chron(times="09:00:00"))
gsept8_2 <- dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="09:00:00"), TIME <=chron(times="15:00:00"))
gsept8_3 <- dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="15:00:00"), TIME <=chron(times="21:00:00"))
gsept8_4 <- rbind(dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="21:00:00"), TIME <=chron(times="23:59:59")),
                  dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="00:00:00"), TIME <=chron(times="03:00:00")))


gsept9_1 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="03:00:00"), TIME <=chron(times="09:00:00"))
gsept9_2 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="09:00:00"), TIME <=chron(times="15:00:00"))
gsept9_3 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="15:00:00"), TIME <=chron(times="21:00:00"))
gsept9_4 <- rbind(dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="21:00:00"), TIME <=chron(times="23:59:59")),
                  dfgas %>% filter(DATE=="9/10/2017", TIME >=chron(times="00:00:00"), TIME <=chron(times="03:00:00")))

# mapping

gmap_Sept6_1<- ggmap(map, extent = "device") + geom_density2d(data =gsept6_1, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept6_1, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 



gmap_Sept6_2<- ggmap(map, extent = "device") + geom_density2d(data =gsept6_2, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept6_2, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

gmap_Sept6_3<- ggmap(map, extent = "device") + geom_density2d(data =gsept6_3, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept6_3, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

gmap_Sept6_4<- ggmap(map, extent = "device") + geom_density2d(data =gsept6_4, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept6_4, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
gmap_Sept7_1<- ggmap(map, extent = "device") + geom_density2d(data =gsept7_1, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept7_1, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
gmap_Sept7_2<- ggmap(map, extent = "device") + geom_density2d(data =gsept7_2, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept7_2, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
gmap_Sept7_3<- ggmap(map, extent = "device") + geom_density2d(data =gsept7_3, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept7_3, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
gmap_Sept7_4<- ggmap(map, extent = "device") + geom_density2d(data =gsept7_4, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept7_4, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 


gmap_Sept8_1<- ggmap(map, extent = "device") + geom_density2d(data =gsept8_1, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept8_1, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

gmap_Sept8_2<- ggmap(map, extent = "device") + geom_density2d(data =gsept8_2, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept8_2, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

gmap_Sept8_3<- ggmap(map, extent = "device") + geom_density2d(data =gsept8_3, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept8_3, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
gmap_Sept8_4<- ggmap(map, extent = "device") + geom_density2d(data =gsept8_4, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept8_4, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
gmap_Sept9_1<- ggmap(map, extent = "device") + geom_density2d(data =gsept9_1, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept9_1, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
gmap_Sept9_2<- ggmap(map, extent = "device") + geom_density2d(data =gsept9_2, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept9_2, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
gmap_Sept9_3<- ggmap(map, extent = "device") + geom_density2d(data =gsept9_3, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept9_3, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red")
gmap_Sept9_4<- ggmap(map, extent = "device") + geom_density2d(data =gsept9_4, 
                                                              aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept9_4, 
                                                                                                                               aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                               bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
# Between days 

gsept6 <- dfgas %>% filter(DATE=="9/6/2017", TIME >=chron(times="06:00:00"), TIME <=chron(times="23:59:59"))


gsept7 <- dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="06:00:00"), TIME <=chron(times="23:59:59"))



gsept8 <- dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="06:00:00"), TIME <=chron(times="23:59:59"))


gsept9 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="06:00:00"), TIME <=chron(times="23:59:59"))

# mapping

gmap_Sept6<- ggmap(map, extent = "device") + geom_density2d(data =gsept6, 
                                                            aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept6, 
                                                                                                                             aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                             bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") + ggtitle("6th September")




gmap_Sept7<- ggmap(map, extent = "device") + geom_density2d(data =gsept7, 
                                                            aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept7, 
                                                                                                                             aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                             bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") + ggtitle("7th September")



gmap_Sept8<- ggmap(map, extent = "device") + geom_density2d(data =gsept8, 
                                                            aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept8, 
                                                                                                                             aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                             bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") + ggtitle("8th September")


gmap_Sept9<- ggmap(map, extent = "device") + geom_density2d(data =gsept9, 
                                                            aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data =gsept9, 
                                                                                                                             aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                             bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") + ggtitle("9th September")

grid.arrange(gmap_Sept6, gmap_Sept7,gmap_Sept8,gmap_Sept9, nrow = 2)

# Public advisories 

pgsept6_1 <- dfgas %>% filter(DATE=="9/6/2017", TIME >=chron(times="05:00:00"), TIME <=chron(times="08:00:00"))
pgsept6_2 <- dfgas %>% filter(DATE=="9/6/2017", TIME >=chron(times="08:00:00"), TIME <=chron(times="11:00:00"))
pgsept6_3 <- dfgas %>% filter(DATE=="9/6/2017", TIME >=chron(times="11:00:00"), TIME <=chron(times="14:00:00"))
pgsept6_4 <- dfgas %>% filter(DATE=="9/6/2017", TIME >=chron(times="14:00:00"), TIME <=chron(times="17:00:00"))
pgsept6_5 <- dfgas %>% filter(DATE=="9/6/2017", TIME >=chron(times="17:00:00"), TIME <=chron(times="20:00:00"))
pgsept6_6 <- dfgas %>% filter(DATE=="9/6/2017", TIME >=chron(times="20:00:00"), TIME <=chron(times="23:00:00"))
pgsept6_7 <- rbind(dfgas %>% filter(DATE=="9/6/2017", TIME >=chron(times="23:00:00"), TIME <=chron(times="23:59:59")),
                   dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="00:00:00"), TIME <=chron(times="02:00:00")))
pgsept6_8 <- dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="02:00:00"), TIME <=chron(times="05:00:00"))



pgmap_Sept6_1<- ggmap(map, extent = "device") + geom_density2d(data=pgsept6_1, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept6_1, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 



pgmap_Sept6_2<- ggmap(map, extent = "device") + geom_density2d(data=pgsept6_2, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept6_2, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

pgmap_Sept6_3<- ggmap(map, extent = "device") + geom_density2d(data=pgsept6_3, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept6_3, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

pgmap_Sept6_4<- ggmap(map, extent = "device") + geom_density2d(data=pgsept6_4, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept6_4, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red")
pgmap_Sept6_5<- ggmap(map, extent = "device") + geom_density2d(data=pgsept6_5, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept6_5, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
pgmap_Sept6_6<- ggmap(map, extent = "device") + geom_density2d(data=pgsept6_6, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept6_6, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
pgmap_Sept6_7<- ggmap(map, extent = "device") + geom_density2d(data=pgsept6_7, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept6_7, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
pgmap_Sept6_8<- ggmap(map, extent = "device") + geom_density2d(data=pgsept6_8, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept6_7, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red")
pgsept7_1 <- dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="05:00:00"), TIME <=chron(times="08:00:00"))
pgsept7_2 <- dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="08:00:00"), TIME <=chron(times="11:00:00"))
pgsept7_3 <- dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="11:00:00"), TIME <=chron(times="14:00:00"))
pgsept7_4 <- dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="14:00:00"), TIME <=chron(times="17:00:00"))
pgsept7_5 <- dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="17:00:00"), TIME <=chron(times="20:00:00"))
pgsept7_6 <- dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="20:00:00"), TIME <=chron(times="23:00:00"))
pgsept7_7 <- rbind(dfgas %>% filter(DATE=="9/7/2017", TIME >=chron(times="23:00:00"), TIME <=chron(times="23:59:59")),
                   dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="00:00:00"), TIME <=chron(times="02:00:00")))
pgsept7_8 <- dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="02:00:00"), TIME <=chron(times="05:00:00"))




pgmap_Sept7_1<- ggmap(map, extent = "device") + geom_density2d(data=pgsept7_1, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept7_1, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 



pgmap_Sept7_2<- ggmap(map, extent = "device") + geom_density2d(data=pgsept7_2, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept7_2, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

pgmap_Sept7_3<- ggmap(map, extent = "device") + geom_density2d(data=pgsept7_3, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept7_3, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

pgmap_Sept7_4<- ggmap(map, extent = "device") + geom_density2d(data=pgsept7_4, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept7_4, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red")
pgmap_Sept7_5<- ggmap(map, extent = "device") + geom_density2d(data=pgsept7_5, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept7_5, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
pgmap_Sept7_6<- ggmap(map, extent = "device") + geom_density2d(data=pgsept7_6, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept7_6, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
pgmap_Sept7_7<- ggmap(map, extent = "device") + geom_density2d(data=pgsept7_7, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept7_7, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
pgmap_Sept7_8<- ggmap(map, extent = "device") + geom_density2d(data=pgsept7_8, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept7_7, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red")



pgsept8_1 <- dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="05:00:00"), TIME <=chron(times="08:00:00"))
pgsept8_2 <- dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="08:00:00"), TIME <=chron(times="11:00:00"))
pgsept8_3 <- dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="11:00:00"), TIME <=chron(times="14:00:00"))
pgsept8_4 <- dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="14:00:00"), TIME <=chron(times="17:00:00"))
pgsept8_5 <- dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="17:00:00"), TIME <=chron(times="20:00:00"))
pgsept8_6 <- dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="20:00:00"), TIME <=chron(times="23:00:00"))
pgsept8_7 <- rbind(dfgas %>% filter(DATE=="9/8/2017", TIME >=chron(times="23:00:00"), TIME <=chron(times="23:59:59")),
                   dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="00:00:00"), TIME <=chron(times="02:00:00")))
pgsept8_8 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="02:00:00"), TIME <=chron(times="05:00:00"))




pgmap_Sept8_1 <- ggmap(map, extent = "device") + geom_density2d(data=pgsept8_1, 
                                                                aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept8_1, 
                                                                                                                                 aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 



pgmap_Sept8_2<- ggmap(map, extent = "device") + geom_density2d(data=pgsept8_2, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept8_2, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

pgmap_Sept8_3<- ggmap(map, extent = "device") + geom_density2d(data=pgsept8_3, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept8_3, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

pgmap_Sept8_4<- ggmap(map, extent = "device") + geom_density2d(data=pgsept8_4, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept8_4, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red")
pgmap_Sept8_5<- ggmap(map, extent = "device") + geom_density2d(data=pgsept8_5, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept8_5, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
pgmap_Sept8_6<- ggmap(map, extent = "device") + geom_density2d(data=pgsept8_6, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept8_6, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
pgmap_Sept8_7<- ggmap(map, extent = "device") + geom_density2d(data=pgsept8_7, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept8_7, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
pgmap_Sept8_8<- ggmap(map, extent = "device") + geom_density2d(data=pgsept8_8, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept8_7, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 


pgsept9_1 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="05:00:00"), TIME <=chron(times="08:00:00"))
pgsept9_2 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="08:00:00"), TIME <=chron(times="11:00:00"))
pgsept9_3 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="11:00:00"), TIME <=chron(times="14:00:00"))
pgsept9_4 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="14:00:00"), TIME <=chron(times="17:00:00"))
pgsept9_5 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="17:00:00"), TIME <=chron(times="20:00:00"))
pgsept9_6 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="20:00:00"), TIME <=chron(times="23:00:00"))
pgsept9_7 <- rbind(dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="23:00:00"), TIME <=chron(times="23:59:59")),
                   dfgas %>% filter(DATE=="9/10/2017", TIME >=chron(times="00:00:00"), TIME <=chron(times="02:00:00")))
pgsept9_8 <- dfgas %>% filter(DATE=="9/10/2017", TIME >=chron(times="02:00:00"), TIME <=chron(times="05:00:00"))



pgmap_Sept9_1<- ggmap(map, extent = "device") + geom_density2d(data=pgsept9_1, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept9_1, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 



pgmap_Sept9_2<- ggmap(map, extent = "device") + geom_density2d(data=pgsept9_2, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept9_2, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

pgmap_Sept9_3<- ggmap(map, extent = "device") + geom_density2d(data=pgsept9_3, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept9_3, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 

pgmap_Sept9_4<- ggmap(map, extent = "device") + geom_density2d(data=pgsept9_4, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept9_4, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red")
pgmap_Sept9_5<- ggmap(map, extent = "device") + geom_density2d(data=pgsept9_5, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept9_5, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
pgmap_Sept9_6<- ggmap(map, extent = "device") + geom_density2d(data=pgsept9_6, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept9_6, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
pgmap_Sept9_7<- ggmap(map, extent = "device") + geom_density2d(data=pgsept9_7, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept9_7, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 
pgmap_Sept9_8<- ggmap(map, extent = "device") + geom_density2d(data=pgsept9_8, 
                                                               aes(x = LONGITUDE2, y = LATITUDE2), size = 0.3) + stat_density2d(data=pgsept9_7, 
                                                                                                                                aes(x = LONGITUDE2, y = LATITUDE2, fill = ..level.., alpha = ..level..), size = 0.01, 
                                                                                                                                bins = 16, geom = "polygon") + scale_fill_gradient(low = "blue", high = "red") 


# Getting tweets to compare with ground truth 

gtsept6_1 <- dfgas %>% filter(DATE=="6/9/2017", TIME >=chron(times="05:00:00"), TIME <=chron(times="08:00:00"))
pgsept9_2 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="08:00:00"), TIME <=chron(times="11:00:00"))
pgsept9_3 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="11:00:00"), TIME <=chron(times="14:00:00"))
pgsept9_4 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="14:00:00"), TIME <=chron(times="17:00:00"))
pgsept9_5 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="17:00:00"), TIME <=chron(times="20:00:00"))
pgsept9_6 <- dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="20:00:00"), TIME <=chron(times="23:00:00"))
pgsept9_7 <- rbind(dfgas %>% filter(DATE=="9/9/2017", TIME >=chron(times="23:00:00"), TIME <=chron(times="23:59:59")),
                   dfgas %>% filter(DATE=="9/10/2017", TIME >=chron(times="00:00:00"), TIME <=chron(times="02:00:00")))
pgsept9_8 <- dfgas %>% filter(DATE=="9/10/2017", TIME >=chron(times="02:00:00"), TIME <=chron(times="05:00:00"))



