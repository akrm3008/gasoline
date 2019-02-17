# In this script I visualise the shortage tweets at city level and aggragate them for each city
library(ggmap)
library(ggplot2)
library(dplyr)


# Downloading maps of Florida and cites of Flrida using get_map

map_florida <- get_map(location = 'Florida', zoom = 7)
ggmap(map_florida)

map_tal <- get_map(location= 'Tallahassee')
map_tampa <- get_map(location= 'Tampa')
map_miami <- get_map(location= 'Miami')
map_jack <- get_map(location='Jacksonville')
map_orlando <- get_map(location='Orlando')
map_gaine <- get_map(location= "Gainesville")
map_palm_beach <- get_map(location= "West Palm Beach")
map_myers <- get_map(location= "Fort Myers")
map_naples <- get_map(location= "Naples, FL" , zoom=9)


# Getting bounding box of each city

bb_tampa <- attr(map_tampa,"bb")  # Tampa and surrounding suburbs and cities
bb_miami <- attr(map_miami,"bb")  # miami and surrounding sunburbs and cities
bb_florida <- attr(map_florida,"bb")
bb_florida <- attr(map_florida,"bb")
bb_tal <- attr(map_tal,"bb")
bb_jack <- attr(map_jack,"bb")
bb_orlando <- attr(map_orlando,"bb")
bb_gaine <- attr(map_gaine,"bb")
bb_palm <- attr(map_palm_beach,"bb")
bb_naples <- attr(map_naples,"bb")


# Tweets with exact location 


# Finding tweets with  exact geolocation (latitude and longitudes given)
df_withexact_loc <- dfgas %>% filter(!is.na(LATITUDE) )
tweets_withexact_loc <- dfgas %>% filter(!is.na(LATITUDE) ) %>% group_by(PLACE_TYPE,PLACE_NAME,DATE) %>% 
  summarise(num_of_tweets= n())  %>% arrange(desc(num_of_tweets))


## bbox of tweets in poi ,hood
df_poi <- dfgas_short %>% filter(PLACE_TYPE== "poi") 
df_hood <- dfgas_short %>% filter(PLACE_TYPE== "neighborhood")

# Realised poi's have exact latitude and longitude 

# However in an instance latitude and logitude is different from bbox 

df_withexact_loc <- dplyr::union(df_withexact_loc,df_poi)

# Finding tweets without exact geolocation (latitude and longitudes given)

df_without_loc <- dplyr::setdiff(dfgas, df_withexact_loc)


df_withexact_loc <- df_withexact_loc %>% mutate(LATITUDE =ifelse(is.na(LATITUDE), PLACE_BBOX_LB_LAT, LATITUDE),
                                                LONGITUDE =ifelse(is.na(LONGITUDE), PLACE_BBOX_LB_LON,LONGITUDE))


#dfbb <- data.frame(PLACE_NAME= as.character(),lat =as.numeric(), lon = as.numeric(), 
#                                            group=as.numeric())

place = unique(df_without_loc$PLACE_NAME)[1]
g <- 1

dfbb <-  data.frame(
  PLACE_NAME= place,
  long = c(unique(dfgas_short[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_LB_LON),
           unique(dfgas_short[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_LT_LON),
           unique(dfgas_short[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_RT_LON),
           unique(dfgas[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_RB_LON)),
  lat = c(unique(dfgas[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_LB_LAT),
          unique(dfgas[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_LT_LAT),
          unique(dfgas[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_RT_LAT),
          unique(dfgas[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_RB_LAT)),
  stringsAsFactors = FALSE,
  group= c(g,g,g,g)
)  


for (place in unique(df_without_loc$PLACE_NAME) ){
  if (g==1){ 
    g= g+1
  }
  
  else{
    temp <-  data.frame(
      PLACE_NAME = place,
      long = c(unique(dfgas_short[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_LB_LON),
               unique(dfgas_short[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_LT_LON),
               unique(dfgas_short[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_RT_LON),
               unique(dfgas_short[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_RB_LON)),
      lat = c(unique(dfgas_short[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_LB_LAT),
              unique(dfgas_short[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_LT_LAT),
              unique(dfgas_short[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_RT_LAT),
              unique(dfgas_short[dfgas_short$PLACE_NAME== place,]$PLACE_BBOX_RB_LAT)),
      stringsAsFactors = FALSE,
      group= c(g,g,g,g))  
    
    dfbb <- rbind(dfbb,temp)
    g=g+1
  }
}


# Mappin bounding boxes and exact location of tweets for Florida and its cities using ggmap

ggmap(map_florida) + 
  geom_polygon(data = dfbb, aes(x=long, y = lat, group = group), fill = NA, color = "red") +
  geom_point(aes(x = LONGITUDE, y = LATITUDE), colour = "black", 
             alpha = 1, size = 0.1, data = df_withexact_loc) 

tweet_origin_tampa <- ggmap(map_tampa) + 
  geom_polygon(data = dfbb, aes(x=long, y = lat, group = group), fill = NA, color = "red")+
  geom_point(aes(x = LONGITUDE, y = LATITUDE), colour = "black", 
             alpha = 1, size = 1, data = df_withexact_loc) + ggtitle("Tampa")

tweet_origin_miami <- ggmap(map_miami) + 
  geom_polygon(data = dfbb, aes(x=long, y = lat, group = group), fill = NA, color = "red")+
  geom_point(aes(x = LONGITUDE, y = LATITUDE), colour = "black", 
             alpha = 1, size = 1, data = df_withexact_loc) + ggtitle("Miami")


tweet_origin_jack <- ggmap(map_jack) + 
  geom_polygon(data = dfbb, aes(x=long, y = lat, group = group), fill = NA, color = "red")+
  geom_point(aes(x = LONGITUDE, y = LATITUDE), colour = "black", 
             alpha = 1, size = 1, data = df_withexact_loc) + ggtitle("Jacksonville")


tweet_origin_orlando <- ggmap(map_orlando) + 
  geom_polygon(data = dfbb, aes(x=long, y = lat, group = group), fill = NA, color = "red")+
  geom_point(aes(x = LONGITUDE, y = LATITUDE), colour = "black", 
             alpha = 1, size = 1, data = df_withexact_loc) + ggtitle("Orlando")

ggmap(map_gaine) + 
  geom_polygon(data = dfbb, aes(x=long, y = lat, group = group), fill = NA, color = "red")+
  geom_point(aes(x = LONGITUDE, y = LATITUDE), colour = "black", 
             alpha = 1, size = 1, data = df_withexact_loc)


ggmap(map_tal) + 
  geom_polygon(data = dfbb, aes(x=long, y = lat, group = group), fill = NA, color = "red")+
  geom_point(aes(x = LONGITUDE, y = LATITUDE), colour = "black", 
             alpha = 1, size = 1, data = df_withexact_loc)


ggmap(map_naples) + 
  geom_polygon(data = dfbb, aes(x=long, y = lat, group = group), fill = NA, color = "red")+
  geom_point(aes(x = LONGITUDE, y = LATITUDE), colour = "black", 
             alpha = 1, size = 1, data = df_withexact_loc)


ggmap(map_palm_beach) + 
  geom_polygon(data = dfbb, aes(x=long, y = lat, group = group), fill = NA, color = "red")+
  geom_point(aes(x = LONGITUDE, y = LATITUDE), colour = "black", 
             alpha = 1, size = 1, data = df_withexact_loc)


grid.arrange(tweet_origin_tampa,tweet_origin_miami,tweet_origin_jack,tweet_origin_orlando, nrow = 2)

