library(geosphere);library(gtfsr);library(dplyr);
library(lubridate);library(tidyr)
library(leaflet);library(xml2);library(gtfsdrilldown)

pm <- "http://gtfs.muni.org/People_Mover.gtfs.zip" %>% import_gtfs
pm_dd <- gtfs_drilldown(gtfs_obj = pm, today = Sys.Date())

### FILTER OUT TRIPS THAT HAVE NOT2 STARTED YET
# Remove future trips so they can not be attributed.
old_trips <- todays_previous_departures(drilldown_obj = pm_dd)

### GET CURRENT BUS LOCATIONS AND ATTRIBUTE ROUTE AND DIRECTION AND NUMBER IN TRANSIT
bus_gps <- "http://bustracker.muni.org/InfoPoint/XML/vehiclelocation.xml" %>% read_xml

current_gps <-
  data.frame(lat =          bus_gps %>% xml_find_all("//latitude") %>% xml_text,
             lon =          bus_gps %>% xml_find_all("//longitude") %>% xml_text,
             route_id =     bus_gps %>% xml_find_all("//routeid") %>% xml_text,
             direction_id = bus_gps %>% xml_find_all("//direction") %>% xml_text,
             stringsAsFactors = FALSE) %>%
  filter(!route_id %in% c("99", "0")) %>%
  mutate(direction_id = ifelse(direction_id == "O", 0, 1))

# map out stop_sequences
stops_leaflet <- pm_dd$all_stop_sequences %>% filter(route_id == "60", direction_id == 0)
gps_leaflet <- current_gps %>% filter(route_id == "60", direction_id == 0)
leaflet(stops_leaflet) %>% addTiles() %>% setView(-149.8, 61.18, zoom = 10) %>%
  addCircles(lat = ~stop_lat, lng = ~stop_lon, popup = ~as.character(stop_sequence)) %>%
  addPolylines(lat = ~stop_lat, lng = ~stop_lon) %>%
  addMarkers(data = gps_leaflet, lat = ~lat, lng = ~lon)

n_current_route_trips <- current_gps %>%
  group_by(route_id, direction_id) %>%
  summarize(n_trips = length(route_id))

existing_trips <- inner_join(n_current_route_trips, old_trips, by = c("route_id", "direction_id")) %>% 
  group_by(route_id, direction_id) %>%
  mutate(trip_order = rank(departure_time)) %>%
  filter(trip_order >= max(trip_order) - max(n_trips)) %>%
  select(-n_trips, trip_id)

all_possible_trips <- full_join(current_gps, existing_trips, by = c("route_id", "direction_id")) %>%
  select(-route_id, -direction_id, -departure_time)

all_possible_stop_times <- 
  pm_dd$today_stop_times %>% filter(trip_id %in% all_possible_trips$trip_id) %>%
  inner_join(pm$stops_df, "stop_id") %>% select(trip_id, stop_id, stop_sequence, stop_lat, stop_lon, departure_time)

ordered_trip_matches <- 
  all_possible_trips %>% 
    full_join(all_possible_stop_times, by = "trip_id") %>%
    mutate(dist = distVincentyEllipsoid(p1 = matrix(c(stop_lon, stop_lat), ncol=2),
                                        p2 = matrix(as.numeric(c(lon, lat)), ncol = 2))) %>% # get distance from route_stops and bus locations.
    group_by(trip_id, lat, lon)  %>%
    mutate(dist_order = order(dist)) %>%
    filter(dist_order <= 2) %>%
    select(lat, lon, trip_id, stop_sequence, dist, dist_order, trip_order, departure_time) 
  
invalidated_distances <- 
  ordered_trip_matches %>%
    filter(max(stop_sequence) - min(stop_sequence) != 1) %>%
    filter(stop_sequence == max(stop_sequence))

validated_distances <- 
  ordered_trip_matches %>%
    filter(max(stop_sequence) - min(stop_sequence) == 1) 

validated_trips <- data.frame(trip_id = character(), stop_sequence = integer(), departure_time = character())
tmp_validated_trips <- data.frame()
for( i in seq(max(just_the_max_stop$trip_order))) {
  tmp_validated_trips <- 
    validated_distances %>% ungroup %>% 
    filter(!trip_id %in% validated_trips$trip_id) %>% group_by(lat, lon) %>%
    filter(dist_order == i, trip_order == max(trip_order) - i + 1) %>% 
    group_by(trip_id, lat, lon) %>%
    filter(stop_sequence == max(stop_sequence)) %>%
    filter(row_number() == 1) %>% ungroup() %>% 
    select(trip_id, stop_sequence, departure_time)
  validated_trips <- rbind(validated_trips, tmp_validated_trips)
}

validated_trips <- validated_trips %>% group_by(trip_id) %>% 
  filter(stop_sequence == max(stop_sequence))
  
  

  filter(stop_sequence == sort(stop_sequence, partial = length(stop_sequence) - 0)[length(stop_sequence) - 0]) 
  
  filter(stop_sequence == max(stop_sequence)) %>%
  select(trip_id, stop_sequence, departure_time) %>%
    


  
  
  
  
  
  
# Routes with the different stop sequences are going to be tricky.
# I'm not sure how to seperate the routes by the location of the bus
# (7a/7j for example)
all_data <- data.frame()
for(i in seq(max(current_trips$trip_order_for_current_buses)))  {
  
  mth_recent_trips <-
    current_trips %>% filter(trip_order_for_current_buses == i) %>%
    select(-trip_order_for_current_buses)
  
  mth_trip_preprotobuf <-
    today_stop_times %>%
    inner_join(mth_recent_trips, by = "trip_id") %>% # show stop_times for current trips w/stop id.
    inner_join(pm$stops_df, by = "stop_id") %>% # add lat/lon of stops
    select(stop_id, route_id, direction_id, stop_lat,
           stop_lon, trip_id,  stop_sequence) %>% # remove meta data
    inner_join(current_gps,
               by = c("route_id", "direction_id")) %>% # join bus location for route and direction
    mutate(dist = distVincentyEllipsoid(p1 = matrix(c(stop_lon, stop_lat), ncol=2),
                                        p2 = matrix(as.numeric(c(lon, lat)), ncol = 2))) %>% # get distance from route_stops and bus locations.
    group_by(trip_id) %>%
    arrange(desc(dist)) %>% top_n(-2) %>% # filter for two closest stops.
    filter(stop_sequence == max(stop_sequence)) %>% # take the larger stop_sequence.
    select(stop_id, stop_sequence, trip_id) %>% # remove used columns.
    arrange(trip_id) %>%
    inner_join(today_stop_times,
               by = c("stop_sequence", "stop_id", "trip_id")) %>% # add the times for each stop_sequence
    mutate(time_diff = ymd_hms(Sys.time()) - ymd_hms(paste(today, departure_time))) %>% # subtract them to find the duration until the next stop.
    select(trip_id, time_diff, stop_id, stop_sequence) %>%
    inner_join(todays_trip_departures, by = "trip_id") %>%
    select(trip_id, time_diff, stop_sequence)
  
  all_data <- bind_rows(all_data, mth_trip_preprotobuf)
  
}

leaflet() %>% addTiles() %>% setView(-149.8, 61.18, zoom = 10) %>%
  addCircles(data = pm$stops_df, lat = ~stop_lat, lng = ~stop_lon, color = "red") %>%
  