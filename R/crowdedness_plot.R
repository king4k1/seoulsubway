crowdedness_plot <- function(input_data, center, zoom, google_key){
  register_google(key = google_key)
  from_set <- input_data %>%
    mutate(set = paste0(from, "-", to)) %>% 
    dplyr::select(date, time, set, from, from_long, from_lat, sum_count)
  from_set$key <- "from"
  to_set <- input_data %>%
    mutate(set = paste0(from, "-", to)) %>% 
    dplyr::select(date, time, set, to, to_long, to_lat, sum_count)
  to_set$key <- "to"  
  colnames(from_set) <- colnames(to_set) <- c("date", "time", "set", "station",
                                              "long", "lat", "sum_count", "key")
  from_to_path_set <- rbind(from_set, to_set) %>% 
    mutate(long = as.numeric(long), lat=as.numeric(lat))
  congestion_plot_result <- 
    ggmap(get_map(center,zoom = zoom, maptype='roadmap', color="bw")) + 
    geom_path(data = from_to_path_set,
              aes(x=long, y=lat, group=set, col = sum_count),
              size = 1.2, alpha = 0.8, 
              lineend = "round") +
    scale_color_gradient(low="yellow", high="red")
  congestion_plot_result
}