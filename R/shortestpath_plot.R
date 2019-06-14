shortestpath_plot <- function (departure, arrival, google_key, zoom = zoom){
  register_google(key = google_key)
  result <- shortestpath(departure, arrival, max_route=1)[[1]]
  Info <- result$Info
  index <- result %>% names %>% str_which("Path")
  Path <- result[[index[1]]]
  if(length(index)>1){
    for(i in 2:length(index)){
      Path <- rbind(Path, result[[index[i]]])
    }
  }
  meanX <- mean(as.numeric(Path$long))
  meanY <- mean(as.numeric(Path$lat))
  g <- ggmap(get_map(c(meanX, meanY), zoom = zoom,
                     maptype = "toner-lite", source = "stamen")) + 
    geom_path(data = Path, aes(x = as.numeric(long), 
                               y = as.numeric(lat), col = Line)) +
    geom_label(data = Path %>% filter(!duplicated(Name)), 
               aes(x = as.numeric(long), 
                   y = as.numeric(lat), label = Name),
               fill = "black", col = "white", alpha = 0.5)
  return(g)
}
