shortestpath_plot <- function(depart, arrival, google_key, zoom =zoom) {
  register_google(key = google_key)
  result <- shortestpath(depart, arrival)
  Info <- result$Info
  if (nrow(Info) == 1) {
    Path <- result$Path
    meanX <- mean(as.numeric(Path$long))
    meanY <- mean(as.numeric(Path$lat))
    g <- ggmap(get_map(c(meanX, meanY), zoom = zoom, maptype = 'toner-lite', source = "stamen"))
    g2 <- g + geom_path(data = Path, aes(x = as.numeric(long),
                                         y = as.numeric(lat), col = Line), size = 3) + 
      geom_label(data = Path %>% filter(!duplicated(Name)), 
                 aes(x = as.numeric(long), y = as.numeric(lat), label = Name), 
                 fill="black", col="white", alpha = 0.5)
  }
  if (nrow(Info) == 2) {
    Path <- rbind(result$Path1, result$Path2)
    meanX <- mean(as.numeric(Path$long))
    meanY <- mean(as.numeric(Path$lat))
    g <- ggmap(get_map(c(meanX, meanY), zoom = zoom, maptype = 'toner-lite', source = "stamen"))
    g2 <- g + geom_path(data = Path, aes(x = as.numeric(long),
                                         y = as.numeric(lat), col = Line), size = 3) + 
      geom_label(data = Path %>% filter(!duplicated(Name)), 
                 aes(x = as.numeric(long), y = as.numeric(lat), label = Name), 
                 fill="black", col="white", alpha = 0.5)
  }
  if (nrow(Info) == 3) {
    Path <- rbind(result$Path1, result$Path2, result$Path3)
    meanX <- mean(as.numeric(Path$long))
    meanY <- mean(as.numeric(Path$lat))
    g <- ggmap(get_map(c(meanX, meanY), zoom = zoom, maptype = 'toner-lite', source = "stamen"))
    g2 <- g + geom_path(data = Path, aes(x = as.numeric(long), 
                                         y = as.numeric(lat), col = Line), size = 3) + 
      geom_label(data = Path %>% filter(!duplicated(Name)), 
                 aes(x = as.numeric(long), y = as.numeric(lat), label = Name), 
                fill="black", col="white", alpha = 0.5)
  }
  if (nrow(Info) == 4) {
    Path <- rbind(result$Path1, result$Path2, result$Path3, result$Path4)
    meanX <- mean(as.numeric(Path$long))
    meanY <- mean(as.numeric(Path$lat))
    g <- ggmap(get_map(c(meanX, meanY), zoom = zoom, maptype = 'toner-lite', source = "stamen"))
    g2 <- g + geom_path(data = Path, aes(x = as.numeric(long),
                                         y = as.numeric(lat), col = Line), size = 3) + 
      geom_label(data = Path %>% filter(!duplicated(Name)), 
                 aes(x = as.numeric(long), y = as.numeric(lat), label = Name), 
                 fill="black", col="white", alpha = 0.5)
  }
  return(g2)
}

