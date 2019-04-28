shortestpath_plot <- function(depart, arrival, google_key, zoom =zoom) {
  register_google(key = google_key)
  result <- shortestpath(depart, arrival)
  Info <- result$Info
  if (nrow(Info) == 1) {
    Path <- result$Path
    meanX <- mean(Path$lat)
    meanY <- mean(Path$long)
    g <- ggmap(get_map(c(meanY, meanX), zoom = zoom, maptype = 'toner-lines', source = "stamen"))
    g2 <- g + geom_path(data = Path , aes(x = long, y = lat, col = Line), size=3) +
      geom_point(data = Path , aes(x = long, y = lat), col="black", size=4) + 
      geom_label(data = Path%>% filter(!duplicated(Name)),
                 aes(x = long, y = lat, label = Name), 
                 size=4, vjust = 1.5, alpha = 0.5) 
  }
  if (nrow(Info) == 2) {
    Path <- rbind(result$Path1, result$Path2)
    meanX <- mean(Path$lat)
    meanY <- mean(Path$long)
    g <- ggmap(get_map(c(meanY, meanX), zoom = zoom, maptype = 'toner-lines', source = "stamen"))
    g2 <- g + geom_path(data = Path , aes(x = long, y = lat, col = Line), size=3) +
      geom_point(data = Path , aes(x = long, y = lat), col="black", size=4) + 
      geom_label(data = Path%>% filter(!duplicated(Name)),
                 aes(x = long, y = lat, label = Name),
                 size=4, vjust = 1.5, alpha = 0.5) 
    
  }
  if (nrow(Info) == 3) {
    Path <- rbind(result$Path1, result$Path2, result$Path3)
    meanX <- mean(Path$lat)
    meanY <- mean(Path$long)
    g <- ggmap(get_map(c(meanY, meanX), zoom = zoom, maptype = 'toner-lines', source = "stamen"))
    g2 <- g + geom_path(data = Path , aes(x = long, y = lat, col = Line), size=3) +
      geom_point(data = Path , aes(x = long, y = lat), col="black", size=4) + 
      geom_label(data = Path%>% filter(!duplicated(Name)),
                 aes(x = long, y = lat, label = Name),
                 size=4, vjust = 1.5, alpha = 0.5) 
  }
  return(g2)
}