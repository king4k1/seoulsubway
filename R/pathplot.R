# pathplot

# shortestpath 경로 지도 상 표현

# naver, 카카오 key 필요

pathplot <- function(depart, depart_line, arrival, arrival_line, naver_secret, naver_key, kakao_key, zoom =zoom) {
  Info <- shortestpath(depart, depart_line, arrival, arrival_line)$Info
  if (nrow(Info) == 1) {
    result <- shortestpath(depart, depart_line, arrival, arrival_line)
    Path <- result$Path
    meanX <- mean(Path$lat)
    meanY <- mean(Path$long)
    g <- ggmap(get_kakao_navermap(center = c(lon = meanY, lat = meanX), 
                                  zoom = zoom, naver_key = "YvhIigePOgOCr1rKLcyT", url_2 = "http://naver.com", 
                                  keyword = F, address = F, kakao_key = "bd0f02bafa236ced3eebb2d12845a306", 
                                  naver_secret = "uP1fgrNQID", overlayers = "anno_satellite"))
    g2 <- g + geom_point(data = Path, aes(x = long, y = lat, col = Line)) + 
      geom_path(data = Path, aes(x = long, y = lat, col = Line)) + 
      scale_colour_manual(values = c("red"))
  }
  
  if (nrow(Info) == 2) {
    result <- shortestpath(depart, depart_line, arrival, arrival_line)
    Path <- rbind(result$Path1, result$Path2)
    meanX <- mean(Path$lat)
    meanY <- mean(Path$long)
    
    g <- ggmap(get_kakao_navermap(center = c(lon = meanY, lat = meanX), 
                                  zoom = zoom, naver_key = "YvhIigePOgOCr1rKLcyT", url_2 = "http://naver.com", 
                                  keyword = F, address = F, kakao_key = "bd0f02bafa236ced3eebb2d12845a306", 
                                  naver_secret = "uP1fgrNQID", overlayers = "anno_satellite"))    
    g2 <- g + geom_point(data = Path, aes(x = long, y = lat, col = Line)) + 
      geom_path(data = Path, aes(x = long, y = lat, col = Line)) + 
      scale_colour_manual(values = c("red", "green"))
    
  }
  if (nrow(Info) == 3) {
    result <- shortestpath(depart, depart_line, arrival, arrival_line)
    Path <- rbind(result$Path1, result$Path2, result$Path3)
    meanX <- mean(Path$lat)
    meanY <- mean(Path$long)
    
    g <- ggmap(get_kakao_navermap(center = c(lon = meanY, lat = meanX), 
                                  zoom = zoom, naver_key = "YvhIigePOgOCr1rKLcyT", url_2 = "http://naver.com", 
                                  keyword = F, address = F, kakao_key = "bd0f02bafa236ced3eebb2d12845a306", 
                                  naver_secret = "uP1fgrNQID", overlayers = "anno_satellite"))    
    g2 <- g + geom_point(data = Path, aes(x = long, y = lat, col = Line)) + 
      geom_path(data = Path, aes(x = long, y = lat, col = Line)) + 
      scale_color_manual(values = c("red", "blue", "green"))
  }
  return(g2)
}

