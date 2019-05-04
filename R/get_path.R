# get_path from shortestpath

get_path <- function(depart, arrival){
  result <- shortestpath(depart, arrival)
  station <- "no_result"
  index <- result %>% names %>% str_which("Path")
  if(result$Time!=300){
    for(i in 2:length(index)){
      Path <- rbind(Path, result[[index[i]]])
    }
    station <- Path$Name %>% unique()
  }
  n_row <- length(station)
  station_1 <- station[1:(n_row-1)]
  station_2 <- station[2:n_row]
  return(data.frame(from=station_1, to=station_2))
}
