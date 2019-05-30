# get_path from shortestpath

get_path <- function(departure, arrival){
  result <- shortestpath_G(departure, arrival)
  station <- "no_result"
  index <- result %>% names %>% str_which("Path")
  if(!(result$Time %in% c(300,600))){
    Path <- result[[index[1]]]
    for(i in 2:length(index)){
      Path <- rbind(Path, result[[index[i]]])
    }
    station <- Path$Name %>% unique()
  }
  n_row <- length(station)
  station_1 <- station[1:(n_row-1)]
  station_2 <- station[2:n_row]
  result_set <- data.frame(set=paste0(departure, "-", arrival),
                           from=station_1, to=station_2)
  return(result_set %>% na.omit)
}