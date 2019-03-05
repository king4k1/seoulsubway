# get_path from shortestpath

get_path <- function(depart, arrival){
  data("seoul_station", envir = environment())
  result <- shortestpath(depart, arrival)
  
  if(result$Time==300){
    station <- "no result"
    ind <- 0
    result$Info <- data.frame(matrix(0,5,5))
  }
  if(result$Time!=300 & nrow(result$Info)==1){
    station <- result$Path
    station <- station$Name
  }
  if(result$Time!=300 & nrow(result$Info)==2){
    station <- rbind(result$Path1, result$Path2)
    station <- station[-which(duplicated(station$Name)),]$Name
  }
  if(result$Time!=300 & nrow(result$Info)==3){
    station <- rbind(result$Path1, result$Path2, result$Path3)
    station <- station[-which(duplicated(station$Name)),]$Name
  }
  if(result$Time!=300 & nrow(result$Info)==4){
    station <- rbind(result$Path1, result$Path2, result$Path3, result$Path4)
    station <- station[-which(duplicated(station$Name)),]$Name
  }
  if(result$Time!=300 & nrow(result$Info)==5){
    station <- rbind(result$Path1, result$Path2, 
                     result$Path3, result$Path4, result$Path5)
    station <- station[-which(duplicated(station$Name)),]$Name
  }  
  return(data.frame(station))
}
