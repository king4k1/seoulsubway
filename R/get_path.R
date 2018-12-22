# get_path from shortestpath

get_path <- function(depart, depart_line, arrival, arrival_line){
  result <- shortestpath(depart, depart_line, arrival, arrival_line)
  
  if(result$Time==300){
    Path <- "no result"
    ind <- 0
    result$Info <- data.frame(matrix(0,5,5))
  }
  if(result$Time!=300 & nrow(result$Info)==1){
    Path <- result$Path
    Path <- Path$Name
    ind <- which(seoul_station%in%Path)
  }
  if(result$Time!=300 & nrow(result$Info)==2){
    Path <- rbind(result$Path1, result$Path2)
    Path <- Path[-which(duplicated(Path$Name)),]$Name
    ind <- which(seoul_station%in%Path)
  }
  if(result$Time!=300 & nrow(result$Info)==3){
    Path <- rbind(result$Path1, result$Path2, result$Path3)
    Path <- Path[-which(duplicated(Path$Name)),]$Name
    ind <- which(seoul_station%in%Path)
  }
  if(result$Time!=300 & nrow(result$Info)==4){
    Path <- rbind(result$Path1, result$Path2, result$Path3, result$Path4)
    Path <- Path[-which(duplicated(Path$Name)),]$Name
    ind <- which(seoul_station%in%Path)
  }
  return(data.frame(Path, ind))
}
