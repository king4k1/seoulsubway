# shortestpath

shortestpath <- function(depart, depart_line, arrival, arrival_line) {
  if(depart_line == arrival_line){
  Zero <- shortestpath_0(depart = depart, depart_line = depart_line, 
                                  arrival = arrival, arrival_line = arrival_line)
  Total <- list(Zero)
  Short_Path_Ind <- 1
  }
  if(depart_line != arrival_line){
  One <- tryCatch(shortestpath_1(depart = depart, depart_line = depart_line, 
                                 arrival = arrival, arrival_line = arrival_line), warning = function(w) {
                                   One = list(Time = 300)
                                 }, error = function(e) {
                                   One = list(Time = 300)
                                 })
  Two <- tryCatch(shortestpath_2(depart = depart, depart_line = depart_line, 
                                 arrival = arrival, arrival_line = arrival_line), error = function(e) {
                                   Two = list(Time = 300)
                                 })
  # use tryCatch() for consider error case get results of Three case, and
  # select shortest path
  Total <- list(One, Two)
  Short_Path_Ind <- which.min(c(Total[[1]]$Time, Total[[2]]$Time))
  }
  # if shortest path(by two transfer) is null -> consider three transfer
  Total <- Total[[Short_Path_Ind]]
  
  if(Total$Time == 300){
    Total <- tryCatch(shortestpath_3(depart = depart, depart_line = depart_line, 
                                     arrival = arrival, arrival_line = arrival_line), error = function(e) {
                                       Total = list(Time = 300)
                                     })
  }
  return(Total)
}

