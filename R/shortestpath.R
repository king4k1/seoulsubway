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
                                 arrival = arrival, arrival_line = arrival_line), error = function(e) {
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
                                       message("If the station and the line do not match, the route can not be obtained.
also you should consider branch line '1_A','1_B','2_A','2_B','5_A','6_A' and 'K_A'
* Geumcheon-gu Office to Kwang Myung Station : '1_A' 
* Byeongjeom to Seodongtan Station : '1_B'
* Seongsu to Sinseol-dong Station : '2_A' \n* Sindorim to Kkachisan Station : '2_B'
* Gangdong to Macheon Station : '5_A' \n* Eungam to Gusan(also include Eungam) Station : '6_A'
* Gajwa to Seoul Station : 'K_A'")
                                     })
  }
  return(Total)
}

