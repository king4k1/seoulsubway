# shortestpath

shortestpath <- function(depart, depart_line, arrival, arrival_line) {
  Zero <- tryCatch(shortestpath_0(depart = depart, depart_line = depart_line, 
                                  arrival = arrival, arrival_line = arrival_line), warning = function(w) {
                                    Zero = list(Time = 300)
                                  }, error = function(e) {
                                    Zero = list(Time = 300)
                                  })
  One <- tryCatch(shortestpath_1(depart = depart, depart_line = depart_line, 
                                 arrival = arrival, arrival_line = arrival_line), warning = function(w) {
                                   One = list(Time = 300)
                                 }, error = function(e) {
                                   One = list(Time = 300)
                                 })
  Two <- tryCatch(shortestpath_2(depart = depart, depart_line = depart_line, 
                                 arrival = arrival, arrival_line = arrival_line), warning = function(w) {
                                   Two = list(Time = 300)
                                 }, error = function(e) {
                                   Two = list(Time = 300)
                                 })
  # use tryCatch() for consider error case get results of Three case, and
  # select shortest path
  Total <- list(Zero, One, Two)
  Short_Path_Ind <- which.min(c(Total[[1]]$Time, Total[[2]]$Time, Total[[3]]$Time))
  return(Total[[Short_Path_Ind]])
}

