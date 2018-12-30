# shortestpath_0

shortestpath_0 <- function(depart, depart_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  Start_Ind_0 <- which(subway_data[[depart_line]]$Name == depart)
  End_Ind_0 <- which(subway_data[[depart_line]]$Name == arrival)
  Total_Depart_Raw <- nrow(subway_data[[depart_line]])
  if (depart_line == arrival_line) {
    Path_Info <- get_pathinfo(total = Total_Depart_Raw, start = Start_Ind_0, 
                              end = End_Ind_0, line = depart_line)
    Path_Count <- as.numeric(Path_Info["count"])
    Path_Time <- as.numeric(Path_Info["time"])
    Transfer_0 <- list(Info = data.frame(Depart = depart, Line = depart_line, Count = Path_Count, 
                                         Time = Path_Time, Arrive = arrival),
                       Total = c(Count = Path_Count, Time = Path_Time))
  }
  Set <- get_pathresult(Transfer_0)
  return(Set)
}
