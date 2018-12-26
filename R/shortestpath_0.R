# shortestpath_0

shortestpath_0 <- function(depart, depart_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  Start_Ind_0 <- which(subway_data[[depart_line]]$Name == depart)
  End_Ind_0 <- which(subway_data[[depart_line]]$Name == arrival)
  Total_Depart_Raw <- nrow(subway_data[[depart_line]])
  if (depart_line == arrival_line) {
    Path_Info <- get_pathinfo(total = Total_Depart_Raw, ind1 = Start_Ind_0, 
                              ind2 = End_Ind_0, line = depart_line)
    Path_Count <- as.numeric(Path_Info["count"])
    Path_Time <- as.numeric(Path_Info["time"])
    Transfer_0 <- data.frame(Depart = depart, Line = depart_line, Count = Path_Count, 
                             Time = Path_Time, Arrive = arrival)
  }
  Set <- list(Info = Transfer_0, Count = as.numeric(Transfer_0[1, "Count"]), 
              Time = as.numeric(Transfer_0[1, "Time"]))
  Set$Path <- subway_data[[depart_line]][Start_Ind_0:End_Ind_0, ]
  if (isTRUE(depart_line == 2) & isTRUE(as.numeric(Transfer_0[3]) == 
                                        (Total_Depart_Raw - Start_Ind_0 + End_Ind_0))) {
    Set$Path <- subway_data[["2"]][c(Start_Ind_0:Total_Depart_Raw, 
                                             1:End_Ind_0), ]
  } else if (isTRUE(depart_line == 2) & isTRUE(as.numeric(Transfer_0[3]) == 
                                               (Total_Depart_Raw - End_Ind_0 + Start_Ind_0))) {
    Set$Path <- subway_data[["2"]][c(End_Ind_0:Total_Depart_Raw, 
                                             1:Start_Ind_0), ]
  }
  if (isTRUE(depart_line == "6-A") & Start_Ind_0 > End_Ind_0) {
    Set$Path <- subway_data[["6-A"]][c(Start_Ind_0 :Total_Depart_Raw, 
                                             1:End_Ind_0), ]
  }
  return(Set)
}
