# shortestpath_1


shortestpath_1 <- function(depart, depart_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  Transfer_List <- get_transferinfo(depart, depart_line, arrival, arrival_line, 
                                    count = 1)
  if(nrow(Transfer_List)==0){
    stop("you can`t get a path from these transfer count number 1 
       or station and line do not match")
  }
  # get available transfer station list
  Total_Depart_Raw <- nrow(subway_data[[depart_line]])
  Total_Transfer_Raw <- nrow(subway_data[[arrival_line]])
  Transfer_1 <- list()
  # first setting for loops
  Start_Ind_1 <- which(subway_data[[depart_line]]$Name == depart)
  End_Ind_1 <- c()
  for (i in 1:nrow(Transfer_List)) {
    End_Ind_1[i] <- which(subway_data[[depart_line]]$Name == Transfer_List$Name[i])
    Path1_Info <- get_pathinfo(total = Total_Depart_Raw, start = Start_Ind_1, 
                               end = End_Ind_1[i], line = depart_line)
    # get time / count information from get_pathinfo() and manufacture one.
    Path1_Count <- as.numeric(Path1_Info["count"])
    Path1_Time <- as.numeric(Path1_Info["time"])
    Start_Ind2_1 <- which(subway_data[[arrival_line]]$Name == Transfer_List[i, 
                                                                            "Name"])
    End_Ind2_1 <- which(subway_data[[arrival_line]]$Name == arrival)
    Path2_Info <- get_pathinfo(total = Total_Transfer_Raw, start = Start_Ind2_1, 
                               end = End_Ind2_1, line = arrival_line)
    Path2_Count <- as.numeric(Path2_Info["count"])
    Path2_Time <- as.numeric(Path2_Info["time"])
    # get transfer time from transfer_info data set; no result can get
    # 2.35(average spend time for transfer)
    Transfer_Time_List <- transfer_info[which(transfer_info$Transfer_Name == 
                                                Transfer_List[i, "Name"]), ]
    Transfer_Time_T <- Transfer_Time_List[which(Transfer_Time_List$Transfer_Line == 
                                                  depart_line), ]
    Transfer_Time <- Transfer_Time_T[which(Transfer_Time_T$Transfer_Line2 == 
                                             arrival_line), ]
    if (nrow(Transfer_Time) >= 1) {
      Transfer_Time <- as.numeric(Transfer_Time$Transfer_Time)
    } else {
      Transfer_Time <- 2.35
    }
    Transfer_1[[i]] <- list(Info = data.frame(Depart = c(depart, Transfer_List[i, "Name"]),
                                              Line = c(depart_line, arrival_line),
                                              Count = c(as.character(Path1_Count), as.character(Path2_Count)),
                                              Time = c(as.character(Path1_Time), as.character(Path2_Time)),
                                              Arrive = c(Transfer_List[i, "Name"], arrival)), 
                            Total = c(Count = Path1_Count + Path2_Count,
                                      Time = Path1_Time + Path2_Time + Transfer_Time + 3))
  }
  Transfer_1_Count_Time <- c()
  for (i in seq_along(Transfer_1)) {
    Transfer_1_Count_Time[i] <- as.numeric(Transfer_1[[i]]["Total"][[1]]["Time"])
  }
  Path_1_Shortest <- which.min(Transfer_1_Count_Time)
  Transfer_1 <- Transfer_1[[Path_1_Shortest]]
  # get shortestpath(time depend)
  Set <- get_pathresult(Transfer_1)
  # get_route / consider line number 2 & 6-A for circulate system.
  return(Set)
}
