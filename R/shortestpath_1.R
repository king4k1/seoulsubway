# shortestpath_1


shortestpath_1 <- function(depart, depart_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  Transfer_list <- get_transferinfo(depart, depart_line, arrival, arrival_line, 
                                    count = 1)
  # get available transfer station list if Transfer_list has no result
  # get break; cannot get result from shortestpath_1
  Total_Depart_Raw <- nrow(subway_data[[depart_line]])
  Total_Transfer_Raw <- nrow(subway_data[[arrival_line]])
  Transfer_1 <- list()
  # first setting for loops
  Start_Ind_1 <- which(subway_data[[depart_line]]$Name == depart)
  End_Ind_1 <- c()
  for (i in 1:nrow(Transfer_list)) {
    End_Ind_1[i] <- which(subway_data[[depart_line]]$Name == Transfer_list$Name[i])
    Path1_Info <- get_pathinfo(total = Total_Depart_Raw, ind1 = Start_Ind_1, 
                               ind2 = End_Ind_1[i], line = depart_line)
    # get time / count information from get_pathinfo() and manufacture one.
    Path1_Count <- as.numeric(Path1_Info["count"])
    Path1_Time <- as.numeric(Path1_Info["time"])
    Start_Ind2_1 <- which(subway_data[[arrival_line]]$Name == Transfer_list[i, 
                                                                            "Name"])
    End_Ind2_1 <- which(subway_data[[arrival_line]]$Name == arrival)
    Path2_Info <- get_pathinfo(total = Total_Transfer_Raw, ind1 = Start_Ind2_1, 
                               ind2 = End_Ind2_1, line = arrival_line)
    Path2_Count <- as.numeric(Path2_Info["count"])
    Path2_Time <- as.numeric(Path2_Info["time"])
    # get transfer time from transfer_info data set; no result can get
    # 3(average spend time for transfer)
    Transfer_Time_List <- transfer_info[which(transfer_info$Transfer_Name == 
                                                Transfer_list[i, "Name"]), ]
    Transfer_Time_T <- Transfer_Time_List[which(Transfer_Time_List$Transfer_Line == 
                                                  depart_line), ]
    Transfer_Time <- Transfer_Time_T[which(Transfer_Time_T$Transfer_Line2 == 
                                             arrival_line), ]
    if (nrow(Transfer_Time) >= 1) {
      Transfer_Time <- as.numeric(Transfer_Time$Transfer_Time)
    } else {
      Transfer_Time <- 2.35
    }
    Transfer_1[[i]] <- list(Info = data.frame(Depart = c(depart, Transfer_list[i, 
                                                                               "Name"]), Line = c(depart_line, arrival_line), Count = c(as.character(Path1_Count), 
                                                                                                                                        as.character(Path2_Count)), Time = c(as.character(Path1_Time), 
                                                                                                                                                                             as.character(Path2_Time)), Arrive = c(Transfer_list[i, "Name"], 
                                                                                                                                                                                                                   arrival)), Total = c(Count = Path1_Count + Path2_Count, Time = Path1_Time + 
                                                                                                                                                                                                                                          Path2_Time + Transfer_Time + 3))
  }
  Transfer_1_Count_Time <- c()
  for (i in seq_along(Transfer_1)) {
    Transfer_1_Count_Time[i] <- as.numeric(Transfer_1[[i]]["Total"][[1]]["Time"])
  }
  Path_1_Shortest <- which.min(Transfer_1_Count_Time)
  Transfer_1 <- Transfer_1[[Path_1_Shortest]]
  Transfer_Name_1 <- as.character(Transfer_1["Info"][[1]][1, "Arrive"])
  # get shortestpath(time depend)
  Set <- list(Info = Transfer_1$Info, Count = as.numeric(Transfer_1["Total"][[1]]["Count"]), 
              Time = as.numeric(Transfer_1["Total"][[1]]["Time"]))
  Start_Ind_1 <- which(subway_data[[depart_line]]$Name == depart)
  End_Ind_1 <- which(subway_data[[depart_line]]$Name == Transfer_Name_1)
  Start_Ind2_1 <- which(subway_data[[arrival_line]]$Name == Transfer_Name_1)
  End_Ind2_1 <- which(subway_data[[arrival_line]]$Name == arrival)
  Set$Path1 <- subway_data[[depart_line]][Start_Ind_1:End_Ind_1, ]
  Set$Path2 <- subway_data[[arrival_line]][Start_Ind2_1:End_Ind2_1, ]
  # default setting for normal case;
  if (isTRUE(depart_line == 2) & isTRUE(Set["Info"][[1]][1, "Count"] == 
                                        (Total_Depart_Raw - Start_Ind_1 + End_Ind_1))) {
    Set$Path1 <- subway_data[["2"]][c(Start_Ind_1:Total_Depart_Raw, 
                                      1:End_Ind_1), ]
  } else if (isTRUE(depart_line == 2) & isTRUE(Set["Info"][[1]][1, "Count"] == 
                                               (Total_Depart_Raw - End_Ind_1 + Start_Ind_1))) {
    Set$Path1 <- subway_data[["2"]][c(End_Ind_1:Total_Depart_Raw, 1:Start_Ind_1), 
                                    ]
  }
  if (isTRUE(arrival_line == 2) & isTRUE(Set["Info"][[1]][2, "Count"] == 
                                         (Total_Transfer_Raw - Start_Ind2_1 + End_Ind2_1))) {
    Set$Path2 <- subway_data[["2"]][c(Start_Ind2_1:Total_Transfer_Raw, 
                                      1:End_Ind2_1), ]
  } else if (isTRUE(arrival_line == 2) & isTRUE(Set["Info"][[1]][2, "Count"] == 
                                                (Total_Transfer_Raw - End_Ind2_1 + Start_Ind2_1))) {
    Set$Path2 <- subway_data[["2"]][c(End_Ind2_1:Total_Transfer_Raw, 
                                      1:Start_Ind2_1), ]
  }
  # consider line number 2 for circulate system.
  return(Set)
}