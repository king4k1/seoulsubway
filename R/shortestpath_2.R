# shortestpath_2

shortestpath_2 <- function(depart, depart_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  # load data
  Total_Depart_Raw <- nrow(subway_data[[depart_line]])
  Total_End_raw <- nrow(subway_data[[arrival_line]])
  Start_Ind_2 <- which(subway_data[[depart_line]]$Name == depart)
  Transfer_List <- get_transferinfo(depart, depart_line, arrival, arrival_line, 
                                    count = 2)
  if(length(Transfer_List)==0){
    stop("you can`t get a path from these transfer count number 2")
  }
  Transfer_2 <- list()
  # get available transfer station list these results are list format and
  # get first/second transfer station
  for (i in seq_along(Transfer_List)) {
    Transfer_First <- Transfer_List[[i]]$first
    Transfer_Second <- Transfer_List[[i]]$second
    End_Ind_2 <- which(subway_data[[depart_line]]$Name == Transfer_First$Name)
    Transfer_First_Ind <- str_split(Transfer_First$Transfer, paste0("[", 
                                                                    "$", "|", "]"))[[1]]
    Transfer_Second_Ind <- str_split(Transfer_Second$Transfer, paste0("[", 
                                                                      "$", "|", "]"))[[1]]
    # process variable for get transfer line
    Transfer_First_Line <- Transfer_Second_Ind[which(Transfer_Second_Ind%in%Transfer_First_Ind)][1]
    Transfer_Second_Line <- arrival_line
    Total_Transfer_Raw <- nrow(subway_data[[Transfer_First_Line]])
    Start_Ind2_2 <- which(subway_data[[Transfer_First_Line]]$Name == Transfer_First$Name)
    End_Ind2_2 <- which(subway_data[[Transfer_First_Line]]$Name == Transfer_Second$Name)
    Start_Ind3_2 <- which(subway_data[[arrival_line]]$Name == Transfer_Second$Name)
    End_Ind3_2 <- which(subway_data[[arrival_line]]$Name == arrival)
    # get information about each path way
    Path1_Info <- get_pathinfo(total = Total_Depart_Raw, start = Start_Ind_2, 
                               end = End_Ind_2, line = depart_line)
    Path1_Count <- as.numeric(Path1_Info["count"])
    Path1_Time <- as.numeric(Path1_Info["time"])
    Path2_Info <- get_pathinfo(total = Total_Transfer_Raw, start = Start_Ind2_2, 
                               end = End_Ind2_2, line = Transfer_First_Line)
    Path2_Count <- as.numeric(Path2_Info["count"])
    Path2_Time <- as.numeric(Path2_Info["time"])
    Path3_Info <- get_pathinfo(total = Total_End_raw, start = Start_Ind3_2, 
                               end = End_Ind3_2, line = arrival_line)
    Path3_Count <- as.numeric(Path3_Info["count"])
    Path3_Time <- as.numeric(Path3_Info["time"])
    # get transfer time{ex) depart -> transfer} each
    Transfer_Time1_List <- transfer_info[which(transfer_info$Transfer_Name == 
                                               Transfer_First$Name), ]
    Transfer_Time1_T <- Transfer_Time1_List[which(Transfer_Time1_List$Transfer_Line == 
                                                  depart_line), ]
    Transfer_Time1 <- Transfer_Time1_T[which(Transfer_Time1_T$Transfer_Line2 == 
                                               Transfer_First_Line), ]
    Transfer_Time2_List <- transfer_info[which(transfer_info$Transfer_Name == 
                                               Transfer_Second$Name), ]
    Transfer_Time2_T <- Transfer_Time2_List[which(Transfer_Time2_List$Transfer_Line == 
                                                  arrival_line), ]
    Transfer_Time2 <- Transfer_Time2_T[which(Transfer_Time2_T$Transfer_Line2 == 
                                             arrival_line), ]
      if (nrow(Transfer_Time1) >= 1) {
        Transfer_Time1 <- as.numeric(Transfer_Time1$Transfer_Time)
      } else {
        Transfer_Time1 <- 2.35
      }
      # if no results about transfer_info data -> print 2.35(citation)
      if (nrow(Transfer_Time2) >= 1) {
        Transfer_Time2 <- as.numeric(Transfer_Time2$Transfer_Time)
      } else {
        Transfer_Time2 <- 2.35
      }
      Transfer_2[[i]] <- list(Info = data.frame(Depart = c(depart, Transfer_First$Name, Transfer_Second$Name),
                                                                Line = c(depart_line, Transfer_First_Line, arrival_line), 
                                                                Count = c(as.numeric(Path1_Count), as.numeric(Path2_Count),
                                                                          as.numeric(Path3_Count)), 
                                                                Time = c(as.numeric(Path1_Time), as.numeric(Path2_Time), as.numeric(Path3_Time)),
                                                                Arrive = c(Transfer_First$Name, Transfer_Second$Name, arrival)),
                                              Total = c(Count = sum(Path1_Count + Path2_Count + Path3_Count), 
                                                        Time = sum(Path1_Time + Path2_Time + Path3_Time) + Transfer_Time1 + Transfer_Time2 + 6))
                                                                                                                                                                                                                                                                                                                                                                     
    }
  Transfer_2_Count_Time <- c()
  for (i in seq_along(Transfer_2)) {
    Transfer_2_Count_Time[i] <- as.numeric(Transfer_2[[i]]["Total"][[1]]["Time"])
  }
  Path_2_Shortest <- which.min(Transfer_2_Count_Time)
  # select shortest path(time depend)
  Transfer_2 <- Transfer_2[[Path_2_Shortest]]
  Set <- list(Info = Transfer_2$Info, Count = as.numeric(Transfer_2["Total"][[1]]["Count"]), 
              Time = as.numeric(Transfer_2["Total"][[1]]["Time"]))
  End_Ind_2 <- which(subway_data[[depart_line]]$Name == as.character(Set["Info"][[1]][1, 
                                                                                      "Arrive"]))
  Start_Ind2_2 <- which(subway_data[[as.character(Set["Info"][[1]][2, 
                                                                   "Line"])]]$Name == as.character(Set["Info"][[1]][2, "Depart"]))
  End_Ind2_2 <- which(subway_data[[as.character(Set["Info"][[1]][2, "Line"])]]$Name == 
                        as.character(Set["Info"][[1]][2, "Arrive"]))
  Start_Ind3_2 <- which(subway_data[[arrival_line]]$Name == as.character(Set["Info"][[1]][3, 
                                                                                          "Depart"]))
  Set$Path1 <- subway_data[[depart_line]][Start_Ind_2:End_Ind_2, ]
  Set$Path2 <- subway_data[[as.character(Set["Info"][[1]][2, "Line"])]][Start_Ind2_2:End_Ind2_2, 
                                                                        ]
  Set$Path3 <- subway_data[[arrival_line]][Start_Ind3_2:End_Ind3_2, ]
  if (isTRUE(depart_line == 2) & isTRUE(depart_line == (Total_Depart_Raw - 
                                                        Start_Ind_2 + End_Ind_2))) {
    Set$Path1 <- subway_data[["2"]][c(Start_Ind_2:Total_Depart_Raw, 
                                      1:End_Ind_2), ]
  } else if (isTRUE(depart_line == 2) & isTRUE(depart_line == (Total_Depart_Raw - 
                                                               End_Ind_2 + Start_Ind_2))) {
    Set$Path1 <- subway_data[["2"]][c(End_Ind_2:Total_Depart_Raw, 1:Start_Ind_2), 
                                    ]
  }
  if (isTRUE(Set["Info"][[1]][2, "Line"] == 2) & isTRUE(Set["Info"][[1]][2, 
                                                                         "Count"] == (Total_Transfer_Raw - Start_Ind2_2 + End_Ind2_2))) {
    Set$Path2 <- subway_data[["2"]][c(Start_Ind2_2:Total_Transfer_Raw, 
                                      1:End_Ind2_2), ]
  } else if (isTRUE(Set["Info"][[1]][2, "Line"] == 2) & isTRUE(Set["Info"][[1]][2, 
                                                                                "Count"] == (Total_Transfer_Raw - End_Ind2_2 + Start_Ind2_2))) {
    Set$Path2 <- subway_data[["2"]][c(End_Ind2_2:Total_Transfer_Raw, 
                                      1:Start_Ind2_2), ]
  }
  if (isTRUE(arrival_line == 2) & isTRUE(Set["Info"][[1]][3, "Count"] == 
                                         (Total_End_raw - Start_Ind3_2 + End_Ind3_2))) {
    Set$Path3 <- subway_data[["2"]][c(Start_Ind3_2:Total_End_raw, 1:End_Ind3_2), 
                                    ]
  } else if (isTRUE(arrival_line == 2) & isTRUE(Set["Info"][[1]][3, "Count"] == 
                                                (Total_End_raw - End_Ind3_2 + Start_Ind3_2))) {
    Set$Path3 <- subway_data[["2"]][c(End_Ind3_2:Total_End_raw, 1:Start_Ind3_2), 
                                    ]
  }
  if (isTRUE(depart_line == "6-A") & Start_Ind_2 > End_Ind_2) {
    Set$Path1 <- subway_data[["6-A"]][c(Start_Ind_2 :6, 
                                        1:End_Ind_2), ]
  }
  if (isTRUE(Set["Info"][[1]][2, "Line"] == "6-A") & Start_Ind2_2 > End_Ind2_2) {
    Set$Path2 <- subway_data[["6-A"]][c(Start_Ind2_2 :6, 
                                        1:End_Ind2_2), ]
  }
  if (isTRUE(arrival_line == "6-A") & Start_Ind3_2 > End_Ind3_2) {
    Set$Path3 <- subway_data[["6-A"]][c(Start_Ind3_2 :6, 
                                        1:End_Ind3_2), ]
  }
  # consider line number 2 & 6-A for circulate system.
  return(Set)
}
