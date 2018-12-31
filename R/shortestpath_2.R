# shortestpath_2

shortestpath_2 <- function(depart, depart_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  # load data
  Total_Depart_Raw <- nrow(subway_data[[depart_line]])
  Total_End_Raw <- nrow(subway_data[[arrival_line]])
  Start_Ind_2 <- which(subway_data[[depart_line]]$Name == depart)
  Transfer_List <- get_transferinfo(depart, depart_line, arrival, arrival_line, 
                                    n = 2)
  # get available transfer station list these results are list format and
  # include first/second transfer station.
  if(length(Transfer_List)==0){
    stop("you can`t get a path from these transfer count number 1,2,3 
or station and line do not match, also you should consider branch line(2-A, 2-B, 5-A, 6-A, K2)")
  }
  Transfer_2 <- list()
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
    Path3_Info <- get_pathinfo(total = Total_End_Raw, start = Start_Ind3_2, 
                               end = End_Ind3_2, line = arrival_line)
    Path3_Count <- as.numeric(Path3_Info["count"])
    Path3_Time <- as.numeric(Path3_Info["time"])
    # get transfer time each / ex) depart -> transfer
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
    Transfer_2_Count_Time[i] <- as.numeric(Transfer_2[[i]]$Total["Time"])
  }
  Path_2_Shortest <- which.min(Transfer_2_Count_Time)
  # select shortest path(time depend)
  Transfer_2 <- Transfer_2[[Path_2_Shortest]]
  Set <- get_pathresult(Transfer_2)
  # get_route / consider line number 2 & 6-A for circulate system.
  return(Set)
}
