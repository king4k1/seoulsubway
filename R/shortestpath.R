# shortestpath_0
## it will works when depart_line == arrival_line
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

# shortestpath_1
## when using transfer 1 time.
shortestpath_1 <- function(depart, depart_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  Transfer_List <- get_transferinfo(depart, depart_line,
                                    arrival, arrival_line, 
                                    transfer_count = 1)
  if(nrow(Transfer_List)==0){
    stop("you can`t get a path from these transfer count number 1.
         also you should consider branch line '2_A', '2_B', '5_A', '6_A', 'K_A'")
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
    Start_Ind2_1 <- which(subway_data[[arrival_line]]$Name == Transfer_List[i,]$Name)
    End_Ind2_1 <- which(subway_data[[arrival_line]]$Name == arrival)
    Path2_Info <- get_pathinfo(total = Total_Transfer_Raw, start = Start_Ind2_1, 
                               end = End_Ind2_1, line = arrival_line)
    # get transfer time from transfer_info data set; no result can get
    # 2.35(average spend time for transfer)
    Transfer_Time <- transfer_info %>% 
      filter(Transfer_Name == Transfer_List$Name[i]) %>% 
      filter(Transfer_Line == depart_line & Transfer_Line2 == arrival_line)
    if (nrow(Transfer_Time) >= 1) {
      Transfer_Time <- as.numeric(Transfer_Time$Transfer_Time)
    } else {
      Transfer_Time <- 2.35
    }
    Transfer_1[[i]] <- list(Info = data.frame(Depart = c(depart, Transfer_List[i,]$Name),
                                              Line = c(depart_line, arrival_line),
                                              Count = c(Path1_Info$count, Path2_Info$count),
                                              Time = c(Path1_Info$time, Path2_Info$time),
                                              Arrive = c(Transfer_List[i,]$Name, arrival)), 
                            Total = c(Count = Path1_Info$count + Path2_Info$count,
                                      Time = Path1_Info$time + Path2_Info$time + Transfer_Time + 3))
  }
  Transfer_1_Count_Time <- c()
  for (i in seq_along(Transfer_1)) {
    Transfer_1_Count_Time[i] <- as.numeric(Transfer_1[[i]]$Total["Time"])
  }
  Path_1_Shortest <- which.min(Transfer_1_Count_Time)
  Transfer_1 <- Transfer_1[[Path_1_Shortest]]
  # get shortestpath(time depend)
  Set <- get_pathresult(Transfer_1)
  # get_route / consider line number 2 & 6_A for circulate system.
  return(Set)
}

# shortestpath_2
## when using transfer 2 time.
shortestpath_2 <- function(depart, depart_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  # load data
  Total_Depart_Raw <- nrow(subway_data[[depart_line]])
  Total_End_Raw <- nrow(subway_data[[arrival_line]])
  Start_Ind_2 <- which(subway_data[[depart_line]]$Name == depart)
  Transfer_List <- get_transferinfo(depart, depart_line,
                                    arrival, arrival_line, 
                                    transfer_count = 2)
  # get available transfer station list these results are list format and
  # include first/second transfer station.
  if(length(Transfer_List)==0){
    stop("you can`t get a path from these transfer count number 2.
         also you should consider branch line '2-A', '2-B', '5-A', '6-A', 'K2'")
  }
  Transfer_2 <- list()
  for (i in seq_along(Transfer_List)) {
    Transfer_First <- Transfer_List[[i]]$first
    Transfer_Second <- Transfer_List[[i]]$second
    End_Ind_2 <- which(subway_data[[depart_line]]$Name == Transfer_First$Name)
    Transfer_First_Ind <- str_split(Transfer_First$Transfe, pattern = paste0("[", "$", "|", "]")) %>% unlist
    Transfer_Second_Ind <- str_split(Transfer_Second$Transfe, pattern = paste0("[", "$", "|", "]")) %>% unlist
    # process variable for get transfer line
    Transfer_First_Line <- Transfer_Second_Ind[which(Transfer_Second_Ind%in%Transfer_First_Ind)][1]
    if(!(is.na(Transfer_First_Line))){
      Total_Transfer_Raw <- nrow(subway_data[[Transfer_First_Line]])
      Start_Ind2_2 <- which(subway_data[[Transfer_First_Line]]$Name == Transfer_First$Name)
      End_Ind2_2 <- which(subway_data[[Transfer_First_Line]]$Name == Transfer_Second$Name)
      Start_Ind3_2 <- which(subway_data[[arrival_line]]$Name == Transfer_Second$Name)
      End_Ind3_2 <- which(subway_data[[arrival_line]]$Name == arrival)
      # get information about each path way
      Path1_Info <- get_pathinfo(total = Total_Depart_Raw, start = Start_Ind_2, 
                                 end = End_Ind_2, line = depart_line)
      Path2_Info <- get_pathinfo(total = Total_Transfer_Raw, start = Start_Ind2_2, 
                                 end = End_Ind2_2, line = Transfer_First_Line)
      Path3_Info <- get_pathinfo(total = Total_End_Raw, start = Start_Ind3_2, 
                                 end = End_Ind3_2, line = arrival_line)
      # get transfer time each / ex) depart -> transfer
      Transfer_Time1 <- transfer_info %>% 
        filter(Transfer_Name == Transfer_First$Name) %>% 
        filter(Transfer_Line == depart_line & Transfer_Line2 == Transfer_First_Line)
      Transfer_Time2 <- transfer_info %>% 
        filter(Transfer_Name == Transfer_Second$Name) %>% 
        filter(Transfer_Line == Transfer_First_Line & Transfer_Line2 == arrival_line)
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
                                                Count = c(Path1_Info$count, Path2_Info$count, Path3_Info$count), 
                                                Time = c(Path1_Info$time, Path2_Info$time, Path3_Info$time),
                                                Arrive = c(Transfer_First$Name, Transfer_Second$Name, arrival)),
                              Total = c(Count = sum(Path1_Info$count + Path2_Info$count + Path3_Info$count), 
                                        Time = sum(Path1_Info$time + Path2_Info$time + Path3_Info$time) +
                                          Transfer_Time1 + Transfer_Time2 + 6))
    }else{
      Transfer_3[[i]] <- list(Total = c(Time = 300))
    }
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

# shortestpath_3
## when using transfer 3 time.
shortestpath_3 <- function(depart, depart_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  # load data
  Total_Depart_Raw <- nrow(subway_data[[depart_line]])
  Total_End_raw <- nrow(subway_data[[arrival_line]])
  Start_Ind_3 <- which(subway_data[[depart_line]]$Name == depart)
  Transfer_List <- get_transferinfo(depart, depart_line, arrival, arrival_line, 
                                    transfer_count = 3)
  # get available transfer station list these results are list format and
  # get first/second/third transfer station.
  if(length(Transfer_List)==0){
    stop("you can`t get a path from these transfer count number 3.
         also you should consider branch line '2_A', '2_B', '5_A', '6_A', 'K_A'")
  }
  Transfer_3 <- list()
  for (i in seq_along(Transfer_List)) {
    Transfer_First <- Transfer_List[[i]]$first
    Transfer_Second <- Transfer_List[[i]]$second
    Transfer_Third <- Transfer_List[[i]]$third
    End_Ind_3 <- which(subway_data[[depart_line]]$Name == Transfer_First$Name)
    Transfer_First_Ind <- str_split(Transfer_First$Transfe, pattern = paste0("[", "$", "|", "]")) %>% unlist
    Transfer_Second_Ind <- str_split(Transfer_Second$Transfe, pattern = paste0("[", "$", "|", "]")) %>% unlist
    Transfer_Third_Ind <- str_split(Transfer_Third$Transfe, pattern = paste0("[", "$", "|", "]")) %>% unlist
    # process variable for get transfer line
    Transfer_First_Line <- Transfer_Second_Ind[which(Transfer_Second_Ind%in%Transfer_First_Ind)][1]
    Transfer_Second_Line <- Transfer_Third_Ind[which(Transfer_Third_Ind%in%Transfer_Second_Ind)][1]
    if(!(is.na(Transfer_First_Line)) & !(is.na(Transfer_Second_Line))){
      Total_Transfer1_Raw <- nrow(subway_data[[Transfer_First_Line]])
      Total_Transfer2_Raw <- nrow(subway_data[[Transfer_Second_Line]])
      Start_Ind2_3 <- which(subway_data[[Transfer_First_Line]]$Name == Transfer_First$Name)
      End_Ind2_3 <- which(subway_data[[Transfer_First_Line]]$Name == Transfer_Second$Name)
      Start_Ind3_3 <- which(subway_data[[Transfer_Second_Line]]$Name == Transfer_Second$Name)
      End_Ind3_3 <- which(subway_data[[Transfer_Second_Line]]$Name == Transfer_Third$Name)
      Start_Ind4_3 <- which(subway_data[[arrival_line]]$Name == Transfer_Third$Name)
      End_Ind4_3 <- which(subway_data[[arrival_line]]$Name == arrival)
      # get information about each path way
      Path1_Info <- get_pathinfo(total = Total_Depart_Raw, start = Start_Ind_3, 
                                 end = End_Ind_3, line = depart_line)
      Path2_Info <- get_pathinfo(total = Total_Transfer1_Raw, start = Start_Ind2_3, 
                                 end = End_Ind2_3, line = Transfer_First_Line)
      Path3_Info <- get_pathinfo(total = Total_Transfer2_Raw, start = Start_Ind3_3, 
                                 end = End_Ind3_3, line = Transfer_Second_Line)
      Path4_Info <- get_pathinfo(total = Total_End_raw, start = Start_Ind4_3, 
                                 end = End_Ind4_3, line = arrival_line)
      # get transfer time{ex) depart -> transfer} each
      Transfer_Time1 <- transfer_info %>% 
        filter(Transfer_Name == Transfer_First$Name) %>% 
        filter(Transfer_Line == depart_line & Transfer_Line2 ==  Transfer_First_Line)
      Transfer_Time2 <- transfer_info %>% 
        filter(Transfer_Name == Transfer_Second$Name) %>% 
        filter(Transfer_Line == Transfer_First_Line & Transfer_Line2 == Transfer_Second_Line)
      Transfer_Time3 <- transfer_info %>% 
        filter(Transfer_Name == Transfer_Third$Name) %>% 
        filter(Transfer_Line == Transfer_Second_Line & Transfer_Line2 == arrival_line)
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
      if (nrow(Transfer_Time3) >= 1) {
        Transfer_Time3 <- as.numeric(Transfer_Time3$Transfer_Time)
      } else {
        Transfer_Time3 <- 2.35
      }
      Transfer_3[[i]] <- list(Info = data.frame(Depart = c(depart, Transfer_First$Name, Transfer_Second$Name, Transfer_Third$Name),
                                                Line = c(depart_line, Transfer_First_Line, Transfer_Second_Line, arrival_line), 
                                                Count = c(Path1_Info$count, Path2_Info$count, Path3_Info$count, Path4_Info$count),
                                                Time = c(Path1_Info$time, Path2_Info$time, Path3_Info$time, Path4_Info$time),
                                                Arrive = c(Transfer_First$Name, Transfer_Second$Name, Transfer_Third$Name, arrival)),
                              Total = c(Count = sum(Path1_Info$count + Path2_Info$count + Path3_Info$count + Path4_Info$count), 
                                        Time = sum(Path1_Info$time + Path2_Info$time + Path3_Info$time + Path4_Info$time) +
                                          Transfer_Time1 + Transfer_Time2 + Transfer_Time3 + 9))
    }else{
      Transfer_3[[i]] <- list(Total = c(Time = 300))
    }
  }
  Transfer_3_Count_Time <- c()
  for (i in seq_along(Transfer_3)) {
    Transfer_3_Count_Time[i] <- as.numeric(Transfer_3[[i]]$Total["Time"])
  }
  Path_3_Shortest <- which.min(Transfer_3_Count_Time)
  # select shortest path(time depend)
  Transfer_3 <- Transfer_3[[Path_3_Shortest]]
  Set <- get_pathresult(Transfer_3)
  # get_route / consider line number 2 & 6_A for circulate system.
  return(Set)
}

### if shortestpath_3 does not work than, shortestpath_4 will work 
### for 3 nearest transfer available stations
### it depend on shortestpath_3..
shortestpath_4 <- function(depart, depart_line, arrival, arrival_line) {
  data("subway_data")
  arr_line <- subway_data[[arrival_line]]
  index <- which(arr_line$Name == arrival)
  tr_arr_line <- which(arr_line$Transfer != 0)
  nn_rank <- rank(abs(index - tr_arr_line))
  nn_list <- tr_arr_line[nn_rank <= 3]
  nn_list_nm <- arr_line$Name[nn_list]
  nn_list_line <- arr_line$Transfer[nn_list]
  result <- result2 <- result_which <- list()
  for (i in seq_along(nn_list_nm)) {
    multi_linecase <- nn_list_line[i]
    multi_linecase <- str_split(multi_linecase, pattern = "|")[[1]]
    multi_linecase <- multi_linecase[multi_linecase != ""]
    multi_linecase <- multi_linecase[multi_linecase != "|"]
    for (j in seq_along(multi_linecase)){
     result[[paste0(i, "-", j)]] <- 
       tryCatch(shortestpath_3(depart, depart_line, 
                               arrival = nn_list_nm[i], 
                               arrival_line = multi_linecase[j]), 
                error = function(e) {result[[paste0(i, "-", j)]] = list(Time = 300)})
    result_which[[paste0(i, "-", j)]] <- result[[paste0(i, "-", j)]]$Time
    result2[[paste0(i, "-", j)]] <- 
      tryCatch(shortestpath_1(depart = nn_list_nm[i], 
                              depart_line = multi_linecase[j],
                              arrival, arrival_line), 
               error = function(e) {})
    }
  }
  shortest_index <- names(result_which)[which.min(result_which)]
  result[[shortest_index]]$Info <- rbind(result[[shortest_index]]$Info, 
                                         result2[[shortest_index]]$Info[2, ])
  result[[shortest_index]]$Count <- result[[shortest_index]]$Count + 
    result2[[shortest_index]]$Count
  result[[shortest_index]]$Time <- result[[shortest_index]]$Time + result2[[shortest_index]]$Time
  result[[shortest_index]]$Path4 <- result2[[shortest_index]]$Path2
  result <- result[[shortest_index]]
  if(is.null(result)){      
    message("If the station and the line do not match, the route can not be obtained.
also you should consider branch line '1_P','1_A','1_B','2_A','2_B','5_A','6_A' and 'K_A'
* Guro to Shin Chang Station : '1_P'
* Geumcheon-gu Office to Kwang Myung Station : '1_A' 
* Byeongjeom to Seodongtan Station : '1_B'
* Seongsu to Sinseol-dong Station : '2_A' \n* Sindorim to Kkachisan Station : '2_B'
* Gangdong to Macheon Station : '5_A' \n* Eungam to Gusan(also include Eungam) Station : '6_A'
* Gajwa to Seoul Station : 'K_A'")
  }
  return(result)
}

# shortestpath_inner
shortestpath_all <- function(depart, depart_line, arrival, arrival_line) {
  if (depart_line == arrival_line) {
    Zero <- shortestpath_0(depart = depart, depart_line = depart_line, 
                           arrival = arrival, arrival_line = arrival_line)
    Total <- list(Zero)
    Short_Path_Ind <- 1
  }
  if (depart_line != arrival_line) {
    One <- tryCatch(shortestpath_1(depart = depart, depart_line = depart_line, 
                                   arrival = arrival, arrival_line = arrival_line), 
                    error = function(e) {
                                     One = list(Time = 300)
                                   })
    Two <- tryCatch(shortestpath_2(depart = depart, depart_line = depart_line, 
                                   arrival = arrival, arrival_line = arrival_line), 
                    error = function(e) {
                                     Two = list(Time = 300)
                                   })
    # use tryCatch() for consider error case get results of thw case, and
    # select shortest path
    Total <- list(One, Two)
    Short_Path_Ind <- which.min(c(Total[[1]]$Time, Total[[2]]$Time))
  }
  Total <- Total[[Short_Path_Ind]]
  if (Total$Time == 300) {
    Total <- shortestpath_3(depart = depart, depart_line = depart_line, 
                            arrival = arrival, arrival_line = arrival_line)
  }
  # if shortest path(by three transfer) is null -> consider four transfer
  if (Total$Time == 300) {
    Total <- shortestpath_4(depart = depart, depart_line = depart_line, 
                            arrival = arrival, arrival_line = arrival_line)
  }
  return(Total)
}


## shortestpath
### it needs only depart, arrival line.
### but it spend many time when shortestpath_3 dosen't work..
shortestpath <- function(depart, arrival) {
  data("subway_data_DT")
  depart_line_list <- subway_data_DT[which(subway_data_DT$Name %in% 
                                             c(depart)), ]$Line
  arrival_line_list <- subway_data_DT[which(subway_data_DT$Name %in% 
                                              c(arrival)), ]$Line
  result <- shortest_index <- list()
  for (i in seq_along(depart_line_list)) {
    for (j in seq_along(arrival_line_list)) {
      result[[paste0(i, "-", j)]] <-
        tryCatch(shortestpath_all(depart, depart_line = depart_line_list[i],
                              arrival, arrival_line = arrival_line_list[j]), 
                 error = function(e) {
                   result[[paste0(i, "-", j)]] = list(Time = 300)
                 })
      shortest_index[[paste0(i, "-", j)]] <-
        result[[paste0(i, "-", j)]]$Time
    }
  }
  result_return <- result[[names(result)[which.min(shortest_index)]]]
  return(result_return)
}


