# shortestpath_0
## it will works when departure_line == arrival_line
shortestpath_0 <- function(departure, departure_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  Start_Ind_0 <- which(subway_data[[departure_line]]$Name == departure)
  End_Ind_0 <- which(subway_data[[departure_line]]$Name == arrival)
  Total_departure_Raw <- nrow(subway_data[[departure_line]])
  if (departure_line == arrival_line) {
    Path_Info <- get_pathinfo(total = Total_departure_Raw, start = Start_Ind_0, 
                              end = End_Ind_0, line = departure_line)
    Path_Count <- as.numeric(Path_Info["count"])
    Path_Time <- as.numeric(Path_Info["time"])
    Transfer_0 <- data.frame(Departure = departure, Line = departure_line, Count = Path_Count,
                             Time = Path_Time, Arrival = arrival,
                             Total_Count = Path_Count, Total_Time = Path_Time)
  }
  return(Transfer_0)
}

# shortestpath_1(departure, departure_line, arrival, arrival_line)
## when using transfer 1 time.
shortestpath_1 <- function(departure, departure_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  Transfer_List <- get_transferinfo(departure, departure_line,
                                    arrival, arrival_line, 
                                    transfer_count = 1)
  if(nrow(Transfer_List)==0){
    stop("you can`t get a path from these transfer count number 1.
         also you should consider branch line '2_A', '2_B', '5_A', '6_A', 'K_A'")
  }
  # get available transfer station list
  Total_departure_Raw <- nrow(subway_data[[departure_line]])
  Total_Transfer_Raw <- nrow(subway_data[[arrival_line]])
  Transfer_1 <- list()
  # first setting for loops
  Start_Ind_1 <- which(subway_data[[departure_line]]$Name == departure)
  End_Ind_1 <- c()
  for (i in 1:nrow(Transfer_List)) {
    End_Ind_1[i] <- which(subway_data[[departure_line]]$Name == Transfer_List$Name[i])
    Path1_Info <- get_pathinfo(total = Total_departure_Raw, start = Start_Ind_1, 
                               end = End_Ind_1[i], line = departure_line)
    # get time / count information from get_pathinfo() and manufacture one.
    Start_Ind2_1 <- which(subway_data[[arrival_line]]$Name == Transfer_List[i,]$Name)
    End_Ind2_1 <- which(subway_data[[arrival_line]]$Name == arrival)
    Path2_Info <- get_pathinfo(total = Total_Transfer_Raw, start = Start_Ind2_1, 
                               end = End_Ind2_1, line = arrival_line)
    # get transfer time from transfer_info data set; no result can get
    # 2.35(average spend time for transfer)
    Transfer_Time <- transfer_info %>% 
      filter(Transfer_Name == Transfer_List$Name[i]) %>% 
      filter(Transfer_Line == departure_line & Transfer_Line2 == arrival_line)
    if (nrow(Transfer_Time) >= 1) {
      Transfer_Time <- as.numeric(Transfer_Time$Transfer_Time)
    } else {
      Transfer_Time <- 2.35
    }
    Transfer_1[[i]] <- data.frame(Departure = c(departure, Transfer_List[i,]$Name),
                                  Line = c(departure_line, arrival_line),
                                  Count = c(Path1_Info$count, Path2_Info$count),
                                  Time = c(Path1_Info$time, Path2_Info$time),
                                  Arrival = c(Transfer_List[i,]$Name, arrival), 
                                  Total_Count = Path1_Info$count + Path2_Info$count,
                                  Total_Time = Path1_Info$time + Path2_Info$time + Transfer_Time + 3)
  }
  return(Transfer_1)
  }

# shortestpath_2
## when using transfer 2 time.
shortestpath_2 <- function(departure, departure_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  # load data
  Total_departure_Raw <- nrow(subway_data[[departure_line]])
  Total_End_Raw <- nrow(subway_data[[arrival_line]])
  Start_Ind_2 <- which(subway_data[[departure_line]]$Name == departure)
  Transfer_List <- get_transferinfo(departure, departure_line,
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
    End_Ind_2 <- which(subway_data[[departure_line]]$Name == Transfer_First$Name)
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
      Path1_Info <- get_pathinfo(total = Total_departure_Raw, start = Start_Ind_2, 
                                 end = End_Ind_2, line = departure_line)
      Path2_Info <- get_pathinfo(total = Total_Transfer_Raw, start = Start_Ind2_2, 
                                 end = End_Ind2_2, line = Transfer_First_Line)
      Path3_Info <- get_pathinfo(total = Total_End_Raw, start = Start_Ind3_2, 
                                 end = End_Ind3_2, line = arrival_line)
      # get transfer time each / ex) departure -> transfer
      Transfer_Time1 <- transfer_info %>% 
        filter(Transfer_Name == Transfer_First$Name) %>% 
        filter(Transfer_Line == departure_line & Transfer_Line2 == Transfer_First_Line)
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
      Transfer_2[[i]] <- data.frame(Departure = c(departure, Transfer_First$Name, Transfer_Second$Name),
                                    Line = c(departure_line, Transfer_First_Line, arrival_line), 
                                    Count = c(Path1_Info$count, Path2_Info$count, Path3_Info$count), 
                                    Time = c(Path1_Info$time, Path2_Info$time, Path3_Info$time),
                                    Arrival = c(Transfer_First$Name, Transfer_Second$Name, arrival),
                                    Total_Count = sum(Path1_Info$count + Path2_Info$count + Path3_Info$count), 
                                    Total_Time = sum(Path1_Info$time + Path2_Info$time + Path3_Info$time) +
                                      Transfer_Time1 + Transfer_Time2 + 6)
    }else{
      Transfer_2[[i]] <- data.frame(Total_Time = 300)
    }
  }
  return(Transfer_2)
  }

# shortestpath_3(departure, departure_line, arrival, arrival_line)
## when using transfer 3 time.
shortestpath_3 <- function(departure, departure_line, arrival, arrival_line) {
  data("subway_data", envir = environment())
  data("transfer_info", envir = environment())
  # load data
  Total_departure_Raw <- nrow(subway_data[[departure_line]])
  Total_End_raw <- nrow(subway_data[[arrival_line]])
  Start_Ind_3 <- which(subway_data[[departure_line]]$Name == departure)
  Transfer_List <- get_transferinfo(departure, departure_line, arrival, arrival_line, 
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
    End_Ind_3 <- which(subway_data[[departure_line]]$Name == Transfer_First$Name)
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
      Path1_Info <- get_pathinfo(total = Total_departure_Raw, start = Start_Ind_3, 
                                 end = End_Ind_3, line = departure_line)
      Path2_Info <- get_pathinfo(total = Total_Transfer1_Raw, start = Start_Ind2_3, 
                                 end = End_Ind2_3, line = Transfer_First_Line)
      Path3_Info <- get_pathinfo(total = Total_Transfer2_Raw, start = Start_Ind3_3, 
                                 end = End_Ind3_3, line = Transfer_Second_Line)
      Path4_Info <- get_pathinfo(total = Total_End_raw, start = Start_Ind4_3, 
                                 end = End_Ind4_3, line = arrival_line)
      # get transfer time{ex) departure -> transfer} each
      Transfer_Time1 <- transfer_info %>% 
        filter(Transfer_Name == Transfer_First$Name) %>% 
        filter(Transfer_Line == departure_line & Transfer_Line2 ==  Transfer_First_Line)
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
      Transfer_3[[i]] <- data.frame(Departure = c(departure, Transfer_First$Name, Transfer_Second$Name, Transfer_Third$Name),
                                    Line = c(departure_line, Transfer_First_Line, Transfer_Second_Line, arrival_line), 
                                    Count = c(Path1_Info$count, Path2_Info$count, Path3_Info$count, Path4_Info$count),
                                    Time = c(Path1_Info$time, Path2_Info$time, Path3_Info$time, Path4_Info$time),
                                    Arrival = c(Transfer_First$Name, Transfer_Second$Name, Transfer_Third$Name, arrival),
                                    Total_Count = sum(Path1_Info$count + Path2_Info$count + Path3_Info$count + Path4_Info$count), 
                                    Total_Time = sum(Path1_Info$time + Path2_Info$time + Path3_Info$time + Path4_Info$time) +
                                      Transfer_Time1 + Transfer_Time2 + Transfer_Time3 + 9)
    }else{
      Transfer_3[[i]] <- data.frame(Total_Time = 300)
    }
  }
  return(Transfer_3)
  }

### if shortestpath_3 does not work than, shortestpath_4 will work 
### for 3 nearest transfer available stations
### it depend on shortestpath_3..
shortestpath_4 <- function(departure, departure_line, arrival, arrival_line) {
  data("subway_data")
  result_list <- function(station, line_choose){
    near_list <- subway_data[[line_choose]]
    index <- which(near_list$Name == station)
    avaliable_line <- near_list[which(near_list$Transfer != 0),]
    nearest_rank <- rank(abs(index - which(near_list$Transfer != 0)))
    nn_list_nm <- avaliable_line$Name[nearest_rank<=3]
    nn_list_nm
  }
  nn_list_nm <- result_list(station = arrival, line_choose = arrival_line)
  result <- result2 <- result_which <- list()
  for (i in seq_along(nn_list_nm)) { 
    multi_linecase <- subway_data[[arrival_line]] %>% 
      filter(Name %in% nn_list_nm[i]) %>% select(Transfer) %>% t() %>% as.character()
    multi_linecase <- str_split(multi_linecase, pattern = "|")[[1]]
    multi_linecase <- multi_linecase[multi_linecase != ""]
    multi_linecase <- multi_linecase[multi_linecase != "|"]
    for (j in seq_along(multi_linecase)){ 
      first_path <- shortestpath_3(departure, departure_line, 
                                   arrival = nn_list_nm[i], arrival_line = multi_linecase[j])
      second_path <- shortestpath_1(departure = nn_list_nm[i], departure_line = multi_linecase[j],
                                    arrival, arrival_line)
      first_vec <- seq_along(first_path)
      second_vec <- seq_along(second_path)
      index_tbl <- expand.grid(first = first_vec, second = second_vec) %>% as_tibble()
      for(k in 1:nrow(index_tbl)){ 
        first_path[[index_tbl$first[k]]]$Info <- rbind(first_path[[index_tbl$first[k]]]$Info, 
                                                       second_path[[index_tbl$second[k]]]$Info)
        first_path[[index_tbl$first[k]]]$Count <- rbind(first_path[[index_tbl$first[k]]]$Count, 
                                                        second_path[[index_tbl$second[k]]]$Count)
        first_path[[index_tbl$first[k]]]$Time <- rbind(first_path[[index_tbl$first[k]]]$Time, 
                                                       second_path[[index_tbl$second[k]]]$Time)
      }
      result[[paste0(i, "-", j)]] <- first_path
    }
  }
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
shortestpath_all <- function(departure, departure_line, arrival, arrival_line) {
  if (departure_line == arrival_line) {
    Total <- shortestpath_0(departure = departure, departure_line = departure_line, 
                            arrival = arrival, arrival_line = arrival_line)
    Total <- list(zero = Total)
  }
  if (departure_line != arrival_line) {
    One <- tryCatch(shortestpath_1(departure = departure, departure_line = departure_line, 
                                   arrival = arrival, arrival_line = arrival_line), 
                    error = function(e) {One = list(data.frame(Total_Time = 300))})
    Two <- tryCatch(shortestpath_2(departure = departure, departure_line = departure_line, 
                                   arrival = arrival, arrival_line = arrival_line), 
                    error = function(e) {Two = list(data.frame(Total_Time = 300))})
    # use tryCatch() for consider error case get results of thw case, and
    # select shortest path
    Total <- list(first=One, second=Two)
    index <- list()
    for(i in seq_along(Total)){
      for(j in seq_along(Total[[i]])){
        index[[paste0(i, "-", j)]] <- Total[[i]][[j]]$Total_Time[1]
      }
    }### 시간값이 모두 300, 즉 잘못된 값이면 3번 환승거리 계산
    if (sum(300 <= index %>% unlist()) == length(index)) {
      Total <- tryCatch(shortestpath_3(departure = departure, departure_line = departure_line, 
                                       arrival = arrival, arrival_line = arrival_line),
                        error = function(e) {Total = list(data.frame(Total_Time = 300))})
      if (sum(300 <= sapply(seq_along(Total),
                            function(x){Total[[x]]$Total_Time[1]})) == length(Total)) {
        Total <- shortestpath_4(departure = departure, departure_line = departure_line, 
                                arrival = arrival, arrival_line = arrival_line)
        names(Total) <- NULL
      }
    }
  }
  return(Total)
}

## shortestpath
### it needs only departure, arrival line.
### but it spend many time when shortestpath_3 dosen't work..
shortestpath <- function(departure, arrival, max_route=1) {
  data("subway_data_DT")
  departure_line_list <- subway_data_DT[which(subway_data_DT$Name %in% c(departure)), ]$Line
  arrival_line_list <- subway_data_DT[which(subway_data_DT$Name %in% c(arrival)), ]$Line
  result <- list()
  for (i in seq_along(departure_line_list)) { 
    for (j in seq_along(arrival_line_list)) { 
      result[[paste0(i, "-", j)]] <-
        tryCatch(shortestpath_all(departure, departure_line = departure_line_list[i],
                                  arrival, arrival_line = arrival_line_list[j]), 
                 error = function(e) {
                   result[[paste0(departure_line_list[i],
                                  "-",  arrival_line_list[j])]] <- 
                     list(data.frame(Total_Time = 300))})
    }
  }
  path_index <- list()
  nms_index <- names(result)
  for(i in seq_along(nms_index)){
    inner_vec <- result[[nms_index[i]]] %>% names()
    for(j in seq_along(inner_vec)){
      if(is.data.frame(result[[nms_index[i]]][[j]])){
        path_index[[paste0(i, "-", j, "-", 1)]] <- result[[i]][[j]]
      }else{
        for(k in seq_along(result[[nms_index[i]]][[j]])){
          if(is.data.frame(result[[i]][[j]][[k]]) & result[[i]][[j]][[k]]$Total_Time[1] !=300){
            path_index[[paste0(i, "-", j, "-", k)]] <- result[[i]][[j]][[k]]
          }else{
            path_index[[paste0(i, "-", j, "-", k)]] <- NULL 
          }
        }
      }
    }
  }
  index_edit <- time_rank <- list()
  ### remove duplicated path
  for(s in seq_along(path_index)){
    if(!(0 %in% path_index[[names(path_index)[s]]]$Count)){
      index_edit[[names(path_index)[s]]] <- path_index[[names(path_index)[s]]]
      time_rank[[names(path_index)[s]]] <- path_index[[names(path_index)[s]]]$Total_Time[1]
    }
  }
  rank_nms <- time_rank %>% unlist() %>% sort() %>% names()
  rank_nms <- rank_nms[1:min(length(rank_nms), max_route)] 
  path_nms <- paste0("path_", seq_along(rank_nms))
  path_result <- list()
  for(i in seq_along(rank_nms)){
    path_i <- index_edit[[rank_nms[i]]]
    path_result[[path_nms[i]]] <- list(Info = path_i %>% dplyr::select(-starts_with("Total")),
                                       Total = path_i[1,] %>% dplyr::select(starts_with("Total")) %>% 
                                         rename(Time = Total_Time, Count = Total_Count))
  }
  return(path_result)
}




