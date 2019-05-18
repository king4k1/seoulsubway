### 보조함수

# 보조함수(checkline)

# 지하철 노선형태로 네트워크를 구축하므로 중도 노선명이 잘못하여 입력되는 경우가 존재함.
# checkline will check the dataset for select accurate line 
checkline <- function(dat, depart_line, arrival_line) {
  data("subway_data", envir = environment())
  # in multi transfer case, we should consider every possible line 
  if (FALSE %in% (names(subway_data) %in% arrival_line)) {
    if (isTRUE(str_detect(depart_line, arrival_line) | str_detect(arrival_line, 
                                                                  depart_line)) == FALSE) {
      anywrongdat_list <- str_remove(dat$Transfer, paste0(depart_line, "_", "A"))
      anywrongdat_list <- str_remove(anywrongdat_list, paste0(depart_line, "_", "P"))
      anywrongdat_list <- str_remove(anywrongdat_list, paste0(depart_line, "_", "B"))
      anyrightdat <- str_which(anywrongdat_list, depart_line)
      dat <- dat[anyrightdat, ]
      anywrongdat_list <- str_remove(dat$Transfer, paste0(arrival_line, "_", "A"))
      anywrongdat_list <- str_remove(anywrongdat_list, paste0(arrival_line, "_", "P"))
      anywrongdat_list <- str_remove(anywrongdat_list, paste0(arrival_line, "_", "B"))
      anyrightdat <- str_which(anywrongdat_list, arrival_line)
      dat <- dat[anyrightdat,]
    }
  }else{
    anywrongdat_list <- str_remove(dat$Transfer, paste0(depart_line, "_", "A"))
    anywrongdat_list <- str_remove(anywrongdat_list, paste0(depart_line, "_", "B"))
    anywrongdat_list <- str_remove(anywrongdat_list, paste0(depart_line, "_", "P"))
    anyrightdat <- str_which(anywrongdat_list, depart_line)
    dat <- dat[anyrightdat, ]
  }
  return(dat)
}

# 보조함수(get_pathinfo)

# 하나의 노선에 대하여 총 역수(total), start(역 1 위치순서), end(역 2 위치순서) 그리고 line(노선)을 입력하면
# 두 역 사이의 이동역수와 소요시간을 산출한다.
get_pathinfo <- function(total, start, end, line) {
  data(subway_data, envir = environment())
  if(is.null(start)|is.null(end)){
    stop("you can`t get a path from these transfer count number")
  }
  Path_Count <- abs(start - end)
  Path_Time <- sum(subway_data[[line]][start:end,]$Time) - subway_data[[line]][start,]$Time
  # (normal case) get index.
  if (line == "2") {
    Circulate <- total - abs(start - end)
    Circulate2 <- abs(start - end)
    Circulate <- c(Circulate, Circulate2)
    Circulate_Time <- c(sum(subway_data[[line]][c(start:total,1:end),"Time", with=FALSE]) - 
                          subway_data[[line]][start, "Time", with=FALSE],
                        sum(subway_data[[line]][c(end:total,1:start),"Time", with=FALSE]) -
                          subway_data[[line]][start,"Time", with=FALSE])
    Circulate_Time <- Circulate_Time[which.min(Circulate_Time)]
    Circulate2_Time <- sum(subway_data[[line]][start:end,"Time", with=FALSE]) - 
      subway_data[[line]][start,"Time", with=FALSE]
    Circulate_Time <- c(Circulate_Time, Circulate2_Time)
    Path_Count <- Circulate[which.min(Circulate_Time)]
    Path_Time <- Circulate_Time[which.min(Circulate_Time)]
    # consider circulate line(line number 2) => use absolute value & total - abs
  }
  if (line == "6_A" & start > end) {
    Path_Count <- 6 + end - start 
    Path_Time <- sum(subway_data[[line]][c(end:6,1:start),"Time", with=FALSE]) - 
      subway_data[[line]][start,"Time", with=FALSE] + 3 
    # 6-A -> 6-A circulate line.
  }
  return(data.frame(count = as.numeric(Path_Count), time = as.numeric(Path_Time) %>% round(2)))
}

# 보조함수(get_transfercriteria)

# 두역에 대한 경로에 대하여 위경도를 기준으로 0.05씩 넓게 패널티를 주어
# 자른 공간에 속하는 출발역과 도착역이 모두 포함된 환승가능한 역을 산출한다.
# shortestpath() 함수의 속도를 향상시키기 위함에 본 함수의 목적이 있다.

get_transfercriteria <- function(depart, arrival, penalty) {
  data(subway_data_DT, envir = environment())
  data(subway_data, envir = environment())
  data(transfer_info, envir = environment())
  data(transfer_station, envir = environment())
  # load data
  
  station_info <- subway_data_DT %>%
    filter(Name %in% c(depart, arrival)) %>% filter(!duplicated(Name))
  # get criteria from depart/arrival's longitude&lattitude
  lat_criteria <- station_info$lat %>% as.numeric
  lat_lowerbound <- min(lat_criteria) - penalty
  lat_upperbound <- max(lat_criteria) + penalty
  long_criteria <- station_info$long %>% as.numeric
  long_lowerbound <- min(long_criteria) - penalty
  long_upperbound <- max(long_criteria) + penalty
  # give penalty for bypass selection
  transfer_list <- transfer_station %>% 
    filter(lat <= lat_upperbound & lat >= lat_lowerbound) %>% 
    filter(long <= long_upperbound & long >= long_lowerbound)
  return(transfer_list)
}

# 보조함수(get_transferinfo)

# get_transfercriteria에 기준하여 위경도 기준으로 환승지의 후보군을 선택한 후,
# 범위를 줄여 각 환승 카운트에 대하여 예상 환승지를 선택한다.
# 만약 두번 환승을 하는 경우라고 한다면, 첫번째 환승예상지역에 대하여 get_transfercriteria로 인해 선정된
# 지역 내 속하는 첫번째 환승역을 택한 후, 이 첫번째 환승역과 도착역 사이의 잘라진 공간에서의 환승역을 잡아
# 다음 환승역을 선정하는 방식으로 알고리즘은 이루어진다.
# 이 함수는 shortestpath()함수의 속도를 향상시키기 위함에 목적이 있다.

get_transferinfo <- function(depart, depart_line, arrival, arrival_line, transfer_count) {
  data("subway_data_DT", envir = environment())
  data("transfer_info", envir = environment())
  data("subway_data", envir = environment())
  # load data
  transfer_list <- get_transfercriteria(depart, arrival, penalty = 0.07)
  # set criteria for available transfer station
  if (transfer_count == 1) {
    transfer_depart <- transfer_list %>% filter(str_detect(Transfer, fixed(depart_line)))
    transfer_arrival <- transfer_depart %>% filter(str_detect(Transfer, fixed(arrival_line)))
    transfer_arrival <- checkline(dat = transfer_arrival, 
                                  depart_line = depart_line, 
                                  arrival_line = arrival_line)
    # find available transfer station(depart, arrival both)
    transfer_arrival$Name %in% subway_data[[arrival_line]]$Name
    transfer_arrival <- transfer_arrival %>% filter(Name %in% subway_data[[arrival_line]]$Name)
  }
  if (transfer_count == 2) {
    transfer_arrival <- list()
    transfer_middle <- transfer_list %>% 
      filter(str_detect(Transfer, fixed(depart_line))) %>% 
      checkline(depart_line = depart_line, arrival_line = names(subway_data))
    # set available transfer station for waypoint.(== first transfer station)
    transfer_middle_list_sub <- str_remove(transfer_middle$Transfer, fixed(depart_line))
    for (i in 1:nrow(transfer_middle)) {
      transfer_middle_list <- unlist(str_split(transfer_middle_list_sub[i], pattern = fixed("|")))    
      index <- which(transfer_middle_list=="")
      if(length(index)!=0){
        transfer_middle_list <- transfer_middle_list[-index]
      }
      for (j in seq_along(transfer_middle_list)) {
        transfer_long <- get_transfercriteria(transfer_middle$Name[i], arrival, penalty = 0.05)
        # set criteria for available transfer station
        transfer_middle_get <- transfer_long %>% 
          filter(str_detect(Transfer, transfer_middle_list[j])) %>% 
          filter(str_detect(Transfer, fixed(arrival_line)))
        if(nrow(transfer_middle_get)!=0){
          transfer_middle_get <- transfer_middle_get %>% 
            filter(Name %in% subway_data[[transfer_middle_list[j]]]$Name) %>%
            filter(Name %in% subway_data[[arrival_line]]$Name) %>% 
            checkline(depart_line = transfer_middle_list[j], arrival_line = arrival_line)
        } 
        # use checkline for erro selection.
        for (k in 1:nrow(transfer_middle_get)) {
          transfer_arrival[[paste0(i, "-", j, "-", k)]] <- list(first = transfer_middle[i, ],
                                                                second = transfer_middle_get[k, ])
        }
      }
    }
    cut_na <- c()
    cut_dup <- c()
    for (k in seq_along(transfer_arrival)) {
      cut_na[k] <- is.na(transfer_arrival[[k]]$second[1,1])
      cut_dup[k] <- isTRUE(transfer_arrival[[k]]$first[1,1] == transfer_arrival[[k]]$second[1,1])
    }
    # if second transfer station is null(no result) ==> remove one
    cut_list <- names(transfer_arrival)[which(cut_na)]
    cut_dup <- names(transfer_arrival)[which(cut_dup)]
    for (l in seq_along(cut_list)) {
      transfer_arrival[[cut_list[l]]] <- NULL
    }
    for (l in seq_along(cut_dup)) {
      transfer_arrival[[cut_dup[l]]] <- NULL
    }
  }
  if (transfer_count == 3) {
    transfer_arrival <- list()
    transfer_middle_first <- transfer_list %>% 
      filter(str_detect(Transfer, fixed(depart_line))) %>% 
      checkline(depart_line = depart_line, arrival_line = names(subway_data))
    transfer_middle_list_sub <- str_remove(transfer_middle_first$Transfer, fixed(depart_line))
    for (i in 1:nrow(transfer_middle_first)) {
      transfer_middle_list <- str_split(transfer_middle_list_sub[i], pattern = fixed("|")) %>% unlist()  
      index <- which(transfer_middle_list=="")
      if(length(index)!=0){
        transfer_middle_list <- transfer_middle_list[-index]
      }
      for (j in seq_along(transfer_middle_list)) {
        transfer_long <- get_transfercriteria(transfer_middle_first$Name[i],
                                              arrival, penalty = 0.05)
        transfer_middle_second <- transfer_long %>% filter(str_detect(Transfer, transfer_middle_list[j]))
        transfer_middle_second <- checkline(dat = transfer_middle_second, 
                                            depart_line = transfer_middle_list[j], 
                                            arrival_line = names(subway_data))
        for (j2 in 1:nrow(transfer_middle_second)) {
          transfer_middle2_list <- unlist(str_split(transfer_middle_second$Transfer[j2], pattern = fixed("|")))    
          index <- which(transfer_middle2_list=="")
          if(length(index)!=0){
            transfer_middle2_list <- transfer_middle2_list[-index]
          }
          for (k in seq_along(transfer_middle2_list)) {
            transfer_long <- get_transfercriteria(transfer_middle_second$Name[j2], arrival, penalty = 0.05)
            # set criteria wider for available transfer station(branch line)
            transfer_middle_get <- transfer_long %>%
              filter(str_detect(Transfer, transfer_middle2_list[j])) %>%
              filter(str_detect(Transfer, fixed(arrival_line)))
            if(nrow(transfer_middle_get)!=0){
              transfer_middle_get <- transfer_middle_get %>% 
                filter(Name %in% subway_data[[transfer_middle2_list[j]]]$Name) %>% 
                filter(Name %in% subway_data[[arrival_line]]$Name) %>% 
                checkline(depart_line = transfer_middle2_list[k], arrival_line = arrival_line)
            }
            for (k2 in 1:nrow(transfer_middle_get)) {
              transfer_arrival[[paste0(i, "-", j, "-", j2, "-", 
                                      k, "-", k2)]] <- list(first = transfer_middle_first[i, ],
                                                            second = transfer_middle_second[j2, ],
                                                            third = transfer_middle_get[k2, ])
            }
          }
        }
      }
    }
    cut_na <- c()
    cut_dup <- c()
    cut_dup2 <- c()
    for (k in seq_along(transfer_arrival)) {
      cut_na[k] <- is.na(transfer_arrival[[k]]$third[1, 1])
      cut_dup[k] <- isTRUE(transfer_arrival[[k]]$first[1, 1] == transfer_arrival[[k]]$second[1, 1])
      cut_dup2[k] <- isTRUE(transfer_arrival[[k]]$second[1, 1] == transfer_arrival[[k]]$third[1, 1])
    }
    # if second transfer station is null(no result) ==> remove one
    cut_list <- names(transfer_arrival)[which(cut_na)]
    cut_dup <- names(transfer_arrival)[which(cut_dup)]
    cut_dup2 <- names(transfer_arrival)[which(cut_dup2)]
    for (l in seq_along(cut_list)) {
      transfer_arrival[[cut_list[l]]] <- NULL
    }
    for (l in seq_along(cut_dup)) {
      transfer_arrival[[cut_dup[l]]] <- NULL
    }
    for (l in seq_along(cut_dup2)) {
      transfer_arrival[[cut_dup2[l]]] <- NULL
    }
  }
  return(transfer_arrival)
}

# 보조함수(get_pathresult)

# 최단거리에 따른 세부 이동경로에 대한 결과를 산출하는 함수
# 결과 포맷을 일정하게 함수형태로 산출하기 위함

get_pathresult <- function(shortestpath_result) {
  data("subway_data", envir = environment())
  Set <- list(Info = shortestpath_result$Info,
              Count = shortestpath_result$Total['Count'] %>% as.numeric %>% round(2),
              Time = shortestpath_result$Total['Time'] %>% as.numeric %>% round(2))
  if (nrow(Set$Info) == 1) {
    Start_Ind_0 <- which(subway_data[[as.character(Set$Info$Line[1])]]$Name == 
                           as.character(Set$Info$Depart[1]))
    End_Ind_0 <- which(subway_data[[as.character(Set$Info$Line[1])]]$Name == 
                         as.character(Set$Info$Arrive[1]))
    Total_Depart_Raw <- nrow(subway_data[[as.character(Set$Info$Line[1])]])
    Set$Path <- subway_data[[as.character(Set$Info$Line[1])]][Start_Ind_0:End_Ind_0, ]
    if (isTRUE(Set$Info$Line[1] == 2) & 
        isTRUE(Set$Info$Count[1] == (Total_Depart_Raw - Start_Ind_0 + End_Ind_0))) {
      Set$Path <- subway_data[["2"]][c(Start_Ind_0:Total_Depart_Raw, 1:End_Ind_0), ]
    } else if (isTRUE(Set$Info$Line[1] == 2) & 
               isTRUE(Set$Info$Count[1] == (Total_Depart_Raw - End_Ind_0 + Start_Ind_0))) {
      Set$Path <- subway_data[["2"]][c(Start_Ind_0:1, Total_Depart_Raw:End_Ind_0), ]
    }
    if (isTRUE(Set$Info$Line[1] == "6_A") & Start_Ind_0 > End_Ind_0) {
      Set$Path <- subway_data[["6_A"]][c(Start_Ind_0 :6, 1:End_Ind_0), ]
    }
  }
  if (nrow(Set$Info) == 2) {
    Start_Ind_1 <- which(subway_data[[as.character(Set$Info$Line[1])]]$Name == 
                           as.character(Set$Info$Depart[1]))
    End_Ind_1 <- which(subway_data[[as.character(Set$Info$Line[1])]]$Name == 
                         as.character(Set$Info$Arrive[1]))
    Start_Ind2_1 <- which(subway_data[[as.character(Set$Info$Line[2])]]$Name == 
                            as.character(Set$Info$Depart[2]))
    End_Ind2_1 <- which(subway_data[[as.character(Set$Info$Line[2])]]$Name == 
                          as.character(Set$Info$Arrive[2]))
    Total_Depart_Raw <- nrow(subway_data[[as.character(Set$Info$Line[1])]])
    Total_Transfer_Raw <- nrow(subway_data[[as.character(Set$Info$Line[2])]])
    Set$Path1 <- subway_data[[as.character(Set$Info$Line[1])]][Start_Ind_1:End_Ind_1, ]
    Set$Path2 <- subway_data[[as.character(Set$Info$Line[2])]][Start_Ind2_1:End_Ind2_1, ]
    # default setting for normal case;
    if (isTRUE(as.character(Set$Info$Line[1]) ==
               2) & isTRUE(Set$Info$Count[1] == (Total_Depart_Raw - Start_Ind_1 + End_Ind_1))) {
      Set$Path1 <- subway_data[["2"]][c(Start_Ind_1:Total_Depart_Raw, 1:End_Ind_1), ]
    } else if (isTRUE(as.character(Set$Info$Line[1]) ==
                      2) & isTRUE(Set$Info$Count[1] == (Total_Depart_Raw - End_Ind_1 + Start_Ind_1))) {
      Set$Path1 <- subway_data[["2"]][c(Start_Ind_1:1, Total_Depart_Raw:End_Ind_1), ]
    }
    if (isTRUE(as.character(Set$Info$Line[2]) == 
               2) & isTRUE(Set$Info$Count[2] == (Total_Transfer_Raw - Start_Ind2_1 + End_Ind2_1))) {
      Set$Path2 <- subway_data[["2"]][c(Start_Ind2_1:Total_Transfer_Raw, 1:End_Ind2_1), ]
    } else if (isTRUE(as.character(Set$Info$Line[2]) == 
                      2) & isTRUE(Set$Info$Count[2] == (Total_Transfer_Raw - End_Ind2_1 + Start_Ind2_1))) {
      Set$Path2 <- subway_data[["2"]][c(Start_Ind2_1:1, Total_Transfer_Raw:End_Ind2_1), ]
    }
    if (isTRUE(as.character(Set$Info$Line[1]) == "6_A") & Start_Ind_1 >= End_Ind_1) {
      Set$Path1 <- subway_data[["6_A"]][c(Start_Ind_1:6, 1:End_Ind_1), ]
    }
    if (isTRUE(as.character(Set$Info$Line[2]) == "6_A") & Start_Ind_1 >= End_Ind_1) {
      Set$Path2 <- subway_data[["6_A"]][c(Start_Ind2_1:6, 1:End_Ind2_1), ]
    }
  }
  if (nrow(Set$Info) == 3) {
    Start_Ind_2 <- which(subway_data[[as.character(Set$Info$Line[1])]]$Name == 
                           as.character(Set$Info$Depart[1]))
    End_Ind_2 <- which(subway_data[[as.character(Set$Info$Line[1])]]$Name == 
                         as.character(Set$Info$Arrive[1]))
    Start_Ind2_2 <- which(subway_data[[as.character(Set$Info$Line[2])]]$Name == 
                            as.character(Set$Info$Depart[2]))
    End_Ind2_2 <- which(subway_data[[as.character(Set$Info$Line[2])]]$Name == 
                          as.character(Set$Info$Arrive[2]))
    Start_Ind3_2 <- which(subway_data[[as.character(Set$Info$Line[3])]]$Name == 
                            as.character(Set$Info$Depart[3]))
    End_Ind3_2 <- which(subway_data[[as.character(Set$Info$Line[3])]]$Name == 
                          as.character(Set$Info$Arrive[3]))
    Total_Depart_Raw <- nrow(subway_data[[as.character(Set$Info$Line[1])]])
    Total_Transfer_Raw <- nrow(subway_data[[as.character(Set$Info$Line[2])]])
    Total_End_Raw <- nrow(subway_data[[as.character(Set$Info$Line[3])]])
    Set$Path1 <- subway_data[[as.character(Set$Info$Line[1])]][Start_Ind_2:End_Ind_2, ]
    Set$Path2 <- subway_data[[as.character(Set$Info$Line[2])]][Start_Ind2_2:End_Ind2_2, ]
    Set$Path3 <- subway_data[[as.character(Set$Info$Line[3])]][Start_Ind3_2:End_Ind3_2, ]
    if (isTRUE(as.character(Set$Info$Line[1]) ==
               2) & isTRUE(Set$Info$Count[1] == (Total_Depart_Raw - Start_Ind_2 + End_Ind_2))) {
      Set$Path1 <- subway_data[["2"]][c(Start_Ind_2:Total_Depart_Raw, 
                                        1:End_Ind_2), ]
    } else if (isTRUE(Set$Info$Line[1] == 
                      2) & isTRUE(Set$Info$Count[1] == (Total_Depart_Raw - End_Ind_2 + Start_Ind_2))) {
      Set$Path1 <- subway_data[["2"]][c(Start_Ind_2:1,Total_Depart_Raw:End_Ind_2), ]
    }
    if (isTRUE(Set$Info$Line[2] == 
               2) & isTRUE(Set$Info$Count[2] == (Total_Transfer_Raw - Start_Ind2_2 + End_Ind2_2))) {
      Set$Path2 <- subway_data[["2"]][c(Start_Ind2_2:Total_Transfer_Raw, 1:End_Ind2_2), ]
    } else if (isTRUE(Set$Info$Line[2] == 
                      2) & isTRUE(Set$Info$Count[2] == (Total_Transfer_Raw - End_Ind2_2 + Start_Ind2_2))) {
      Set$Path2 <- subway_data[["2"]][c(Start_Ind2_2:1, Total_Transfer_Raw:End_Ind2_2), ]
    }
    if (isTRUE(Set$Info$Line[3] == 2) & isTRUE(Set$Info$Count[3] == 
                                                  (Total_End_Raw - Start_Ind3_2 + End_Ind3_2))) {
      Set$Path3 <- subway_data[["2"]][c(Start_Ind3_2:Total_End_Raw, 
                                        1:End_Ind3_2), ]
    } else if (isTRUE(Set$Info$Line[3] == 
                      2) & isTRUE(Set$Info$Count[3] == (Total_End_Raw - End_Ind3_2 + Start_Ind3_2))) {
      Set$Path3 <- subway_data[["2"]][c(Start_Ind3_2:1, Total_End_Raw:End_Ind3_2), ]
    }
    if (isTRUE(Set$Info$Line[1] == "6_A") & Start_Ind_2 > End_Ind_2) {
      Set$Path1 <- subway_data[["6_A"]][c(Start_Ind_2:6, 1:End_Ind_2), ]
    }
    if (isTRUE(Set$Info$Line[2] == "6_A") & Start_Ind2_2 > End_Ind2_2) {
      Set$Path2 <- subway_data[["6_A"]][c(Start_Ind2_2:6, 1:End_Ind2_2), ]
    }
    if (isTRUE(Set$Info$Line[3] == "6_A") & Start_Ind3_2 > End_Ind3_2) {
      Set$Path3 <- subway_data[["6_A"]][c(Start_Ind3_2:6, 1:End_Ind3_2), ]
    }
  }
  if (nrow(Set$Info) == 4) {
    Start_Ind_3 <- which(subway_data[[as.character(Set$Info$Line[1])]]$Name == 
                           as.character(Set$Info$Depart[1]))
    End_Ind_3 <- which(subway_data[[as.character(Set$Info$Line[1])]]$Name == 
                         as.character(Set$Info$Arrive[1]))
    Start_Ind2_3 <- which(subway_data[[as.character(Set$Info$Line[2])]]$Name == 
                            as.character(Set$Info$Depart[2]))
    End_Ind2_3 <- which(subway_data[[as.character(Set$Info$Line[2])]]$Name == 
                          as.character(Set$Info$Arrive[2]))
    Start_Ind3_3 <- which(subway_data[[as.character(Set$Info$Line[3])]]$Name == 
                            as.character(Set$Info$Depart[3]))
    End_Ind3_3 <- which(subway_data[[as.character(Set$Info$Line[3])]]$Name == 
                          as.character(Set$Info$Arrive[3]))
    Start_Ind4_3 <- which(subway_data[[as.character(Set$Info$Line[4])]]$Name == 
                            as.character(Set$Info$Depart[4]))
    End_Ind4_3 <- which(subway_data[[as.character(Set$Info$Line[4])]]$Name == 
                          as.character(Set$Info$Arrive[4]))
    Total_Depart_Raw <- nrow(subway_data[[as.character(Set$Info$Line[1])]])
    Total_Transfer1_Raw <- nrow(subway_data[[as.character(Set$Info$Line[2])]])
    Total_Transfer2_Raw <- nrow(subway_data[[as.character(Set$Info$Line[3])]])
    Total_End_Raw <- nrow(subway_data[[as.character(Set$Info$Line[4])]])
    Set$Path1 <- subway_data[[as.character(Set$Info$Line[1])]][Start_Ind_3:End_Ind_3, ]
    Set$Path2 <- subway_data[[as.character(Set$Info$Line[2])]][Start_Ind2_3:End_Ind2_3, ]
    Set$Path3 <- subway_data[[as.character(Set$Info$Line[3])]][Start_Ind3_3:End_Ind3_3, ]
    Set$Path4 <- subway_data[[as.character(Set$Info$Line[4])]][Start_Ind4_3:End_Ind4_3, ]
    if (isTRUE(Set$Info$Line[1] == 
               2) & isTRUE(Set$Info$Line[1] == (Total_Depart_Raw - Start_Ind_3 + End_Ind_3))) {
      Set$Path1 <- subway_data[["2"]][c(Start_Ind_3:Total_Depart_Raw, 1:End_Ind_3), ]
    } else if (isTRUE(Set$Info$Line[1] == 
                      2) & isTRUE(Set$Info$Line[1] == (Total_Depart_Raw - End_Ind_3 + Start_Ind_3))) {
      Set$Path1 <- subway_data[["2"]][c(Start_Ind_3:1, Total_Depart_Raw:End_Ind_3), ]
    }
    if (isTRUE(Set$Info$Line[2] == 
               2) & isTRUE(Set$Info$Count[2] == (Total_Transfer1_Raw - Start_Ind2_3 + End_Ind2_3))) {
      Set$Path2 <- subway_data[["2"]][c(Start_Ind2_3:Total_Transfer1_Raw, 1:End_Ind2_3), ]
    } else if (isTRUE(Set$Info$Line[2] == 
                      2) & isTRUE(Set$Info$Count[2] == (Total_Transfer1_Raw - End_Ind2_3 + Start_Ind2_3))) {
      Set$Path2 <- subway_data[["2"]][c(Start_Ind2_3:1, Total_Transfer1_Raw:End_Ind2_3), ]
    }
    if (isTRUE(Set$Info$Line[3] == 
               2) & isTRUE(Set$Info$Count[3] == (Total_Transfer2_Raw - Start_Ind3_3 + End_Ind3_3))) {
      Set$Path3 <- subway_data[["2"]][c(Start_Ind3_3:Total_Transfer2_Raw, 1:End_Ind3_3), ]
    } else if (isTRUE(Set$Info$Line[3] == 
                      2) & isTRUE(Set$Info$Count[3] == (Total_Transfer2_Raw - End_Ind3_3 + Start_Ind3_3))) {
      Set$Path3 <- subway_data[["2"]][c(Start_Ind3_3:1, Total_Transfer2_Raw:End_Ind3_3), ]
    }
    if (isTRUE(Set$Info$Line[3] == 
               2) & isTRUE(Set$Info$Count[4] == (Total_End_Raw - Start_Ind4_3 + End_Ind4_3))) {
      Set$Path4 <- subway_data[["2"]][c(Start_Ind3_3:Total_End_Raw, 1:End_Ind4_3), ]
    } else if (isTRUE(Set$Info$Line[4] == 
                      2) & isTRUE(Set$Info$Count[4] == (Total_End_Raw - End_Ind4_3 + Start_Ind4_3))) {
      Set$Path4 <- subway_data[["2"]][c(Start_Ind4_3:1,Total_End_Raw:End_Ind4_3), ]
    }
    if (isTRUE(Set$Info$Line[1] == "6_A") & Start_Ind_3 > End_Ind_3) {
      Set$Path1 <- subway_data[["6_A"]][c(Start_Ind_3:6, 1:End_Ind_3), ]
    }
    if (isTRUE(Set$Info$Line[2] == "6_A") & Start_Ind2_3 > End_Ind2_3) {
      Set$Path2 <- subway_data[["6_A"]][c(Start_Ind2_3:6, 1:End_Ind2_3), ]
    }
    if (isTRUE(Set$Info$Line[3] == "6_A") & Start_Ind3_3 > End_Ind3_3) {
      Set$Path3 <- subway_data[["6_A"]][c(Start_Ind3_3:6, 1:End_Ind3_3), ]
    }
    if (isTRUE(Set$Info$Line[4] == "6_A") & Start_Ind4_3 > End_Ind4_3) {
      Set$Path4 <- subway_data[["6_A"]][c(Start_Ind4_3:6, 1:End_Ind4_3), ]
    }
  }
  return(Set)
}


