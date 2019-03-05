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
  transfer_long <- get_transfercriteria(depart, arrival, penalty = 0.05)
  # set criteria for available transfer station
  if (transfer_count == 1) {
    transfer_depart <- transfer_long[str_which(transfer_long$Transfer, 
                                               fixed(depart_line)), ]
    transfer_arrival <- transfer_depart[str_which(transfer_depart$Transfer, 
                                                  arrival_line), ]
    transfer_arrival <- checkline(dat = transfer_arrival, depart_line = depart_line, 
                                  arrival_line = arrival_line)
    # find available transfer station(depart, arrival both)
    transfer_arrival <- transfer_arrival[which(transfer_arrival$Name %in% 
                                                 subway_data[[arrival_line]]$Name), ]
  }
  if (transfer_count == 2) {
    transfer_arrival <- list()
    transfer_middle <- transfer_long[str_which(transfer_long$Transfer, 
                                               fixed(depart_line)), ]
    # set available transfer station for waypoint.(== first transfer
    # station)
    transfer_middle <- checkline(dat = transfer_middle, depart_line = depart_line, 
                                 arrival_line = names(subway_data))
    for (i in 1:nrow(transfer_middle)) {
      transfer_middle_list_sub <- str_remove(transfer_middle$Transfer[i], 
                                             fixed(depart_line))
      transfer_middle_list_sub <- unlist(strsplit(transfer_middle_list_sub, 
                                                  split = "|", fixed = TRUE))
      ind_null <- which(transfer_middle_list_sub == "")
      transfer_middle_list <- transfer_middle_list_sub
      if (isTRUE(length(ind_null) == 0) == FALSE) {
        transfer_middle_list <- transfer_middle_list_sub[-ind_null]
      }
      for (j in seq_along(transfer_middle_list)) {
        transfer_long <- get_transfercriteria(transfer_middle[i, "Name"], arrival, penalty = 0.05)
        # set criteria for available transfer station
        transfer_middle_get <- transfer_long[str_which(transfer_long$Transfer, 
                                                       fixed(transfer_middle_list[j])), ]
        transfer_middle_get <- transfer_middle_get[str_which(transfer_middle_get$Transfer, 
                                                             fixed(arrival_line)), ]
        transfer_middle_get <- transfer_middle_get[which(transfer_middle_get$Name %in% 
                                                           subway_data[[transfer_middle_list[j]]]$Name), ]
        transfer_middle_get <- transfer_middle_get[which(transfer_middle_get$Name %in% 
                                                           subway_data[[arrival_line]]$Name), ]
        # find available transfer station(depart, arrival both)
        transfer_middle_get <- checkline(transfer_middle_get, depart_line = transfer_middle_list[j], 
                                         arrival_line = arrival_line)
        if(nrow(transfer_middle_get)==0){
          transfer_long <-get_transfercriteria(transfer_middle[i, "Name"], arrival,
                                               penalty = 0.1)
          transfer_middle_get <- transfer_long[str_which(transfer_long$Transfer, 
                                                         fixed(transfer_middle_list[j])), ]
          transfer_middle_get <- transfer_middle_get[str_which(transfer_middle_get$Transfer, 
                                                               fixed(arrival_line)), ]
          transfer_middle_get <- transfer_middle_get[which(transfer_middle_get$Name %in% 
                                                             subway_data[[transfer_middle_list[j]]]$Name), ]
          transfer_middle_get <- transfer_middle_get[which(transfer_middle_get$Name %in%
                                                             subway_data[[arrival_line]]$Name), ]
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
      cut_na[k] <- is.na(transfer_arrival[[k]]$second[1, 1])
      cut_dup[k] <- isTRUE(transfer_arrival[[k]]$first[1, 1] == transfer_arrival[[k]]$second[1, 1])
    }
    # if second transfer station is null(no result) ==> remove one
    cut_ind <- which(cut_na)
    cut_list <- names(transfer_arrival)[cut_ind]
    cut_ind <- which(cut_dup)
    cut_dup <- names(transfer_arrival)[cut_ind]
    for (l in seq_along(cut_list)) {
      transfer_arrival[[cut_list[l]]] <- NULL
    }
    for (l in seq_along(cut_dup)) {
      transfer_arrival[[cut_dup[l]]] <- NULL
    }
  }
  if (transfer_count == 3) {
    transfer_arrival <- list()
    transfer_middle_first <- transfer_long[str_which(transfer_long$Transfer, 
                                                     fixed(depart_line)), ]
    
    # set available transfer station for waypoint.
    # (== first transfer station)
    transfer_middle_first <- checkline(dat = transfer_middle_first, 
                                       depart_line = depart_line, arrival_line = names(subway_data))
    for (i in 1:nrow(transfer_middle_first)) {
      transfer_middle_list_sub <- str_remove(transfer_middle_first$Transfer[i], 
                                             fixed(depart_line))
      transfer_middle_list_sub <- unlist(strsplit(transfer_middle_list_sub, 
                                                  split = "|", fixed = TRUE))
      ind_null <- which(transfer_middle_list_sub == "")
      transfer_middle_list <- transfer_middle_list_sub
      if (isTRUE(length(ind_null) == 0) == FALSE) {
        transfer_middle_list <- transfer_middle_list_sub[-ind_null]
      }
      for (j in seq_along(transfer_middle_list)) {
        transfer_long <- get_transfercriteria(transfer_middle_first[i, "Name"],
                                              arrival, penalty = 0.05)
        transfer_middle_second <- transfer_long[str_which(transfer_long$Transfer, 
                                                          fixed(transfer_middle_list[j])), ]
        
        transfer_middle_second <- checkline(dat = transfer_middle_second, 
                                            depart_line = transfer_middle_list[j], 
                                            arrival_line = names(subway_data))
        for (j2 in 1:nrow(transfer_middle_second)) {
          transfer_middle2_list_sub <- str_remove(transfer_middle_second$Transfer[j2], 
                                                  fixed(transfer_middle_list[j]))
          transfer_middle2_list_sub <- unlist(strsplit(transfer_middle2_list_sub, 
                                                       split = "|", fixed = TRUE))
          ind_null <- which(transfer_middle2_list_sub == "")
          transfer_middle2_list <- transfer_middle2_list_sub
          if (isTRUE(length(ind_null) == 0) == FALSE) {
            transfer_middle2_list <- transfer_middle2_list_sub[-ind_null]
          }
          for (k in seq_along(transfer_middle2_list)) {
            transfer_long <- get_transfercriteria(transfer_middle_second[j2, "Name"], 
                                                  arrival, penalty = 0.05)
            # set criteria wider for available transfer station(branch line)
            transfer_middle_get <- transfer_long[str_which(transfer_long$Transfer, 
                                                           fixed(transfer_middle2_list[j])), ]
            transfer_middle_get <- transfer_middle_get[str_which(transfer_middle_get$Transfer, 
                                                                 fixed(arrival_line)), ]
            transfer_middle_get <- transfer_middle_get[which(transfer_middle_get$Name %in% 
                                                               subway_data[[transfer_middle2_list[j]]]$Name), ]
            transfer_middle_get <- transfer_middle_get[which(transfer_middle_get$Name %in% 
                                                               subway_data[[arrival_line]]$Name), ]
            if(nrow(transfer_middle_get)==0){
              transfer_long <- get_transfercriteria(transfer_middle_second[j2, "Name"], 
                                                    arrival, penalty = 0.1)
              transfer_middle_get <- transfer_long[str_which(transfer_long$Transfer, 
                                                             fixed(transfer_middle2_list[j])), ]
              transfer_middle_get <- transfer_middle_get[str_which(transfer_middle_get$Transfer, 
                                                                   fixed(arrival_line)), ]
              transfer_middle_get <- transfer_middle_get[which(transfer_middle_get$Name %in% 
                                                                 subway_data[[transfer_middle2_list[j]]]$Name), ]
              transfer_middle_get <- transfer_middle_get[which(transfer_middle_get$Name %in% 
                                                                 subway_data[[arrival_line]]$Name), ]
            }
            # find available transfer station(depart, arrival both)
            transfer_middle_get <- checkline(transfer_middle_get, 
                                             depart_line = transfer_middle2_list[k], 
                                             arrival_line = arrival_line)
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
    cut_ind <- which(cut_na)
    cut_list <- names(transfer_arrival)[cut_ind]
    cut_ind <- which(cut_dup)
    cut_dup <- names(transfer_arrival)[cut_ind]
    cut_ind <- which(cut_dup2)
    cut_dup2 <- names(transfer_arrival)[cut_ind]
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


