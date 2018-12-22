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
  depart_info <- subway_data_DT[which(subway_data_DT$Name == depart)[1], ]
  arrival_info <- subway_data_DT[which(subway_data_DT$Name == arrival)[1], ]
  # get criteria from depart/arrival's longitude&lattitude
  lat_criteria <- c(depart_info$lat, arrival_info$lat)
  lat_lowerbound <- as.numeric(lat_criteria[which.min(lat_criteria)]) - penalty
  lat_upperbound <- as.numeric(lat_criteria[which.max(lat_criteria)]) + penalty
  long_criteria <- c(depart_info$long, arrival_info$long)
  long_lowerbound <- as.numeric(long_criteria[which.min(long_criteria)]) - penalty
  long_upperbound <- as.numeric(long_criteria[which.max(long_criteria)]) + penalty
  # give penalty for bypass selection
  transfer_lat <- transfer_station[which(transfer_station$lat <= lat_upperbound), ]
  transfer_lat <- transfer_lat[which(transfer_lat$lat >= lat_lowerbound), ]
  transfer_long <- transfer_lat[which(transfer_lat$long <= long_upperbound), ]
  transfer_long <- transfer_long[which(transfer_long$long >= long_lowerbound), ]
  return(transfer_long)
}

