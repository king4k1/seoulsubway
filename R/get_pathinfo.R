# 보조함수(get_pathinfo)

# 하나의 노선에 대하여 총 역수(total), ind1(역 1 위치순서), ind2(역 2 위치순서) 그리고 line(노선)을 입력하면

# 두 역 사이의 이동역수와 소요시간을 산출한다.

get_pathinfo <- function(total, ind1, ind2, line) {
  data(subway_data, envir = environment())
  data(transfer_info, envir = environment())
  # load data
  if (line == "2") {
    Circulate <- total - abs(ind1 - ind2)
    Circulate2 <- abs(ind1 - ind2)
    Circulate <- c(Circulate, Circulate2)
    Circulate_Time <- c(sum(subway_data[[line]][c(ind1:total, 1:ind2), 
                                                "Time"]) - subway_data[[line]][ind1, "Time"], sum(subway_data[[line]][c(ind2:total, 
                                                                                                                        1:ind1), "Time"]) - subway_data[[line]][ind1, "Time"])
    Circulate_Time <- Circulate_Time[which.min(Circulate_Time)]
    Circulate2_Time <- sum(subway_data[[line]][ind1:ind2, "Time"]) - 
      subway_data[[line]][ind1, "Time"]
    Circulate_Time <- c(Circulate_Time, Circulate2_Time)
    Path_Count <- Circulate[which.min(Circulate_Time)]
    Path_Time <- Circulate_Time[which.min(Circulate_Time)]
    # consider circulate line(line number 2) => use absolute value & total
    # - abs
  } else {
    Path_Count <- abs(ind1 - ind2)
    Path_Time <- sum(subway_data[[line]][ind1:ind2, "Time"]) - subway_data[[line]][ind1, 
                                                                                   "Time"]
    # (normal case) get index.
  }
  return(data.frame(count = as.numeric(Path_Count), time = as.numeric(Path_Time)))
}

