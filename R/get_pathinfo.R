# 보조함수(get_pathinfo)

# 하나의 노선에 대하여 총 역수(total), start(역 1 위치순서), end(역 2 위치순서) 그리고 line(노선)을 입력하면

# 두 역 사이의 이동역수와 소요시간을 산출한다.

get_pathinfo <- function(total, start, end, line) {
  data(subway_data, envir = environment())
  if(is.null(start)|is.null(end)){
    stop("you can`t get a path from these transfer count number")
  }
  Path_Count <- abs(start - end)
  Path_Time <- sum(subway_data[[line]][start:end, "Time"]) - subway_data[[line]][start, "Time"]
  # (normal case) get index.
  if (line == "2") {
    Circulate <- total - abs(start - end)
    Circulate2 <- abs(start - end)
    Circulate <- c(Circulate, Circulate2)
    Circulate_Time <- c(sum(subway_data[[line]][c(start:total,
                        1:end), "Time"]) - subway_data[[line]][start, "Time"], sum(subway_data[[line]][c(end:total,
                        1:start), "Time"]) - subway_data[[line]][start, "Time"])
    Circulate_Time <- Circulate_Time[which.min(Circulate_Time)]
    Circulate2_Time <- sum(subway_data[[line]][start:end, "Time"]) - 
      subway_data[[line]][start, "Time"]
    Circulate_Time <- c(Circulate_Time, Circulate2_Time)
    Path_Count <- Circulate[which.min(Circulate_Time)]
    Path_Time <- Circulate_Time[which.min(Circulate_Time)]
    # consider circulate line(line number 2) => use absolute value & total - abs
  }
  if (line == "6_A" & start > end) {
      Path_Count <- 6 + end - start 
      Path_Time <- sum(subway_data[[line]][c(end:6,
                       1:start), "Time"]) - subway_data[[line]][start, "Time"] + 2 
      # 6-A -> 6-A circulate line.
  }
  return(data.frame(count = as.numeric(Path_Count), time = as.numeric(Path_Time)))
}

