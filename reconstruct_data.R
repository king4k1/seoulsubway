### reconstruct_data for subway using log data

### setting line & station name(remove sub-name)


reconstruct_data <- function(raw_data, colname_list=c("depart","depart_line",
                                                  "arrival","arrival_line")){
  ### raw_data의 특성상 크기가 크므로, data.table 패키지를 이용
  dep_name <- raw_data[,colname_list[1], with=FALSE]
  dep_line <- raw_data[,colname_list[2], with=FALSE]
  arr_name <- raw_data[,colname_list[3], with=FALSE]
  arr_line <- raw_data[,colname_list[4], with=FALSE]
  dep_name_index <- which(str_detect(dep_name, "[(]"))
  dep_name_index_nm <- str_locate(dep_name[dep_name_index], "[(]")
  dep_name[dep_name_index] <- str_sub(dep_name[dep_name_index], 1, 
           dep_name_index_nm[, 1] - 1)
  arr_name_index <- which(str_detect(arr_name, "[(]"))
  arr_name_index_nm <- str_locate(arr_name[arr_name_index], "[(]")
  arr_name[arr_name_index] <- str_sub(arr_name[arr_name_index], 1, 
           arr_name_index_nm[, 1] - 1)  
  arr_name <- str_replace(arr_name, "서울역", "서울")
  dep_name <- str_replace(dep_name, "서울역", "서울")
  arr_name <- str_replace(arr_name, "이수", "총신대입구")
  dep_name <- str_replace(dep_name, "이수", "총신대입구")
  dep_line <- str_sub(dep_line, 1, 1) # ex. "1호선" -> "1"
  arr_line <- str_sub(arr_line, 1, 1) # ex. "1호선" -> "1"
  ind_2_A_up <- which(dep_name%in%c("용답", "신답", "용두","신설동") & 
                        dep_line%in%c("2"))
  ind_2_B_up <- which(dep_name%in%c("도림천", "양천구청", "신정네거리") &
                        dep_line%in%c("2"))
  ind_5_A_up <- which(dep_name%in%c("둔촌동","올림픽공원", "방이","개롱","거여","마천","오금") & 
                        dep_line%in%c("5"))
  ind_6_A_up <- which(dep_name%in%c("연신내", "불광", "역촌", "구산", "독바위", "응암") & 
                        dep_line%in%c("6"))
  ind_2_A_down <- 
    which(arr_name%in%c("용답", "신답", "용두","신설동")& arr_line%in%c("2"))
  ind_2_B_down <- 
    which(arr_name%in%c("도림천", "양천구청", "신정네거리")& arr_line%in%c("2"))
  ind_5_A_down <- 
    which(arr_name%in%c("둔촌동","올림픽공원", "방이","개롱","거여","마천","오금")& 
            arr_line%in%c("5"))
  ind_6_A_down <- 
    which(arr_name%in%c("연신내", "불광", "역촌", "구산", "독바위", "응암")& 
            arr_line%in%c("6"))
  raw_data[,"depart", with=FALSE] <- dep_name
  raw_data[,"depart_line", with=FALSE] <- dep_line
  raw_data[,"arrival", with=FALSE] <- arr_name
  raw_data[,"arrival_line", with=FALSE] <- arr_lin
  raw_data[ind_2_A_up,4, "depart_line", with=FALSE] <- "2_A"
  raw_data[ind_2_B_up,4, "depart_line", with=FALSE] <- "2_B"
  raw_data[ind_5_A_up,4, "depart_line", with=FALSE] <- "5_A"
  raw_data[ind_6_A_up,4, "depart_line", with=FALSE] <- "6_A"
  raw_data[ind_2_A_down,7, "arrival_line", with=FALSE] <- "2_A"
  raw_data[ind_2_B_down,7, "arrival_line", with=FALSE] <- "2_B"
  raw_data[ind_5_A_down,7, "arrival_line", with=FALSE] <- "5_A"
  raw_data[ind_6_A_down,7, "arrival_line", with=FALSE] <- "6_A"
  return(raw_data)
}
