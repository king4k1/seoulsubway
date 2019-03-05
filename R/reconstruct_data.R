### reconstruct_data for subway using log data

### setting line & station name(remove sub-name)


reconstruct_data <- function(raw_data, colname_list=c("depart","depart_line",
                                                  "arrival","arrival_line")){
  up_Line <- raw_data[,colname_list[2]]
  down_Line <- raw_data[, colname_list[4]]
  up_Name <- raw_data[,colname_list[1]]
  up_Name_index <- which(str_detect(up_Name, "[(]"))
  up_Name_index_nm <- str_locate(up_Name[up_Name_index], "[(]")
  up_Name[up_Name_index] <- substr(up_Name[up_Name_index], 1, 
           up_Name_index_nm[, 1] - 1)
  down_Name <- raw_data[,colname_list[3]]
  down_Name_index <- which(str_detect(down_Name, "[(]"))
  down_Name_index_nm <- str_locate(down_Name[down_Name_index], "[(]")
  down_Name[down_Name_index] <- substr(down_Name[down_Name_index], 1, 
           down_Name_index_nm[, 1] - 1)  
  down_Name <- str_replace(down_Name, "서울역", "서울")
  up_Name <- str_replace(up_Name, "서울역", "서울")
  down_Name <- str_replace(down_Name, "이수", "총신대입구")
  up_Name <- str_replace(up_Name, "이수", "총신대입구")
  
  up_Line <- str_sub(up_Line, 1, 1) # ex. "1호선" -> "1"
  down_Line <- str_sub(down_Line, 1, 1) # ex. "1호선" -> "1"

  ind_2_A_up <- 
    which(up_Name%in%c("용답", "신답", "용두","신설동")& 
            up_Line%in%c("2"))
  ind_2_B_up <- 
    which(up_Name%in%c("도림천", "양천구청", "신정네거리")& 
            up_Line%in%c("2"))
  ind_5_A_up <- 
    which(up_Name%in%c("둔촌동","올림픽공원", "방이","개롱","거여","마천","오금")& 
            up_Line%in%c("5"))
  ind_6_A_up <- 
    which(up_Name%in%c("연신내", "불광", "역촌", "구산", "독바위", "응암")& 
            up_Line%in%c("6"))

  ind_2_A_down <- 
    which(down_Name%in%c("용답", "신답", "용두","신설동")& 
            down_Line%in%c("2"))
  ind_2_B_down <- 
    which(down_Name%in%c("도림천", "양천구청", "신정네거리")& 
            down_Line%in%c("2"))
  ind_5_A_down <- 
    which(down_Name%in%c("둔촌동","올림픽공원", "방이","개롱","거여","마천","오금")& 
            down_Line%in%c("5"))
  ind_6_A_down <- 
    which(down_Name%in%c("연신내", "불광", "역촌", "구산", "독바위", "응암")& 
            down_Line%in%c("6"))
  raw_data[,colname_list[1]] <- up_Name
  raw_data[,colname_list[2]] <- up_Line
  raw_data[,colname_list[3]] <- down_Name
  raw_data[,colname_list[4]] <- down_Line
    
  raw_data[ind_2_A_up,colname_list[2]] <- "2_A"
  raw_data[ind_2_B_up,colname_list[2]] <- "2_B"
  raw_data[ind_5_A_up,colname_list[2]] <- "5_A"
  raw_data[ind_6_A_up,colname_list[2]] <- "6_A"
  raw_data[ind_2_A_down,colname_list[4]] <- "2_A"
  raw_data[ind_2_B_down,colname_list[4]] <- "2_B"
  raw_data[ind_5_A_down,colname_list[4]] <- "5_A"
  raw_data[ind_6_A_down,colname_list[4]] <- "6_A"

  return(raw_data)
}
