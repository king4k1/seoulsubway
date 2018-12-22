# get_total_count

get_total_count <- function(dat, depart_name, depart_line_name, arrival_name, arrival_line_name){
  data("subway_data_DT")
  data("subway_route")
  data("seoul_route")
  seoul_station <- subway_data_DT$Name
  seoul_station <- sort(unique(seoul_station))
  subway_make_list <- data.frame(matrix(0,1,460))
  colnames(subway_make_list) <- seoul_station
  depart <- dat[,depart_name]
  depart_line <- dat[,depart_line_name]
  arrival <- dat[,arrival_name]
  arrival_line <- dat[,arrival_line_name]
  for(i in 1:nrow(dat)){
    key <- paste0(depart[i], "(", depart_line[i], ")", "-", 
                  arrival[i], "(", arrival_line[i], ")")
    key_2 <- paste0(arrival[i], "(", arrival_line[i], ")", "-",
                    depart[i], "(", depart_line[i], ")")
    result <- subway_route[[key]]
    if(is.null(result)){
      result <- subway_route[[key_2]]  
    }
    subway_make_list[result$ind] <- subway_make_list[result$ind] + 1
  }
  return(subway_make_list)
}
