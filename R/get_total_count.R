# get_total_count

get_total_count <- function(dat, depart_name, depart_line_name, arrival_name, arrival_line_name){
  data("subway_data_DT")
  data("subway_route")
  data("seoul_route")
  seoul_station <- subway_data_DT$Name
  seoul_station <- sort(unique(seoul_station))
  total <- data.frame(matrix(0,1,460))
  colnames(total) <- seoul_station
  depart <- dat[,depart_name]
  depart_line <- dat[,depart_line_name]
  arrival <- dat[,arrival_name]
  arrival_line <- dat[,arrival_line_name]
  for(i in 1:nrow(dat)){
    get <- paste0(depart[i], "(", depart_line[i], ")", "-", 
                  arrival[i], "(", arrival_line[i], ")")
    get_2 <- paste0(arrival[i], "(", arrival_line[i], ")", "-",
                    depart[i], "(", depart_line[i], ")")
    result <- subway_route[[get]]
    if(is.null(result)){
      result <- subway_route[[get_2]]  
    }
    total[result$ind] <- total[result$ind] + 1
  }
  return(totals)
}
