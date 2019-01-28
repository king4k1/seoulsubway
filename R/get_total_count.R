# get_total_count

get_total_count <- function(dat, depart_name, depart_line_name, arrival_name, arrival_line_name){
  data("subway_data_DT")
  data("subway_route")
  data("seoul_route")
  seoul_station <- subway_data_DT$Name
  seoul_station <- sort(unique(seoul_station))
  total <- data.frame(matrix(0,1,468))
  colnames(total) <- seoul_station
  depart <- dat[,depart_name]
  depart_line <- dat[,depart_line_name]
  arrival <- dat[,arrival_name]
  arrival_line <- dat[,arrival_line_name]
  
  time.started <- Sys.time()
  cat(paste('Started at : ', time.started, ' / ...ing...', sep = ''))
  
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
  
  total_gather <- total%>%gather(key = "station", value = "count")
  time.finished <- Sys.time() # Store finished time
  time.elapsed  <- time.finished - time.started # Calculate elapsed time
  cat(paste('Finished..! / elapsed time : ', time.elapsed, '\n\n', sep = ''))
  return(total_gather)
}
