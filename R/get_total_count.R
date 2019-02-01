# get_total_count

get_total_count <- function(dat, depart_name, arrival_name){
  data("subway_route")
  data("seoul_station")
  total <- data.frame(matrix(0,1,468))
  colnames(total) <- seoul_station
  depart <- dat[,depart_name]
  arrival <- dat[,arrival_name]
  time.started <- Sys.time()
  cat(paste('Started at : ', time.started, ' / ...ing...', sep = ''))
  
  for(i in 1:nrow(dat)){
    get <- paste0(depart[i], "-", arrival[i])
    get_2 <- paste0(arrival[i], "-", depart[i])
    result <- subway_route[[get]]
    if(is.null(result)){
      result <- subway_route[[get_2]]  
    }
    total[which(seoul_station%in%result$station)] <- 
      total[which(seoul_station%in%result$station)] + 1
  }
  total_gather <- total%>%gather(key = "station", value = "count")
  time.finished <- Sys.time() # Store finished time
  time.elapsed  <- time.finished - time.started # Calculate elapsed time
  cat(paste('Finished..! / elapsed time : ', time.elapsed, '\n\n', sep = ''))
  return(total_gather)
}
