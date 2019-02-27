### if shortestpath_3 does not work than, shortestpath_4 will work 
### for 3 nearest transfer available stations
### it depend on shortestpath_3..

shortestpath_4 <- function(depart, depart_line, arrival, arrival_line){
  data("subway_data")
  arr_line <- subway_data[[arrival_line]]
  index <- which(arr_line$Name==arrival)
  tr_arr_line <- which(arr_line$Transfer != 0)
  nn_rank <- rank(abs(index - tr_arr_line))
  nn_list <- tr_arr_line[nn_rank <= 3]
  nn_list_nm <- arr_line$Name[nn_list]
  nn_list_line <- arr_line$Transfer[nn_list]
  result <- result2 <- result_which <- list()
  for(i in seq_along(nn_list_nm)){
    multi_linecase <- nn_list_line[i]
    multi_linecase <- str_split(multi_linecase, pattern = "|")[[1]]
    multi_linecase <- multi_linecase[multi_linecase!=""]
    multi_linecase <- multi_linecase[multi_linecase!="|"]
    for(j in seq_along(multi_linecase))
    result[[paste0(i,"-",j)]] <- tryCatch(shortestpath_3(depart, depart_line,
                                 arrival= nn_list_nm[i],
                                 arrival_line= multi_linecase[j]),
                                 error = function(e) {
                                   result[[paste0(i,"-",j)]] = list(Time = 300)})
    result_which[[paste0(i,"-",j)]] <- result[[paste0(i,"-",j)]]$Time
    result2[[paste0(i,"-",j)]] <- tryCatch(shortestpath_1(depart= nn_list_nm[i],
                                  depart_line= multi_linecase[j],
                                  arrival, arrival_line),
                                  error = function(e){
                                  })
  }
  shortest_index <- names(result_which)[which.min(result_which)]
  result[[shortest_index]]$Info <- rbind(result[[shortest_index]]$Info, 
                                         result2[[shortest_index]]$Info[2,])
  result[[shortest_index]]$Count <-
    result[[shortest_index]]$Count + result2[[shortest_index]]$Count 
  result[[shortest_index]]$Time <-
    result[[shortest_index]]$Time + result2[[shortest_index]]$Time 
  result[[shortest_index]]$Path4 <- result2[[shortest_index]]$Path2
  result <- result[[shortest_index]]
  if(is.null(result)){      
    message("If the station and the line do not match, the route can not be obtained.
also you should consider branch line '1_P','1_A','1_B','2_A','2_B','5_A','6_A' and 'K_A'
* Guro to Shin Chang Station : '1_P'
* Geumcheon-gu Office to Kwang Myung Station : '1_A' 
* Byeongjeom to Seodongtan Station : '1_B'
* Seongsu to Sinseol-dong Station : '2_A' \n* Sindorim to Kkachisan Station : '2_B'
* Gangdong to Macheon Station : '5_A' \n* Eungam to Gusan(also include Eungam) Station : '6_A'
* Gajwa to Seoul Station : 'K_A'")
  }
  return(result)
}
