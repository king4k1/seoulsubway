## get handled shortestpath result in data base
## The data base was based on 2018/12 seoul subway network.
## you can get shortestpath which has from-to format.
## also, spending Time and Count.

get_shortestpath <- function(depart, arrival){
  data(subway_route_1812_handle)
  result <- subway_route_1812_handle[[paste0(depart, "-", arrival)]]
  if(is.null(result)){
    result_inv <- subway_route_1812_handle[[paste0(arrival, "-", depart)]]
    ord_f <- result_inv$Path$from
    result_inv$Path$from <- result_inv$Path$to
    result_inv$Path$to <- ord_f
    dat_inv_change <- result_inv$Path
    for(i in 1:nrow(dat_inv_change)){
      dat_inv_change[i,] <- result_inv$Path[(nrow(result_inv$Path)+1-i),]
    }
    result_inv$Path <- dat_inv_change
  }
  result_inv
}