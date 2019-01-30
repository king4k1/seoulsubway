# checkline will check the dataset for select accurate line 

checkline <- function(dat, depart_line, arrival_line) {
  data("subway_data", envir = environment())
  # in multi transfer case, we should consider every possible line 
  if (FALSE %in% (names(subway_data) %in% arrival_line)) {
    if (isTRUE(str_detect(depart_line, arrival_line) | str_detect(arrival_line, 
                                                                  depart_line)) == FALSE) {
      anywrongdat <- which(str_detect(dat$Transfer, paste0(depart_line, "_")))
      if (isTRUE(length(anywrongdat) == 0) == FALSE) {
        dat <- dat[-anywrongdat, ]
      }
      anywrongdat2 <- which(str_detect(dat$Transfer, paste0(arrival_line, "_")))
      if (isTRUE(length(anywrongdat2) == 0) == FALSE) {
        dat <- dat[-anywrongdat2, ]
      }
    }
    # for consider a case of branch line
    if (isTRUE(str_detect(depart_line, arrival_line) | str_detect(arrival_line, depart_line))) {
      anydat <- which(str_detect(dat$Transfer, fixed(paste0(depart_line, "|", arrival_line))))
      if (isTRUE(length(anydat) == 0) == FALSE) {
        dat <- dat[anydat, ]
      }
      anydat2 <- which(str_detect(dat$Transfer, fixed(paste0(arrival_line, "|", depart_line))))
      if (isTRUE(length(anydat2) == 0) == FALSE) {
        dat <- dat[anydat2, ]
      }
    }
  }else{
    anywrongdat <- which(str_detect(dat$Transfer, paste0(depart_line, "_")))
    if (isTRUE(length(anywrongdat) == 0) == FALSE) {
      dat <- dat[-anywrongdat, ]
    }
  }
  return(dat)
}
