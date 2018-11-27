# checkline

# 분기점의 경우 string이 잘못 선별하는 경우가 있어 참고하였습니다.

checkline <- function(dat) {
  anywrongdat <- which(str_detect(dat$Transfer, paste0(depart_line, "-")))
  if (isTRUE(length(anywrongdat) == 0) == FALSE) {
    dat <- dat[-anywrongdat, ]
  }
  anywrongdat2 <- which(str_detect(dat$Transfer, paste0(arrival_line, 
                                                        "-")))
  if (isTRUE(length(anywrongdat2) == 0) == FALSE) {
    dat <- dat[-anywrongdat2, ]
  }
  anywrongdat3 <- which(str_detect(dat$Transfer, paste0("K", depart_line)))
  if (isTRUE(length(anywrongdat3) == 0) == FALSE) {
    dat <- dat[-anywrongdat3, ]
  }
  anywrongdat4 <- which(str_detect(dat$Transfer, paste0("K", arrival_line)))
  if (isTRUE(length(anywrongdat4) == 0) == FALSE) {
    dat <- dat[-anywrongdat4, ]
  }
  dat
}

