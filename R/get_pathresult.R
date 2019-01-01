# which makes the shortestpath route to see easily
# considering circulate line 2, 6-A
# which contains case that using transfer 1,2,3

get_pathresult <- function(shortestpath_result) {
  data("subway_data", envir = environment())
  Set <- list(Info = shortestpath_result$Info, 
              Count = as.numeric(shortestpath_result$Total["Count"]), 
              Time = as.numeric(shortestpath_result$Total["Time"]))
  if (nrow(Set$Info) == 1) {
    Start_Ind_0 <- which(subway_data[[as.character(Set$Info[1, "Line"])]]$Name == 
                           as.character(Set$Info[1, "Depart"]))
    End_Ind_0 <- which(subway_data[[as.character(Set$Info[1, "Line"])]]$Name == 
                           as.character(Set$Info[1, "Arrive"]))
    Total_Depart_Raw <- nrow(subway_data[[as.character(Set$Info[1, 
                                                                "Line"])]])
    Set$Path <- subway_data[[as.character(Set$Info[1, "Line"])]][Start_Ind_0:End_Ind_0, ]
    if (isTRUE(Set$Info[1, "Line"] == 2) & isTRUE(Set$Info[1, "Count"] == 
                                          (Total_Depart_Raw - Start_Ind_0 + End_Ind_0))) {
      Set$Path <- subway_data[["2"]][c(Start_Ind_0:Total_Depart_Raw, 1:End_Ind_0), ]
    } else if (isTRUE(Set$Info[1, "Line"] == 2) & isTRUE(Set$Info[1, "Count"] == 
                                                 (Total_Depart_Raw - End_Ind_0 + Start_Ind_0))) {
      Set$Path <- subway_data[["2"]][c(End_Ind_0:Total_Depart_Raw, 1:Start_Ind_0), ]
    }
    if (isTRUE(Set$Info[1, "Line"] == "6-A") & Start_Ind_0 > End_Ind_0) {
      Set$Path <- subway_data[["6-A"]][c(Start_Ind_0 :6, 1:End_Ind_0), ]
    }
  }
  if (nrow(Set$Info) == 2) {
    Start_Ind_1 <- which(subway_data[[as.character(Set$Info[1, "Line"])]]$Name == 
                           as.character(Set$Info[1, "Depart"]))
    End_Ind_1 <- which(subway_data[[as.character(Set$Info[1, "Line"])]]$Name == 
                         as.character(Set$Info[1, "Arrive"]))
    Start_Ind2_1 <- which(subway_data[[as.character(Set$Info[2, "Line"])]]$Name == 
                            as.character(Set$Info[2, "Depart"]))
    End_Ind2_1 <- which(subway_data[[as.character(Set$Info[2, "Line"])]]$Name == 
                          as.character(Set$Info[2, "Arrive"]))
    Total_Depart_Raw <- nrow(subway_data[[as.character(Set$Info[1, "Line"])]])
    Total_Transfer_Raw <- nrow(subway_data[[as.character(Set$Info[2, "Line"])]])
    Set$Path1 <- subway_data[[as.character(Set$Info[1, "Line"])]][Start_Ind_1:End_Ind_1, ]
    Set$Path2 <- subway_data[[as.character(Set$Info[2, "Line"])]][Start_Ind2_1:End_Ind2_1, ]
    # default setting for normal case;
    if (isTRUE(as.character(Set$Info[1, "Line"]) ==
               2) & isTRUE(Set$Info[1, "Count"] == (Total_Depart_Raw - Start_Ind_1 + End_Ind_1))) {
      Set$Path1 <- subway_data[["2"]][c(Start_Ind_1:Total_Depart_Raw, 1:End_Ind_1), ]
    } else if (isTRUE(as.character(Set$Info[1, "Line"]) ==
                      2) & isTRUE(Set$Info[1, "Count"] == (Total_Depart_Raw - End_Ind_1 + Start_Ind_1))) {
      Set$Path1 <- subway_data[["2"]][c(End_Ind_1:Total_Depart_Raw, 1:Start_Ind_1), ]
    }
    if (isTRUE(as.character(Set$Info[2, "Line"]) == 
               2) & isTRUE(Set$Info[2, "Count"] == (Total_Transfer_Raw - Start_Ind2_1 + End_Ind2_1))) {
      Set$Path2 <- subway_data[["2"]][c(Start_Ind2_1:Total_Transfer_Raw, 1:End_Ind2_1), ]
    } else if (isTRUE(as.character(Set$Info[2, "Line"]) == 
                      2) & isTRUE(Set$Info[2, "Count"] == (Total_Transfer_Raw - End_Ind2_1 + Start_Ind2_1))) {
      Set$Path2 <- subway_data[["2"]][c(End_Ind2_1:Total_Transfer_Raw, 1:Start_Ind2_1), ]
    }
    if (isTRUE(as.character(Set$Info[1, "Line"]) == "6-A") & Start_Ind_1 >= End_Ind_1) {
      Set$Path1 <- subway_data[["6-A"]][c(Start_Ind_1:6, 1:End_Ind_1), ]
    }
    if (isTRUE(as.character(Set$Info[2, "Line"]) == "6-A") & Start_Ind_1 >= End_Ind_1) {
      Set$Path2 <- subway_data[["6-A"]][c(Start_Ind2_1:6, 1:End_Ind2_1), ]
    }
  }
  if (nrow(Set$Info) == 3) {
    Start_Ind_2 <- which(subway_data[[as.character(Set$Info[1, "Line"])]]$Name == 
                           as.character(Set$Info[1, "Depart"]))
    End_Ind_2 <- which(subway_data[[as.character(Set$Info[1, "Line"])]]$Name == 
                         as.character(Set$Info[1, "Arrive"]))
    Start_Ind2_2 <- which(subway_data[[as.character(Set$Info[2, "Line"])]]$Name == 
                            as.character(Set$Info[2, "Depart"]))
    End_Ind2_2 <- which(subway_data[[as.character(Set$Info[2, "Line"])]]$Name == 
                          as.character(Set$Info[2, "Arrive"]))
    Start_Ind3_2 <- which(subway_data[[as.character(Set$Info[3, "Line"])]]$Name == 
                            as.character(Set$Info[3, "Depart"]))
    End_Ind3_2 <- which(subway_data[[as.character(Set$Info[3, "Line"])]]$Name == 
                          as.character(Set$Info[3, "Arrive"]))
    Total_Depart_Raw <- nrow(subway_data[[as.character(Set$Info[1, "Line"])]])
    Total_Transfer_Raw <- nrow(subway_data[[as.character(Set$Info[2, "Line"])]])
    Total_End_Raw <- nrow(subway_data[[as.character(Set$Info[3, "Line"])]])
    Set$Path1 <- subway_data[[as.character(Set$Info[1, "Line"])]][Start_Ind_2:End_Ind_2, ]
    Set$Path2 <- subway_data[[as.character(Set$Info[2, "Line"])]][Start_Ind2_2:End_Ind2_2, ]
    Set$Path3 <- subway_data[[as.character(Set$Info[3, "Line"])]][Start_Ind3_2:End_Ind3_2, ]
    if (isTRUE(as.character(Set$Info[1, "Line"]) ==
               2) & isTRUE(Set$Info[1, "Count"] == (Total_Depart_Raw - Start_Ind_2 + End_Ind_2))) {
      Set$Path1 <- subway_data[["2"]][c(Start_Ind_2:Total_Depart_Raw, 
                                        1:End_Ind_2), ]
    } else if (isTRUE(Set$Info[1, "Line"] == 
                      2) & isTRUE(Set$Info[1, "Count"] == (Total_Depart_Raw - End_Ind_2 + Start_Ind_2))) {
      Set$Path1 <- subway_data[["2"]][c(End_Ind_2:Total_Depart_Raw, 1:Start_Ind_2), ]
    }
    if (isTRUE(Set$Info[2, "Line"] == 
               2) & isTRUE(Set$Info[2, "Count"] == (Total_Transfer_Raw - Start_Ind2_2 + End_Ind2_2))) {
      Set$Path2 <- subway_data[["2"]][c(Start_Ind2_2:Total_Transfer_Raw, 1:End_Ind2_2), ]
    } else if (isTRUE(Set$Info[2, "Line"] == 
                      2) & isTRUE(Set$Info[2, "Count"] == (Total_Transfer_Raw - End_Ind2_2 + Start_Ind2_2))) {
      Set$Path2 <- subway_data[["2"]][c(End_Ind2_2:Total_Transfer_Raw, 1:Start_Ind2_2), ]
    }
    if (isTRUE(Set$Info[3, "Line"] == 2) & isTRUE(Set$Info[3, "Count"] == 
                                                  (Total_End_Raw - Start_Ind3_2 + End_Ind3_2))) {
      Set$Path3 <- subway_data[["2"]][c(Start_Ind3_2:Total_End_Raw, 
                                        1:End_Ind3_2), ]
    } else if (isTRUE(Set$Info[3, "Line"] == 
                      2) & isTRUE(Set$Info[3, "Count"] == (Total_End_Raw - End_Ind3_2 + Start_Ind3_2))) {
      Set$Path3 <- subway_data[["2"]][c(End_Ind3_2:Total_End_Raw, 1:Start_Ind3_2), ]
    }
    if (isTRUE(Set$Info[1, "Line"] == "6-A") & Start_Ind_2 > End_Ind_2) {
      Set$Path1 <- subway_data[["6-A"]][c(Start_Ind_2:6, 1:End_Ind_2), ]
    }
    if (isTRUE(Set$Info[2, "Line"] == "6-A") & Start_Ind2_2 > End_Ind2_2) {
      Set$Path2 <- subway_data[["6-A"]][c(Start_Ind2_2:6, 1:End_Ind2_2), ]
    }
    if (isTRUE(Set$Info[3, "Line"] == "6-A") & Start_Ind3_2 > End_Ind3_2) {
      Set$Path3 <- subway_data[["6-A"]][c(Start_Ind3_2:6, 1:End_Ind3_2), ]
    }
  }
  if (nrow(Set$Info) == 4) {
    Start_Ind_3 <- which(subway_data[[as.character(Set$Info[1, "Line"])]]$Name == 
                           as.character(Set$Info[1, "Depart"]))
    End_Ind_3 <- which(subway_data[[as.character(Set$Info[1, "Line"])]]$Name == 
                         as.character(Set$Info[1, "Arrive"]))
    Start_Ind2_3 <- which(subway_data[[as.character(Set$Info[2, "Line"])]]$Name == 
                            as.character(Set$Info[2, "Depart"]))
    End_Ind2_3 <- which(subway_data[[as.character(Set$Info[2, "Line"])]]$Name == 
                          as.character(Set$Info[2, "Arrive"]))
    Start_Ind3_3 <- which(subway_data[[as.character(Set$Info[3, "Line"])]]$Name == 
                            as.character(Set$Info[3, "Depart"]))
    End_Ind3_3 <- which(subway_data[[as.character(Set$Info[3, "Line"])]]$Name == 
                          as.character(Set$Info[3, "Arrive"]))
    Start_Ind4_3 <- which(subway_data[[as.character(Set$Info[4, "Line"])]]$Name == 
                            as.character(Set$Info[4, "Depart"]))
    End_Ind4_3 <- which(subway_data[[as.character(Set$Info[4, "Line"])]]$Name == 
                          as.character(Set$Info[4, "Arrive"]))
    Total_Depart_Raw <- nrow(subway_data[[as.character(Set$Info[1, "Line"])]])
    Total_Transfer1_Raw <- nrow(subway_data[[as.character(Set$Info[2, "Line"])]])
    Total_Transfer2_Raw <- nrow(subway_data[[as.character(Set$Info[3, "Line"])]])
    Total_End_Raw <- nrow(subway_data[[as.character(Set$Info[4, "Line"])]])
    Set$Path1 <- subway_data[[as.character(Set$Info[1, "Line"])]][Start_Ind_3:End_Ind_3, ]
    Set$Path2 <- subway_data[[as.character(Set$Info[2, "Line"])]][Start_Ind2_3:End_Ind2_3, ]
    Set$Path3 <- subway_data[[as.character(Set$Info[3, "Line"])]][Start_Ind3_3:End_Ind3_3, ]
    Set$Path4 <- subway_data[[as.character(Set$Info[4, "Line"])]][Start_Ind4_3:End_Ind4_3, ]
    if (isTRUE(Set$Info[1, "Line"] == 
               2) & isTRUE(Set$Info[1, "Line"] == (Total_Depart_Raw - Start_Ind_3 + End_Ind_3))) {
      Set$Path1 <- subway_data[["2"]][c(Start_Ind_3:Total_Depart_Raw, 1:End_Ind_3), ]
    } else if (isTRUE(Set$Info[1, "Line"] == 
                      2) & isTRUE(Set$Info[1, "Line"] == (Total_Depart_Raw - End_Ind_3 + Start_Ind_3))) {
      Set$Path1 <- subway_data[["2"]][c(End_Ind_3:Total_Depart_Raw, 1:Start_Ind_3), ]
    }
    if (isTRUE(Set$Info[2, "Line"] == 
               2) & isTRUE(Set$Info[2, "Count"] == (Total_Transfer1_Raw - Start_Ind2_3 + End_Ind2_3))) {
      Set$Path2 <- subway_data[["2"]][c(Start_Ind2_3:Total_Transfer1_Raw, 1:End_Ind2_3), ]
    } else if (isTRUE(Set$Info[2, "Line"] == 
                      2) & isTRUE(Set$Info[2, "Count"] == (Total_Transfer1_Raw - End_Ind2_3 + Start_Ind2_3))) {
      Set$Path2 <- subway_data[["2"]][c(End_Ind2_3:Total_Transfer1_Raw, 1:Start_Ind2_3), ]
    }
    if (isTRUE(Set$Info[3, "Line"] == 
               2) & isTRUE(Set$Info[3, "Count"] == (Total_Transfer2_Raw - Start_Ind3_3 + End_Ind3_3))) {
      Set$Path3 <- subway_data[["2"]][c(Start_Ind3_3:Total_Transfer2_Raw, 1:End_Ind3_3), ]
    } else if (isTRUE(Set$Info[3, "Line"] == 
                      2) & isTRUE(Set$Info[3, "Count"] == (Total_Transfer2_Raw - End_Ind3_3 + Start_Ind3_3))) {
      Set$Path3 <- subway_data[["2"]][c(End_Ind3_3:Total_Transfer2_Raw, 1:Start_Ind3_3), ]
    }
    if (isTRUE(Set$Info[3, "Line"] == 
               2) & isTRUE(Set$Info[4, "Count"] == (Total_End_Raw - Start_Ind4_3 + End_Ind4_3))) {
      Set$Path4 <- subway_data[["2"]][c(Start_Ind3_3:Total_End_Raw, 1:End_Ind4_3), ]
    } else if (isTRUE(Set$Info[4, "Line"] == 
                      2) & isTRUE(Set$Info[4, "Count"] == (Total_End_Raw - End_Ind4_3 + Start_Ind4_3))) {
      Set$Path4 <- subway_data[["2"]][c(End_Ind4_3:Total_End_Raw, 1:Start_Ind4_3), ]
    }
    if (isTRUE(Set$Info[1, "Line"] == "6-A") & Start_Ind_3 > End_Ind_3) {
      Set$Path1 <- subway_data[["6-A"]][c(Start_Ind_3:6, 1:End_Ind_3), ]
    }
    if (isTRUE(Set$Info[2, "Line"] == "6-A") & Start_Ind2_3 > End_Ind2_3) {
      Set$Path2 <- subway_data[["6-A"]][c(Start_Ind2_3:6, 1:End_Ind2_3), ]
    }
    if (isTRUE(Set$Info[3, "Line"] == "6-A") & Start_Ind3_3 > End_Ind3_3) {
      Set$Path3 <- subway_data[["6-A"]][c(Start_Ind3_3:6, 1:End_Ind3_3), ]
    }
    if (isTRUE(Set$Info[4, "Line"] == "6-A") & Start_Ind4_3 > End_Ind4_3) {
      Set$Path4 <- subway_data[["6-A"]][c(Start_Ind4_3:6, 1:End_Ind4_3), ]
    }
  }
  return(Set)
}

