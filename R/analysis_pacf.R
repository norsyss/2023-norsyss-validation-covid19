analysis_pacf <- function(data, argset){
  if(plnr::is_run_directly()){
    data <- p1$get_data()
    argset <- p1$get_argset(1)
  }
  
  
  return(list(
    "hospital" = pacf(data$hard$hospital_n, plot = F),
    "icu" = pacf(data$hard$icu_n, plot = F),
    "death" = pacf(data$hard$death_n, plot = F),
    "norsyss" = pacf(
      data$norsyss[
        icpc2group_tag=="covid19" & 
        tariffgroup_tag=="fe"
      ]$consultations_icpc2group_vs_all_pr100
    )
  ))
}
