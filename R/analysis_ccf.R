analysis_ccf <- function(data, argset){
  if(plnr::is_run_directly()){
    data <- p2$get_data()
    argset <- p2$get_argset(1)
  }
  
  if(argset$time == "wuhan"){
    isoyearweek_min <- "2020-09"
    isoyearweek_max <- "2021-06"
  } else if(argset$time == "alpha"){
    isoyearweek_min <- "2021-07"
    isoyearweek_max <- "2021-26"
  } else if(argset$time == "delta"){
    isoyearweek_min <- "2021-07"
    isoyearweek_max <- "2021-51"
  } else if(argset$time == "omicron"){
    isoyearweek_min <- "2021-52"
    isoyearweek_max <- "2023-20"
  } else if(argset$time == "nocontrol"){
    isoyearweek_min <- "2022-07"
    isoyearweek_max <- "2023-20"
  } else if(argset$time == "all"){
    isoyearweek_min <- "2020-09"
    isoyearweek_max <- "2023-20"
  }

  hard <- data$hard[isoyearweek >= isoyearweek_min & isoyearweek <= isoyearweek_max][[argset$hard]]
  norsyss <- data$norsyss[
    isoyearweek >= isoyearweek_min & isoyearweek <= isoyearweek_max &
    icpc2group_tag==argset$icpc2group_tag & 
    tariffgroup_tag==argset$tariffgroup_tag
  ]$consultations_icpc2group_vs_all_pr100
  
  hard <- diff(hard)
  norsyss <- diff(norsyss)

  res <- ccf(
    x = hard,
    y = norsyss,
    plot = F
  )
  autocorrelations <- data.table(
    hard = argset$hard,
    time = argset$time,
    icpc2group_tag = argset$icpc2group_tag,
    tariffgroup_tag = argset$tariffgroup_tag,
    lag = res$lag[,,1],
    autocorrelations = res$acf[,,1],
    upper_ci = qnorm((1 + 0.95)/2)/sqrt(res$n.used),
    lower_ci = -qnorm((1 + 0.95)/2)/sqrt(res$n.used)
  )
  return(autocorrelations)
}
