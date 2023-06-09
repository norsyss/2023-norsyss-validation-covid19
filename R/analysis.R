analysis_data <- function(data, argset){
  if(plnr::is_run_directly()){
    data <- p1$get_data()
    argset <- p1$get_argset(1)
  }

  d <- data$data[
    days_delay<=argset$delay,
    .(
      consultations_all_temporary_n = .N,
      consultations_diagnosis_temporary_pr100 = round(100*mean(value),3),
      consultations_diagnosis_temporary_n = sum(value)
    ),
    keyby = .(
      isoyearweek_event,
      location_code,
      granularity_geo,
      pop,
      diagnosis
    )
  ]
  d[, days_delay := argset$delay]
  return(d)
}

analysis_results_analyzed <- function(data, argset){
  if(plnr::is_run_directly()){
    data <- p2$get_data()
    argset <- p2$get_argset(1)
  }

  pd <- data$results[
    days_delay != 365,
    .(
      consultations_all_mean_n = round(mean(consultations_all_final_n)),
      consultations_all_sd_n = round(sd(consultations_all_final_n)),
      consultations_diagnosis_mean_n = round(mean(consultations_diagnosis_final_n)),
      consultations_diagnosis_sd_n = round(sd(consultations_diagnosis_final_n)),
      consultations_diagnosis_mean_pr100 = mean(consultations_diagnosis_final_pr100),
      q_u = quantile(pr100_diagnosis_temporary_over_final, probs = 1-argset$probability/2, na.rm=T),
      q_l = quantile(pr100_diagnosis_temporary_over_final, probs = argset$probability/2, na.rm=T)
    ),
    keyby=.(
      granularity_geo,
      pop,
      diagnosis,
      days_delay
    )
  ]

  pd[q_l >= 1/argset$magnitude & q_u <= argset$magnitude, recommended_day := days_delay]
  pd[, recommended_day := min(recommended_day, na.rm=T),
     by =.(granularity_geo, pop, diagnosis)]
  pd[is.infinite(recommended_day), recommended_day := 29]
  pd[]
  pd <- pd[days_delay==recommended_day]
  pd[, q_u := NULL]
  pd[, q_l := NULL]
  pd[, probability := argset$probability]
  pd[, magnitude := argset$magnitude]

  return(pd)
}
