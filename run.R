# hard endpoints extracted 2023-06-09
# norsyss extracted 2023-06-09

# initialize the project
# note: remember to keep in sync with quarto/quarto.qmd
project <- org::initialize_project(
  env     = .GlobalEnv,
  home    = c(
    "~/articles/2023-norsyss-statistical-evaluation-covid19",
    "~/fhi/2023-norsyss-statistical-evaluation-covid19"
  ),
  quarto  = c(
    "~/articles/2023-norsyss-statistical-evaluation-covid19/quarto",
    "~/fhi/2023-norsyss-statistical-evaluation-covid19/quarto"
  ),
  data    = c(
    "~/articles/2023-norsyss-statistical-evaluation-covid19/data",
    "~/fhi/2023-norsyss-statistical-evaluation-covid19/data"
  ),
  results = c(
    "~/articles/2023-norsyss-statistical-evaluation-covid19/results",
    "~/fhi/2023-norsyss-statistical-evaluation-covid19/results"
  ),
  folders_to_be_sourced = "R"
)

library(data.table)
library(ggplot2)
library(magrittr)

# save_norsyss_consultations_data()
# save_norsyss_timeliness_data()

p1 <- plnr::Plan$new()
p1$add_data("norsyss", direct = get_norsyss_consultations_data())
p1$add_data("hard", direct = get_hard_endpoint_data())

p1$add_argset(x = 1)

p1$apply_action_fn_to_all_argsets(analysis_pacf)

results_p1 <- p1$run_all()

results_p1 <- results_p1[[1]]

saveRDS(results_p1, org::path(org::project$data, "results_p1.RDS"), compress = "xz")


# wuhan >= 2020-09 & 2021-06
# alpha >= 2021-07 & <= 2021-26
# delta >= 2021-27 & <= 2021-51 (https://www.sciencedirect.com/science/article/pii/S1201971221012182)
# omicron >= 2021-52 & <= 2023-20 (https://www.fhi.no/contentassets/8a971e7b0a3c4a06bdbf381ab52e6157/vedlegg/3.-alle-ukerapporter-2021/ukerapport-uke-52-27.12.21---02.01.22.pdf page 34)
# nocontrol >= 2022-07 & <= 2023-20 (https://www.regjeringen.no/no/tema/Koronasituasjonen/tidslinje-koronaviruset/id2692402/)


p2 <- plnr::Plan$new()
p2$add_data("norsyss", direct = get_norsyss_data())
p2$add_data("hard", direct = get_hard_endpoint_data())

# wuhan >= 2020-09 & 2021-06
# alpha >= 2021-07 & <= 2021-26
# delta >= 2021-27 & <= 2021-51 (https://www.sciencedirect.com/science/article/pii/S1201971221012182)
# omicron >= 2021-52 & <= 2023-20 (https://www.fhi.no/contentassets/8a971e7b0a3c4a06bdbf381ab52e6157/vedlegg/3.-alle-ukerapporter-2021/ukerapport-uke-52-27.12.21---02.01.22.pdf page 34)
# nocontrol >= 2022-07 & <= 2023-20 (https://www.regjeringen.no/no/tema/Koronasituasjonen/tidslinje-koronaviruset/id2692402/)

p2$add_argset_from_list(
  plnr::expand_list(
    icpc2group_tag = c("covid19", "r991", "r992"),
    tariffgroup_tag = c("f", "e", "s", "fe", "fes"),
    time = c("wuhan", "alpha", "delta", "omicron", "nocontrol", "all"),
    hard = c("hospital_n", "icu_n", "death_n")
  )
)

p2$apply_action_fn_to_all_argsets(analysis_ccf)

results_p2 <- p2$run_all()

results_p2 <- rbindlist(results_p2)

results_p2[, tariffgroup_tag := factor(
  tariffgroup_tag,
  levels = c("f", "e", "s", "fe", "fes")
)]

results_p2[, 
  time_label := 
  factor(
    time,
    levels = c("all", "nocontrol", "omicron", "delta", "alpha", "wuhan"),
    labels = c(
      "All",
      "No control",
      "Omicron",
      "Delta",
      "Alpha",
      "Wuhan"
    )
  )
]

results_p2[, hard_label := factor(
  hard,
  levels = c("hospital_n","icu_n","death_n"),
  labels = c("Hospital", "ICU", "Death")
)]

results_p2[, icpc2group_label := factor(
  icpc2group_tag,
  levels = c("r991", "r992", "covid19"),
  labels = c("R991", "R992", "R991+R992")
)]

results_p2[, significant := abs(autocorrelations) > upper_ci]

results_p2[, autocorrelation_label := csstyle::format_num_as_nor_num_2(autocorrelations)]
results_p2[, autocorrelation_label_zero_for_non_sig := autocorrelation_label]
results_p2[significant==F, autocorrelation_label_zero_for_non_sig := ""]
results_p2[, autocorrelation_label_star_for_sig := autocorrelation_label]
results_p2[significant==T, autocorrelation_label_star_for_sig := paste0(autocorrelation_label_star_for_sig,"*")]

results_p2[, autocorrelation_color := fancycut::wafflecut(
  autocorrelations,
  c("[-1,-0.9)","[-0.9,-0.7)","[-0.7,-0.4)","[-0.4,-0.2)","[-0.2,0.2]","(0.2,0.4]","(0.4,0.7]","(0.7,0.9]","(0.9,1]","[10,11]"),
  c(
    "-1.00 to -0.91 (Very strong)", 
    "-0.71 to -0.90 (High)", 
    "-0.41 to -0.70 (Moderate)", 
    "-0.21 to -0.40 (Weak)", 
    "-0.20 to 0.20 (Very weak)", 
    "0.21 to 0.40 (Weak)", 
    "0.41 to 0.70 (Moderate)", 
    "0.71 to 0.90 (High)", 
    "1.00 to 0.91 (Very strong)", 
    "Not significant"
  )
)]
results_p2[significant==F, autocorrelation_color := "Not significant"]

results_p2[, lag_factor_reverse := factor(
  lag,
  levels = c(
    19:-19
  )
)]

saveRDS(results_p2, org::path(org::project$data, "results_p2.RDS"), compress = "xz")

# timeliness

p3 <- plnr::Plan$new()
p3$add_data("data", direct = get_norsyss_timeliness_data())
for(i in c(0:29, 365)){
  p3$add_argset(delay=i)
}
p3$apply_action_fn_to_all_argsets(analysis_timeliness_data)

results <- p3$run_all()
results <- rbindlist(results)
results[days_delay==365, consultations_diagnosis_final_pr100 := consultations_diagnosis_temporary_pr100]
results[days_delay==365, consultations_diagnosis_final_n := consultations_diagnosis_temporary_n]
results[days_delay==365, consultations_all_final_n := consultations_all_temporary_n]
results[
  ,
  c(
    "consultations_diagnosis_final_pr100",
    "consultations_diagnosis_final_n",
    "consultations_all_final_n"
  ) := .(
    mean(consultations_diagnosis_final_pr100, na.rm=T),
    mean(consultations_diagnosis_final_n, na.rm=T),
    mean(consultations_all_final_n, na.rm=T)
  ),
  by = .(
    isoyearweek_event,
    location_code,
    diagnosis
  )]
results[, pr100_diagnosis_temporary_over_final := consultations_diagnosis_temporary_pr100 / consultations_diagnosis_final_pr100]
results[is.nan(pr100_diagnosis_temporary_over_final), pr100_diagnosis_temporary_over_final := 1]

saveRDS(results, org::path(org::project$data, "results_p3.RDS"), compress = "xz")

p4 <- plnr::Plan$new()
p4$add_data("results", direct = readRDS(org::path(org::project$data, "results_p3.RDS")))

for(m in c(1.05, 1.1, 1.15, 1.2)) for(pr in c(0.5, 0.6, 0.7, 0.8, 0.9)) {
  p4$add_argset(magnitude=m, probability=pr)
}
p4$apply_action_fn_to_all_argsets(analysis_timeliness_results)

results <- p4$run_all()
results <- rbindlist(results)
saveRDS(results, org::path(org::project$data, "results_p4.RDS"), compress = "xz")

# render the quarto doc
quarto::quarto_render(
  input = org::path(project$quarto), # not org::project here!
  quiet = FALSE
)

# move the rendered folder to results
# note: "rendered_quarto" is specified in quarto/_quarto.yml
org::move_directory(
  from = org::path(org::project$quarto, "rendered_quarto"),
  to = org::path(org::project$results_today, "rendered_quarto"),
  overwrite_to = TRUE
)
