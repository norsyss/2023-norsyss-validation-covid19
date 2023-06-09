# initialize the project
# note: remember to keep in sync with quarto/quarto.qmd
project <- org::initialize_project(
  env     = .GlobalEnv,
  home    = "~/articles/2023-norsyss-validation-covid19",
  quarto  = "~/articles/2023-norsyss-validation-covid19/quarto",
  data    = "~/articles/2023-norsyss-validation-covid19/data",
  results = "~/articles/2023-norsyss-validation-covid19/results",
  folders_to_be_sourced = "R"
)

library(data.table)
library(ggplot2)
library(magrittr)

# save_norsyss_data()

p1 <- plnr::Plan$new()
p1$add_data("data", direct = get_data())
for(i in c(0:29, 365)){
  p1$add_argset(delay=i)
}
p1$apply_action_fn_to_all_argsets(analysis_data)

results <- p1$run_all()
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
    granularity_geo,
    pop,
    diagnosis
  )]
results[, pr100_diagnosis_temporary_over_final := consultations_diagnosis_temporary_pr100 / consultations_diagnosis_final_pr100]
results[is.nan(pr100_diagnosis_temporary_over_final), pr100_diagnosis_temporary_over_final := 1]

saveRDS(results, org::path(org::project$data, "results_analyzed.RDS"), compress = "xz")


x <- results[pop=="0 to 10 000" & diagnosis %in% c("covid19", "r80") & days_delay!=365]
x[, .(mean(consultations_all_final_n)),by=.(diagnosis, days_delay)]

p2 <- plnr::Plan$new()
p2$add_data("results", direct = readRDS(org::path(org::project$data, "results_analyzed.RDS")))

for(pr in c(0.5, 0.6, 0.7, 0.8, 0.9)) for(m in c(1.05, 1.1)) {
  p2$add_argset(probability=pr, magnitude=m)
}
p2$apply_action_fn_to_all_argsets(analysis_results_analyzed)

results <- p2$run_all()
results <- rbindlist(results)
saveRDS(results, org::path(org::project$data, "results_cleaned.RDS"), compress = "xz")


pd <- results[
  days_delay != 365,
  .(
    consultations_mean_pr100 = mean(consultations_final_pr100),
    q75 = quantile(pr100_temporary_over_final, probs = 0.75, na.rm=T)
  ),
  keyby=.(
    granularity_geo,
    pop,
    diagnosis,
    days_delay
  )
]
pd[granularity_geo != "Nation", consultations_mean_pr100 := NA]
pd[, consultations_mean_pr100 := mean(consultations_mean_pr100, na.rm=T), by=diagnosis]

pd[q75 <= 1.1, recommended_day := days_delay]
pd[, recommended_day := min(recommended_day, na.rm=T),
   by =.(granularity_geo, pop, diagnosis)]
pd[]
pd <- pd[days_delay==recommended_day]
x <- pd[granularity_geo == "Nation",.(diagnosis, consultations_mean_pr100)]
setorder(x, -consultations_mean_pr100)
pd[, diagnosis := factor(diagnosis, levels = x$diagnosis)]
dcast.data.table(
  pd,
  granularity_geo + pop ~ diagnosis,
  value.var = "recommended_day"
)

pd[recommended_day==TRUE]
pd[pop=="[500001,1300000]" & diagnosis=="s72"]

pd[days_delay==28]

q <- ggplot(pd, aes(x= days_delay, y = pr100_temporary_over_final, group = days_delay))
q <- q + geom_boxplot()
q <- q + scale_y_continuous(
  trans = scales::log2_trans(),
  breaks = c(1/1.2,1/1.1, 1, 1.1, 1.2),
  labels = c("1/1.2", "1/1.1", "1.0", "1.1", "1.2")
)
q

# figure 1
pd <- data.frame(x = 1:10, y = 1:10)
q <- ggplot(pd, aes(x = x, y = y))
q <- q + geom_point()
filepath <- org::path(org::project$results_today, "fig_01.png")
ggsave(filepath, plot = q, width = 297, height = 210, units ="mm")
results$fig_01_filepath <- filepath
results$fig_01 <- q

# save results
saveRDS(results, org::path(org::project$results_today, "results.rds"))

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
