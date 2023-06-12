get_hard_endpoint_data <- function(){
  d <- fread(fs::path(org::project$data, "status-for-pandemien-nas.csv"))
  setnames(
    d,
    c(
      "isoyearweek",
      "hospital_n",
      "icu_n",
      "death_n"
    )
  )
  return(d)
}