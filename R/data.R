save_norsyss_data <- function(){
  dbcon <- csdb::DBConnection_v9$new(
    driver = Sys.getenv("SC9_DBCONFIG_DRIVER"),
    port = as.integer(Sys.getenv("SC9_DBCONFIG_PORT")),
    user = Sys.getenv("SC9_DBCONFIG_USER"),
    password = Sys.getenv("SC9_DBCONFIG_PASSWORD"),
    trusted_connection = Sys.getenv("SC9_DBCONFIG_TRUSTED_CONNECTION"),
    server = Sys.getenv("SC9_DBCONFIG_SERVER"),
    schema = Sys.getenv("SC9_DBCONFIG_SCHEMA_ANON"),
    db = Sys.getenv("SC9_DBCONFIG_DB_ANON")
  )
  
  icpc2 <- c("r991", "r992", "covid19")
  retval <- vector("list", length = length(icpc2))
  for(i in seq_along(icpc2)){
    retval[[i]] <- dbcon$autoconnection %>% 
      dplyr::tbl(glue::glue("anon_norsyss_data_PARTITION_{icpc2[i]}")) %>% 
      dplyr::filter(granularity_time %in% "isoyearweek") %>% 
      dplyr::filter(granularity_geo %in% "nation") %>% 
      dplyr::filter(age %in% "total") %>% 
      dplyr::filter(sex %in% "total") %>% 
      dplyr::filter(tariffgroup_tag %in% c("f", "e", "s", "fe", "fes")) %>% 
      dplyr::filter(isoyearweek >= "2020-03") %>%
      dplyr::select(
        granularity_time,
        granularity_geo,
        country_iso3,
        location_code,
        border,
        age,
        sex,
        
        isoyear,
        isoweek,
        isoyearweek,
        season,
        seasonweek,
        
        calyear,
        calmonth,
        calyearmonth,
        
        date,
        
        tariffgroup_tag,
        
        consultations_icpc2group_n,
        consultations_icpc2group_vs_all_pr100,
        consultations_icpc2group_vs_without_influenza_covid19_pr100,
        consultations_all_n,
        consultations_without_influenza_covid19_n,
        
        icpc2group_tag
      ) %>%
      dplyr::collect() %>%
      setDT()
  }
  retval <- rbindlist(retval)
  saveRDS(retval, fs::path(org::project$data, "norsyss.RDS"))
}
