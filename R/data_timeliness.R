get_raw_data <- function(date_from, date_to){
  db <- DBI::dbConnect(
    odbc::odbc(),
    driver="ODBC Driver 17 for SQL Server",
    server="dm-prod",
    database="SykdomspulsenAnalyse",
    trusted_connection="yes"
  )
  
  command <- glue::glue(
    "SELECT Id as id,Diagnose,BehandlerKommune,Konsultasjonsdato as date_event,Registreringsdato as date_registered,Takst
    FROM Konsultasjon
    JOIN KonsultasjonDiagnose ON Id=KonsultasjonId
    JOIN KonsultasjonTakst ON Id=KonsultasjonTakst.KonsultasjonId
    WHERE Konsultasjonsdato >='{date_from}' AND Konsultasjonsdato<='{date_to}'"
  )
  d <- DBI::dbGetQuery(db, command)
  setDT(d)
  
  d <- d[BehandlerKommune != "2100"]
  
  return(d)
}

save_norsyss_timeliness_data <- function(){
  d <- get_raw_data("2022-01-03", "2023-01-01")
  d[, municip_code := paste0("municip_nor", BehandlerKommune)]
  d[, county_code := paste0("county_nor", stringr::str_extract(municip_code,"[0-9][0-9]"))]
  d[, BehandlerKommune := NULL]
  d[, municip_code := NULL]
  
  for(i in c("covid19")){
    index <- which(norsyss::icpc2$icpc2group_tag==i)
    d[, (norsyss::icpc2$icpc2group_tag[index]) := as.integer(Diagnose %in% norsyss::icpc2$icpc2raw_tag[[index]])]
  }
  
  d[, tariffgroup_tag := fcase(
    Takst %in% norsyss::tariff$raw[tariffgroup_tag=="f"]$tariffraw_tag, "f",
    Takst %in% norsyss::tariff$raw[tariffgroup_tag=="e"]$tariffraw_tag, "e",
    Takst %in% norsyss::tariff$raw[tariffgroup_tag=="s"]$tariffraw_tag, "s"
  )]
  d[, Takst := NULL]
  d <- d[tariffgroup_tag %in% c("f", "e")]
  
  d[, consultations_all_n := as.integer(1:.N==1), by=.(
    id
  )]
  
  d <- d[, lapply(.SD, sum), ,
         by = .(
           id,
           date_event,
           date_registered,
           county_code,
           tariffgroup_tag
         ),
         .SDcols = c("covid19", "consultations_all_n")
  ]
  
  county_levels <- unique(d$county_code)
  d[, county_code := as.numeric(factor(county_code, levels = county_levels))]
  d[, id := 1:.N]
  d
  
  saveRDS(d, org::path(org::project$data, "data_norsyss_timeliness.RDS"), compress = "xz")
}

get_norsyss_timeliness_data <- function(){
  dx <- readRDS(org::path(org::project$data, "data_norsyss_timeliness.RDS"))
  d_wide <- rbindlist(list(
    dx[,.(
      id,
      date_event,
      date_registered,
      location_code = paste0("county_", county_code),
      granularity_geo = "County",
      covid19,
      consultations_all_n
    )],
    dx[,.(
      id,
      date_event,
      date_registered,
      location_code = "nation",
      granularity_geo = "Nation",
      covid19,
      consultations_all_n
    )]
  ))
  
  rm("dx")
  
  d_long <- melt.data.table(
    d_wide,
    id.vars = c(
      "id",
      "date_event",
      "date_registered",
      "location_code",
      "granularity_geo",
      "consultations_all_n"
    ),
    variable.factor = FALSE,
    variable.name = "diagnosis"
  )
  
  rm("d_wide")
  
  gc()
  
  d_long[, granularity_geo := factor(
    granularity_geo,
    levels = c("Nation", "County")
  )]

  d_long[, isoyearweek_event := cstime::date_to_isoyearweek_c(date_event)]
  # 0 = available from first monday
  d_long[, days_delay := as.numeric(difftime(date_registered, cstime::isoyearweek_to_last_date(isoyearweek_event), units = "days"))]
  
  return(d_long)
}
