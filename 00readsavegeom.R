### purpose: read in spatial dat from tigris; save as rds
### last run: 08/05/2022

library(tidycensus)
library(tidyverse)
library(sf)
library(tigris)
library(acsmapping)

options(tigris_use_cache = T)

readsave_spatial_acsmapoverall("block group")

readsave_spatial_acsmapoverall <- function(geog, tidyyear = 2020, tidystate = "Maryland", basedir = "./data"){
  
  # browser()
  
  if (is.null(tidystate)){
    
    geogpath <- glue::glue("{basedir}/{tidyyear}/{geog}/processed")
    
  }
  
  else {
    stateabr <- state.abb[grep(tidystate, state.name)]
    
    geogpath <- glue::glue("{basedir}/{tidyyear}/{stateabr}/{geog}/processed")
    
  }
  
  agedf <- readRDS(glue::glue("{geogpath}/process_vars_agesex.rds")) %>%
    acsmapping::pctna_recode(totcol = tot_people, colname = pct_age) %>%
    # rename(estimate = age_tot_est,
    #        moe = age_tot_moe) %>%
    acsmapping::pivotfunct( "agegrp")
  
  povdf <- readRDS(glue::glue("{geogpath}/process_vars_pov_ratio.rds")) %>%
    acsmapping::pctna_recode(totcol = pop_tot, colname = pct_incpov) %>%
    acsmapping::pivotfunct("incpov_ratio")
  
  vehicledf <- readRDS(glue::glue("{geogpath}/process_vars_vehicle.rds")) %>%
    # dplyr::rename(estimate = anyveh_est,
    #        moe = anyveh_moe) %>%
    acsmapping::pctna_recode(totcol = tothous, colname = pct_anyveh) %>%
    acsmapping::pivotfunct("anyvehicle")
  
  racedf <- readRDS(glue::glue("{geogpath}/process_vars_race.rds")) %>%
    dplyr::filter(!grepl("Not Hispanic", race_ethnicity)) %>%
    acsprocess::race_recode(race_col = race_ethnicity)%>%
    acsmapping::pctna_recode(totcol = pop_total, colname = pct_race) %>%
    acsmapping::pivotfunct("race")
  
  tenuredf <- readRDS(glue::glue("{geogpath}/process_vars_tenure.rds")) %>%
    acsmapping::pctna_recode(totcol = tenure_overall, colname = pct_tenure) %>%
    acsmapping::pivotfunct("tenure")
  
  popdf <- readRDS(glue::glue("{geogpath}/process_vars_pop.rds")) %>%
    dplyr::rename(estimate_Population = estimate)%>%
    dplyr::select(-c(label, concept, variable, moe, geography))
  
  if (is.null(tidystate)){
    
    geogfile <- glue::glue("{geog}_{tidyyear}.rds")
    
  }
  
  else{
    
    stateabr <- state.abb[grep(tidystate, state.name)]
    
    geogfile <- glue::glue("{geog}_{tidyyear}_{stateabr}.rds")
  }
  
  geogfile <- read_rds(glue::glue("{basedir}/{geogfile}")) %>%
    dplyr::rename_all(tolower) %>%
    sf::st_transform(4326)
  
  # if (grepl("place", geog)){
  #   browser()
  # }
  
  joineddf <- geogfile %>%
    dplyr::left_join(povdf, by = "geoid") %>%
    dplyr::left_join(tenuredf, by = "geoid") %>%
    dplyr::left_join(vehicledf, by = "geoid") %>%
    dplyr::left_join(agedf, by = "geoid") %>%
    dplyr::left_join(popdf, by = "geoid") %>%
    dplyr::left_join(racedf, by = "geoid")
  
  saveRDS(joineddf, glue::glue("{geogpath}/acs_shp.rds"))
  
}

tigrisgeom <- tigris::block_groups(
  state = 24,
  year = 2020
)

saveRDS(tigrisgeom, "./data/mdblock group_2020.rds")

tigrisgeom <- tigris::tracts(
  state = 24,
  year = 2020
)

saveRDS(tigrisgeom, "./data/mdtract_2020.rds")

tigrisgeom <- tigris::places(
  state = 24,
  year = 2020
)

saveRDS(tigrisgeom, "./data/mdplace_2020.rds")

tigrisgeom <- tigris::counties(
  state = 24,
  year = 2020
)

saveRDS(tigrisgeom, "./data/mdcounty_2020.rds")

library(acsmapping)
loadtidydata_acsmapoverall <- function(geogs = c("block group", "tract", "place", "county"), tidyyear = 2020, tidystate = "Maryland", downloadgeog = F, basedir= "./data", .geomyear = tidyyear ){
  
  browser()
  # load tidycensus variables
  varscensus <- tidycensus::load_variables(year = tidyyear, "acs5", cache = T)
  
  # read
  vars_race <- varscensus %>%
    dplyr::filter(grepl("^B02001", name)) %>%
    dplyr::pull(name)
  
  vars_hisp <- varscensus %>%
    dplyr::filter(name %in% c("B03002_002", "B03002_012")) %>%
    dplyr::pull(name)
  
  vars_agesex <- varscensus %>%
    dplyr::filter(grepl("AGE", concept) & grepl("SEX", concept)) %>%
    dplyr::filter(concept == "SEX BY AGE") %>%
    dplyr::pull(name)
  
  vars_disab <- varscensus %>%
    dplyr::filter(grepl("DISABILITY", concept)) %>%
    dplyr::filter(geography == "block group")
  
  vars_disab <- varscensus %>%
    dplyr::filter(grepl("^B22010", name)) %>%
    dplyr::pull(name)
  
  vars_pov <- varscensus %>%
    dplyr::filter(grepl("(POVERTY)|(INCOME)", concept))
  
  vars_pov_bg <- vars_pov %>%
    dplyr::filter(geography == "block group") %>%
    dplyr::filter(grepl("POVERTY", concept))
  
  vars_pov_ratio <- vars_pov %>%
    dplyr::filter(concept == "RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS") %>%
    dplyr::pull(name)
  
  vars_vehicle <- varscensus %>%
    dplyr::filter(grepl("car", label))
  
  vars_vehicle <- varscensus %>%
    dplyr::filter(grepl("VEHICLE", concept)) %>%
    dplyr::filter(geography == "block group")
  
  vars_vehicle <- vars_vehicle %>%
    dplyr::filter(grepl("^B25044", name)) %>%
    dplyr::pull(name)
  
  vars_tenure <- varscensus %>%
    dplyr::filter(grepl("^B25003_", name)) %>%
    dplyr::pull(name)
  
  vars_pop <- varscensus %>%
    dplyr::filter(grepl("^B01003_", name)) %>%
    dplyr::pull(name)
  
  # create list of dfs to download
  readvars <- list(
    "vars_race" = c(vars_race, vars_hisp),
    "vars_agesex" = vars_agesex,
    "vars_disab" = vars_disab,
    "vars_pov_ratio" = vars_pov_ratio,
    "vars_vehicle" = vars_vehicle,
    'vars_pop' = vars_pop,
    "vars_tenure" = vars_tenure
  )
  
  if (downloadgeog){
    
    acsmapping::downloadgeogs(geogs = geogs, tidyyear = .geomyear, tidystate = tidystate, dirsave = basedir)
  }
  
  # save each at different geographies
  purrr::walk2(
    readvars,
    names(readvars), ~
      {
        purrr::walk(geogs, function(geog){
          
          acspull(.x, .y, geog = geog, state = tidystate, basedir = basedir)
          
        })
      })
}

acspull <- function(varlist, filename, year = 2020, geog = "block group", varscensus = NULL, state = "Maryland", basedir = "./data"){
  browser()
  if (is.null(varscensus)){
    
    varscensus <- tidycensus::load_variables(year = year, "acs5", cache = T)
    
  }
  
  acsdata <- tidycensus::get_acs(
    year= 2020,
    variables = varlist,
    geography = geog,
    survey= "acs5",
    state = state,
    geometry = F,
    cache_table = T
  ) %>%
    dplyr::rename_all(tolower)
  
  # browser()
  # join
  acsdata <- acsdata %>%
    dplyr::left_join(varscensus, by = c("variable" = "name"))
  
  stateabbr <- state.abb[grep(state, state.name)]
  
  # create directory to store data if not exist
  acsprocess::quickdircreate(basedir)
  acsprocess::quickdircreate(paste0(basedir, "/", year))
  acsprocess::quickdircreate(paste0(basedir, "/", year, "/", stateabbr, "/"))
  acsprocess::quickdircreate(paste0(basedir, "/", year, "/", stateabbr, "/", geog))
  
  # save data
  saveRDS(object = acsdata,
          file = paste0(basedir,
                        "/",
                        year,
                        "/",
                        stateabbr,
                        "/",
                        geog,
                        "/",
                        filename,
                        ".rds"))
  
}


acsmapping::prepallinone_acsmapoverall()

acsmapping::loadtidydata_acsmapoverall()
acsmapping::readprocessdata_acsmapoverall()

readsave_spatial_acsmapoverall <- function(geog, tidyyear = 2020, tidystate = "Maryland", basedir = "./data"){
  
  browser()
  
  geogpath <- glue::glue("{basedir}/{tidyyear}/{tidystate}/{geog}/processed")
  
  agedf <- readRDS(glue::glue("{geogpath}/process_vars_agesex.rds")) %>%
    acsmapping::pctna_recode(totcol = tot_people, colname = pct_age) %>%
    # rename(estimate = age_tot_est,
    #        moe = age_tot_moe) %>%
    acsmapping::pivotfunct( "agegrp")
  
  povdf <- readRDS(glue::glue("{geogpath}/process_vars_pov_ratio.rds")) %>%
    acsmapping::pctna_recode(totcol = pop_tot, colname = pct_incpov) %>%
    acsmapping::pivotfunct("incpov_ratio")
  
  vehicledf <- readRDS(glue::glue("{geogpath}/process_vars_vehicle.rds")) %>%
    # dplyr::rename(estimate = anyveh_est,
    #        moe = anyveh_moe) %>%
    acsmapping::pctna_recode(totcol = tothous, colname = pct_anyveh) %>%
    acsmapping::pivotfunct("anyvehicle")
  
  racedf <- readRDS(glue::glue("{geogpath}/process_vars_race.rds")) %>%
    dplyr::filter(!grepl("Not Hispanic", race_ethnicity)) %>%
    acsprocess::race_recode(race_col = race_ethnicity)%>%
    acsmapping::pctna_recode(totcol = pop_total, colname = pct_race) %>%
    acsmapping::pivotfunct("race")
  
  tenuredf <- readRDS(glue::glue("{geogpath}/process_vars_tenure.rds")) %>%
    acsmapping::pctna_recode(totcol = tenure_overall, colname = pct_tenure) %>%
    acsmapping::pivotfunct("tenure")
  
  popdf <- readRDS(glue::glue("{geogpath}/process_vars_pop.rds")) %>%
    dplyr::rename(estimate_Population = estimate)%>%
    dplyr::select(-c(label, concept, variable, moe, geography))
  
  if (is.null(stae)){
    
    geogfile <- glue::glue("{geog}_{tidyyear}.rds")
    
  }
  
  else{
    
    stateabr <- state.abb[grep(tidystate, state.name)]
    
    geogfile <- glue::glue("{geog}_{tidyyear}_{stateabr}.rds")
  }
  
  geogfile <- read_rds(glue::glue("{basedir}/{geogfile}")) %>%
    dplyr::rename_all(tolower) %>%
    sf::st_transform(4326)
  
  # if (grepl("place", geog)){
  #   browser()
  # }
  
  joineddf <- geogfile %>%
    dplyr::left_join(povdf, by = "geoid") %>%
    dplyr::left_join(tenuredf, by = "geoid") %>%
    dplyr::left_join(vehicledf, by = "geoid") %>%
    dplyr::left_join(agedf, by = "geoid") %>%
    dplyr::left_join(popdf, by = "geoid") %>%
    dplyr::left_join(racedf, by = "geoid")
  
  saveRDS(joineddf, glue::glue("{geogpath}/acs_shp.rds"))
  
}


processdfs_acsmapoverall <- function(geog, tidyyear = 2020, tidystate = "Maryland", basedir = "./data"){
  
  browser()
  # browser()
  
  acsmapping::quickdircreate(basedir)
  acsmapping::quickdircreate(glue::glue("{basedir}/{tidyyear}"))
  
  if (is.null(tidystate)){
    
    dirfiles<- glue::glue("{basedir}/{tidyyear}/{geog}")
    dirprocess <- glue::glue("{dirfiles}/processed")
    
  }
  
  else{
    
    stateabr <- state.abb[grep(tidystate, state.name)]
    
    acsmapping::quickdircreate(glue::glue("{basedir}/{tidyyear}/{stateabr}"))
    
    dirfiles<- glue::glue("{basedir}/{tidyyear}/{stateabr}/{geog}")
    
    dirprocess <- glue::glue("{dirfiles}/processed")
    
  }
  
  acsmapping::quickdircreate(dirfiles)
  acsmapping::quickdircreate(dirprocess)
  
  readRDS(glue::glue("{dirfiles}/vars_race.rds")) %>%
    acsprocess::process_race_ethn() %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_race.rds"))
  
  readRDS(glue::glue("{dirfiles}/vars_agesex.rds"))  %>%
    # acsprocess::location_col() %>%
    acsprocess::process_age_overall() %>%
    dplyr::mutate(
      agegrp = dplyr::case_when(
        grepl("(^Under)|(^[1-9] to)|(^1[0-7] )", age) ~ "Under 18",
        grepl("(^6[5-9] )|(^[7-9][0-9] )|(over$)", age) ~ "65 and over",
        T ~ "18-65"
      )
    ) %>%
    acsprocess::est_moe_derive(group_cols = c("name", "agegrp")) %>%
    dplyr::select(
      geoid, name, agegrp, tot_people, tot_people_moe, name_agegrp_est, name_agegrp_moe
    ) %>%
    dplyr::distinct() %>%
    dplyr::rename(estimate = name_agegrp_est,
                  moe = name_agegrp_moe) %>%
    acsprocess::derive_pct_est_moe("pct_age", "tot_people", "tot_people_moe") %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_agesex.rds"))
  
  readRDS(glue::glue("{dirfiles}/vars_disab.rds"))  %>%
    acsprocess::location_col() %>%
    acsprocess::process_disab_foodstamp(overall = T) %>%
    dplyr::distinct() %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_disab.rds"))
  
  readRDS(glue::glue("{dirfiles}/vars_pov_ratio.rds")) %>%
    acsprocess::process_poverty_detail() %>%
    acsprocess::incpov_recode(incpov_ratio) %>%
    dplyr::group_by(name) %>%
    dplyr::arrange(incpov_new) %>%
    dplyr::mutate(estimate = cumsum(estimate),
                  pct_incpov = estimate/ pop_tot) %>%
    dplyr::ungroup() %>%
    dplyr::filter(incpov_new %in% c(".50 to .99", "1.85 to 1.99")) %>%
    dplyr::mutate(incpov_ratio = dplyr::case_when(
      incpov_new == ".50 to .99" ~ "Under poverty line",
      incpov_new == "1.85 to 1.99"~ "Under 2X poverty line"
    )) %>%
    dplyr::mutate(
      dplyr::across(grep("^pct_", colnames(.), value = T),
                    ~ round(.x * 100, 2))
    ) %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_pov_ratio.rds"))
  
  readRDS(glue::glue("{dirfiles}/vars_vehicle.rds")) %>%
    acsprocess::process_tenure_vehicleown(overall = T) %>%
    # dplyr::select(geoid, name, tothous, tothous_moe, anyvehicle, anyveh_est, anyveh_moe, pct_anyveh, pct_moe, pct_upper, pct_lower) %>%
    # dplyr::distinct() %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_vehicle.rds"))
  
  readRDS(glue::glue("{dirfiles}/vars_pop.rds")) %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_pop.rds"))
  
  read_rds(glue::glue("{dirfiles}/vars_tenure.rds")) %>%
    acsprocess::process_tenure_df() %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_tenure.rds"))
  
}

vehicles <- readRDS(glue::glue("./data/2020/MD/block group/vars_vehicle.rds")) %>%
  acsprocess::process_tenure_vehicleown(overall = T)

readprocessdata_acsmapoverall <- function(geogs = c("block group", "tract", "place", "county"), tidyyear = 2020, tidystate = "Maryland", basedir = "./data"){
  
  # at different geometries - read in/save
  purrr::walk(geogs, ~ processdfs_acsmapoverall(.x, tidyyear = tidyyear, tidystate = tidystate, basedir = basedir))
  
}

