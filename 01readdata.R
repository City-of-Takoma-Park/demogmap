#### purpose: load tidycensus non-spatial dataframes; process using acsprocess functions; save as rds files
#### last run: 08/05/2022

library(tidycensus)
library(tidyverse)
library(ipumsr)
library(sf)
library(leaflet)
library(tigris)
library(acsprocess)

# download tidycensus data ----
# load tidycensus variables
varscensus <- tidycensus::load_variables(year = 2020, "acs5", cache = T)

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
  filter(grepl("POVERTY", concept))

vars_pov_bg$concept %>% unique()

vars_pov_ratio <- vars_pov %>%
  dplyr::filter(concept == "RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS") %>%
  pull(name)

vars_vehicle <- varscensus %>%
  dplyr::filter(grepl("car", label))

vars_vehicle$concept %>% unique()

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
  pull(name)

# function for downloading acs data at given geography and year
# varlist = names of variables to download; year is acs5 year; geog is geography downloading data for
acspull <- function(varlist, filename, year = 2020, geog = "block group", varscensus = varscensus){
  
  # browser()

  acsdata <- tidycensus::get_acs(
    year= 2020,
    variables = varlist,
    geography = geog,
    survey= "acs5",
    state = "Maryland",
    geometry = F,
    cache_table = T
  ) %>%
    rename_all(tolower)
  
  # browser()
  # join
  acsdata <- acsdata %>%
    left_join(varscensus, by = c("variable" = "name"))

  # create directory to store data if not exist
  if (!dir.exists(paths = paste0("./data/", year, "/", geog))){
    dir.create(paste0("./data/", year, "/", geog))
  }
  
  # save data
  saveRDS(object = acsdata,
          file = paste0("./data/", 
                        year, 
                        "/", 
                        geog,
                        "/",
                        filename, 
                        ".rds"))
  
}

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

# save each at different geographies
purrr::walk2(
  readvars,
  names(readvars), ~
    {
      walk(c("block group", "tract", "place", "county"), function(geog){
        acspull(.x, .y, geog = geog)
      })
    }
)

# process downloaded data ----

# read in data at different geometries; process
process_dfs <- function(geog){
  # browser()
  
  dirfiles<- glue::glue("./data/2020/{geog}")
  dirprocess <- glue::glue("{dirfiles}/processed")
  
  if (!dir.exists(dirprocess)){
    dir.create(dirprocess)
  }
  
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
    distinct() %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_disab.rds"))
  
  readRDS(glue::glue("{dirfiles}/vars_pov_ratio.rds")) %>%
    acsprocess::process_poverty_detail() %>%
    incpov_recode(incpov_ratio) %>%
    dplyr::group_by(name) %>%
    arrange(incpov_new) %>%
    dplyr::mutate(estimate = cumsum(estimate),
                  pct_incpov = estimate/ pop_tot) %>%
    dplyr::ungroup() %>%
    dplyr::filter(incpov_new %in% c(".50 to .99", "1.85 to 1.99")) %>%
    dplyr::mutate(incpov_ratio = dplyr::case_when(
      incpov_new == ".50 to .99" ~ "Under poverty line",
      incpov_new == "1.85 to 1.99"~ "Under 2X poverty line"
    )) %>% 
    mutate(
      across(grep("^pct_", colnames(.), value = T),
             ~ round(.x * 100, 2))
    ) %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_pov_ratio.rds")) 
  
  readRDS(glue::glue("{dirfiles}/vars_vehicle.rds")) %>%
    acsprocess::process_tenure_vehicleown(overall = T) %>%
    dplyr::select(geoid, name, tothous, tothous_moe, anyvehicle, anyveh_est, anyveh_moe, pct_anyveh, pct_moe, pct_upper, pct_lower) %>%
    dplyr::distinct() %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_vehicle.rds"))
  
  readRDS(glue::glue("{dirfiles}/vars_pop.rds")) %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_pop.rds"))
  
  read_rds(glue::glue("{dirfiles}/vars_tenure.rds")) %>%
    acsprocess::process_tenure_df() %>%
    saveRDS(glue::glue("{dirprocess}/process_vars_tenure.rds"))
  
}

# at different geometries - read in/save
walk(c("block group", "tract", "place", "county"), ~ process_dfs(.x))
