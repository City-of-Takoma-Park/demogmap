#### Purpose: pivot non-spatial ACS data to wider dataframes; join to spatial dataframes at different geograpgies
#### last run: 09/19/2022

library(leaflet)
library(sf)
library(tidyverse)

# download with devtools::install_github("dpowerstp/packagename")
library(leafletwrappers)
library(acsprocess)
library(acsmapping)

acsprocess::quickdircreate(dirpath = "./data")

# update these parameters - acs
yearacs <- 2020
yeargeom <- 2020

# acs mapping for md ----

acsmapping::prepallinone_acsmapoverall(tidystate = "Maryland", downloadgeog = T,tidyyear = yearacs, .geomyear = yeargeom)

# gen dc
acsmapping::prepallinone_acsmapoverall(tidystate = "District of Columbia", tidyyear = yearacs, .geomyear = yeargeom, downloadgeog = T)


acsmapping::prepallinone_acsmapoverall(tidystate = "Maryland", geogs = c("county subdivision"), downloadgeog = T, tidyyear = yearacs, .geomyear = yeargeom)

# acscounty_shp <- read_rds("./data/2020/MD/county subdivision/processed/acs_shp.rds")

# acsbg_shp <- read_rds("./data/2020/MD/block group/processed/acs_shp.rds")

# read/join non-spatial data to spatial ----

# # function to pivot data for joining to spatial file
# pivotfunct <- function(df, namescol) {
#   
#   colspct <- grep("pct", colnames(df), value = T) %>%
#     grep("(upper)|(lower)|(moe)", ., value = T, invert = T)
#   
#   df <- df %>%
#     dplyr::rename(pct := !!dplyr::sym(colspct))
#   
#   print(colspct)
#   
#   totcol <- grep("(tot)|(tenure_overall)", colnames(df), value = T)
#   
#   print(totcol)
#   
#   df %>%
#     tidyr::pivot_wider(
#       id_cols = c("geoid", "name", totcol), 
#       names_from = namescol, 
#       values_from = c("estimate", pct), names_repair = "universal")}

# recode pct values if total pop less than 10 to avoid distortions
# pctna_recode <- function(df, totcol, colname) {
#   df %>%
#     dplyr::mutate({{colname}} := ifelse({{totcol}} < 10, NA, {{colname}}))
# }

# disabdf <- readRDS(glue::glue("./data/2020/block group/processed/process_vars_disab.rds")) %>%
#   dplyr::group_by(geoid, name, disab) %>%
#   dplyr::mutate(n = dplyr::n()) %>%
#   dplyr::filter(n > 1L) %>%
#   arrange(geoid, name, disab) %>%
# 

# pivotfunct("disab")

# test_data <-   read_rds(glue::glue("./data/mdblock group_2020.rds")) %>%
#   rename_all(tolower) %>%
#   st_transform(4326) 


# read each acs data in; pivot; and join to sptail data
# geom = geometry of data reading in/joining to
# readsave_spatial <- function(geom){
#   
#   if (!dir.exists(glue::glue("./data/2020/{geom}/processed"))){
#     dir.create(glue::glue("./data/2020/{geom}/processed"))
#   }
#   
#   geompath <- glue::glue("./data/2020/{geom}/processed")
#   
#   agedf <- readRDS(glue::glue("{geompath}/process_vars_agesex.rds")) %>%
#     pctna_recode(totcol = tot_people, colname = pct_age) %>%
#     # rename(estimate = age_tot_est,
#     #        moe = age_tot_moe) %>%
#     pivotfunct( "agegrp")
#   
#   povdf <- readRDS(glue::glue("{geompath}/process_vars_pov_ratio.rds")) %>%
#     pctna_recode(totcol = pop_tot, colname = pct_incpov) %>%
#     pivotfunct("incpov_ratio")
#   
#   vehicledf <- readRDS(glue::glue("{geompath}/process_vars_vehicle.rds")) %>%
#     rename(estimate = anyveh_est,
#            moe = anyveh_moe) %>%
#     pctna_recode(totcol = tothous, colname = pct_anyveh) %>%
#     pivotfunct("anyvehicle")
#   
#   racedf <- readRDS(glue::glue("{geompath}/process_vars_race.rds")) %>%
#     filter(!grepl("Not Hispanic", race_ethnicity)) %>%
#     acsprocess::race_recode(race_col = race_ethnicity)%>%
#     pctna_recode(totcol = pop_total, colname = pct_race) %>%
#     pivotfunct("race")
#   
#   tenuredf <- readRDS(glue::glue("{geompath}/process_vars_tenure.rds")) %>%
#     pctna_recode(totcol = tenure_overall, colname = pct_tenure) %>%
#     pivotfunct("tenure")
#   
#   popdf <- readRDS(glue::glue("{geompath}/process_vars_pop.rds")) %>%
#     rename(estimate_Population = estimate)%>%
#     select(-c(label, concept, variable, moe, geography))
#   
#   mdbg_2020 <- read_rds(glue::glue("./data/md{geom}_2020.rds")) %>%
#     rename_all(tolower) %>%
#     st_transform(4326) 
#   
#   # if (grepl("place", geom)){
#   #   browser()
#   # }
#   
#   joineddf <- mdbg_2020 %>%
#     left_join(povdf, by = "geoid") %>%
#     left_join(tenuredf, by = "geoid") %>%
#     left_join(vehicledf, by = "geoid") %>%
#     left_join(agedf, by = "geoid") %>%
#     left_join(popdf, by = "geoid") %>%
#     left_join(racedf, by = "geoid")
#   
#   saveRDS(joineddf, glue::glue("{geompath}/acs_shp.rds"))
#   
# }

# run for each geography
# purrr::walk(c("block group", "tract", "place", "county"), ~ readsave_spatial(.x))

# TAKOMA PARK ONLY ----

# read in tracts, block groups; intersect with ward boundaries
acsbg <- read_rds(glue::glue("./data/{yearacs}/MD/block group/processed/acs_shp.rds"))

acsshp_county <- read_rds(glue::glue("./data/county_{yeargeom}_MD.rds")) %>%
  st_transform(4326)
  
acsshp_place <- read_rds(glue::glue("./data/place_{yeargeom}_MD.rds")) %>%
  st_transform(4326)

acsshp_place_processed <- read_rds(glue::glue("./data/{yearacs}/MD/place/processed/acs_shp.rds"))

# intersect places with counties to get county id for palces
testplace <- acsshp_place_processed %>%
  sf::st_intersection(acsshp_county %>%
                        rename(countyfp = COUNTYFP) %>%
                        dplyr::select(
                          countyfp
                        ))

# calculate number of counties associated with each place
splitplace <- testplace %>%
  sf::st_drop_geometry() %>%
  dplyr::select(geoid, countyfp) %>%
  dplyr::group_by(geoid) %>%
  dplyr::mutate(numcounties = n())

# join county data to place shapefile
testplace_join <- acsshp_place_processed %>%
  left_join(splitplace)

# function to associate common geographies
# prepleafdown <- function(processgeog = "place", tiegeog = "county", tidyyear = yearacs, tidystate = "Maryland", basedir = "./data", .geomyear = tidyyear){
# 
#   # browser()
# 
#   stateabr <- usa::states %>%
#     dplyr::filter(name == tidystate) %>%
#     dplyr::pull(abb)
# 
#   dirloc_process <- glue::glue("{basedir}/{tidyyear}/{stateabr}/{processgeog}/processed")
# 
#   processdf <- readr::read_rds(glue::glue("{dirloc_process}/acs_shp.rds")) %>%
#     sf::st_transform(4326)
# 
#   outdf <- processdf
# 
#   purrr::walk(tiegeog, ~{
# 
#     basedf <- readr::read_rds(glue::glue("{basedir}/{.x}_{tidyyear}_{stateabr}.rds"))
# 
#     if (.x == "county subdivision"){
# 
#       commoncol <- "cousubfp"
# 
#       numcol <- "cousubnum"
# 
#     }
# 
#     else{
# 
#       commoncol <- paste0(.x, "fp")
# 
#       numcol <- paste0(.x, "num")
# 
#     }
#     outdf <<- tiegeogs(processedgeog_df = outdf, basegeog_df = basedf, commoncol = commoncol, numcol = numcol)
# 
#   })
# 
#   readr::write_rds(outdf, glue::glue("{dirloc_process}/acsshp_leafdown.rds"))
# 
# }

# functon to associate common geographies
# tiegeogs <- function(processedgeog_df, basegeog_df, commoncol, numcol){
# 
#   # browser()
# 
#   tiedf <- basegeog_df %>%
#     sf::st_transform(4326) %>%
#     dplyr::rename_all(tolower) %>%
#     dplyr::select(dplyr::all_of(commoncol))
# 
#   splitdf <- processedgeog_df %>%
#     sf::st_intersection(tiedf) %>%
#     sf::st_drop_geometry() %>%
#     dplyr::select(geoid, dplyr::all_of(commoncol)) %>%
#     dplyr::group_by(geoid) %>%
#     dplyr::mutate(!!dplyr::sym(numcol) := n()) %>%
#     dplyr::distinct()
# 
#   outjoin <- processedgeog_df %>%
#     dplyr::left_join(splitdf)
# 
#   outjoin
# 
# }

# prepleafdown(tidyyear = yearacs, tidystate = "Maryland", basedir = "./data", .geomyear = yeargeom)
# 
# prepleafdown(processgeog = "tract", tiegeog = "county", tidyyear = yearacs, tidystate = "Maryland", basedir = "./data", .geomyear = yeargeom)


# 
# prepleafdown()
# 
# prepleafdown(processgeog = "tract", tiegeog = c("place", "county subdivision"))
# 


# prepleafdown(processgeog = "block group", tiegeog = c("place"), tidyyear = yearacs, .geomyear = yeargeom)
# 

# read in md trac data
acstract <- read_rds(glue::glue("./data/{yearacs}/MD/tract/processed/acs_shp.rds"))

# pull in takoma park ward data
tp_wards <- leafletwrappers::wards_new %>%
  st_transform(4326)

# block groups that are not part of takoma park but are caught in intersection
bg_exclude <- c("240317020003", "240317019001", "240317024012", "240317024023", "240317025021")

bg_tp <- acsbg %>%
  st_filter(y = tp_wards)  %>%
  filter(!geoid %in% bg_exclude)

# gen_acs_map(bg_tp)

tpbgs <- bg_tp$geoid
# create takoma park specific dir
quickdircreate(glue::glue("./data/{yearacs}/takomapark"))

# save to tp folder
saveRDS(bg_tp, glue::glue("./data/{yearacs}/takomapark/bg_tp.rds"))

quickdircreate("./tpdemographicmap/data/")

saveRDS(bg_tp, "./tpdemographicmap/data/bg_tp.rds")

# tracts that intersect with tp but aren't in tp
tract_exclude <- c("24031702401", "24031702402", "24031702502", "24031702502", "24031702502", "24031702000", "24031701900")

# filter out those tracts after intersection
tract_tp <- acstract %>%
  st_filter(y = tp_wards)  %>%
  filter(!geoid %in% tract_exclude)

# gen_acs_map(tract_tp) %>%
#   add_wards_new()

tptracts <- tract_tp$geoid

saveRDS(tract_tp, glue::glue("./data/{yearacs}/takomapark/tract_tp.rds"))

# save to app
saveRDS(tract_tp, glue::glue("./tpdemographicmap/data/tract_tp.rds"))

# rad in dc tract data
dc_tract <- readRDS(glue::glue("./data/{yearacs}/DC/tract/processed/acs_shp.rds")) %>%
  mutate(countyfp = "001")

# prepleafdown_placecounty(tidyyear = 2020, tidystate = "Maryland", basedir = "./data", .geomyear = 2020)

# # read in tract data with countyfps associated with it
# acstract <- read_rds(glue::glue("./data/{yearacs}/MD/tract/processed/acsshp_leafdown.rds")) %>%
#   select(-countynum)

# filter tracts to montgomery or prince george's county - and bind dc tracts to it
mont_tract <- acstract %>%
  # select(-countynum) %>%
  filter(
    countyfp %in% c("031", "033")
  ) %>%
  select(
    -colnames(.)[!colnames(.) %in% colnames(dc_tract)]
  ) %>%
  rbind(dc_tract %>%
          select(
            -colnames(dc_tract)[!colnames(dc_tract) %in% colnames(acstract)])) %>%
  mutate(
    tp_col = geoid %in% tptracts
  )

saveRDS(mont_tract, glue::glue("./data/{yearacs}/takomapark/mont_tract.rds"))

saveRDS(mont_tract, glue::glue("./tpdemographicmap/data/mont_tract.rds"))

dc_bg <- readRDS(glue::glue("./data/{yearacs}/DC/block group/processed/acs_shp.rds"))

mont_bg <- acsbg %>%
  filter(
    countyfp %in% c("031", "033")
  ) %>%
  select(
    -colnames(.)[!colnames(.) %in% colnames(dc_bg)]
  ) %>%
  rbind(dc_bg %>%
          select(
            -colnames(.)[!colnames(.) %in% colnames(acsbg)]))  %>%
  mutate(
    tp_col = geoid %in% tpbgs
  )

# save block groups with dc
saveRDS(mont_bg, glue::glue("./data/{yearacs}/takomapark/mont_bg.rds"))

saveRDS(mont_bg, "./tpdemographicmap/data/mont_bg.rds")

# read in place and county data
acsplace <- read_rds(glue::glue("./data/{yearacs}/MD/place/processed/acs_shp.rds"))

acscounty <- read_rds(glue::glue("./data/{yearacs}/MD/county/processed/acs_shp.rds"))

montborders <- acscounty %>%
  dplyr::filter(countyfp %in% c("031", "033")) %>%
  select(namelsad, countyfp)

dc_place <- readRDS(glue::glue("./data/{yearacs}/DC/place/processed/acs_shp.rds"))

# identify places interesctiong with motngomery/pg county
placecounty <- acsplace %>%
  st_intersection(montborders)  %>%
  # select(
  #   -colnames(.)[!colnames(.) %in% colnames(dc_place)]
  # ) %>%
  dplyr::bind_rows(dc_place %>%
                     select(
                       -colnames(.)[!colnames(.) %in% colnames(acsplace)]) %>%
                     mutate(countyfp = "001")) %>%
  mutate(
    tp_col= grepl("Takoma Park", name.x)
  )

# gen_acs_map(placecounty)

# save data
saveRDS(placecounty, glue::glue("./data/{yearacs}/takomapark/mont_place.rds"))

saveRDS(placecounty, "./tpdemographicmap/data/mont_place.rds")


# mont_place <- acsplace %>%
#   filter

# save takoma park only
place_tp <- acsplace %>%
  filter(grepl("Takoma Park", name.x))

saveRDS(place_tp, glue::glue("./data/{yearacs}/takomapark/place_tp.rds"))

saveRDS(place_tp, "./tpdemographicmap/data/place_tp.rds")
