#### Purpose: pivot non-spatial ACS data to wider dataframes; join to spatial dataframes at different geograpgies
#### last run: 08/05/2022

library(leaflet)
library(leafletwrappers)
library(sf)
library(tidyverse)
library(acsprocess)
library(acsmapping)

# acs mapping for md ----

acsmapping::prepallinone_acsmapoverall(tidystate = "Maryland", downloadgeog = T)

prepallinone_acsmapoverall(tidystate = "Maryland", geogs = c("county subdivision"), downloadgeog = T)

acscounty_shp <- read_rds("./data/2020/MD/county subdivision/processed/acs_shp.rds")

acsbg_shp <- read_rds("./data/2020/MD/block group/processed/acs_shp.rds")

# read/join non-spatial data to spatial ----

# function to pivot data for joining to spatial file
pivotfunct <- function(df, namescol) {
  
  colspct <- grep("pct", colnames(df), value = T) %>%
    grep("(upper)|(lower)|(moe)", ., value = T, invert = T)
  
  df <- df %>%
    dplyr::rename(pct := !!dplyr::sym(colspct))
  
  print(colspct)
  
  totcol <- grep("(tot)|(tenure_overall)", colnames(df), value = T)
  
  print(totcol)
  
  df %>%
    tidyr::pivot_wider(
      id_cols = c("geoid", "name", totcol), 
      names_from = namescol, 
      values_from = c("estimate", pct), names_repair = "universal")}

# recode pct values if total pop less than 10 to avoid distortions
pctna_recode <- function(df, totcol, colname) {
  df %>%
    dplyr::mutate({{colname}} := ifelse({{totcol}} < 10, NA, {{colname}}))
}

# disabdf <- readRDS(glue::glue("./data/2020/block group/processed/process_vars_disab.rds")) %>%
#   dplyr::group_by(geoid, name, disab) %>%
#   dplyr::mutate(n = dplyr::n()) %>%
#   dplyr::filter(n > 1L) %>%
#   arrange(geoid, name, disab) %>%
# 

# pivotfunct("disab")

test_data <-   read_rds(glue::glue("./data/mdblock group_2020.rds")) %>%
  rename_all(tolower) %>%
  st_transform(4326) 


# read each acs data in; pivot; and join to sptail data
# geom = geometry of data reading in/joining to
readsave_spatial <- function(geom){
  
  if (!dir.exists(glue::glue("./data/2020/{geom}/processed"))){
    dir.create(glue::glue("./data/2020/{geom}/processed"))
  }
  
  geompath <- glue::glue("./data/2020/{geom}/processed")
  
  agedf <- readRDS(glue::glue("{geompath}/process_vars_agesex.rds")) %>%
    pctna_recode(totcol = tot_people, colname = pct_age) %>%
    # rename(estimate = age_tot_est,
    #        moe = age_tot_moe) %>%
    pivotfunct( "agegrp")
  
  povdf <- readRDS(glue::glue("{geompath}/process_vars_pov_ratio.rds")) %>%
    pctna_recode(totcol = pop_tot, colname = pct_incpov) %>%
    pivotfunct("incpov_ratio")
  
  vehicledf <- readRDS(glue::glue("{geompath}/process_vars_vehicle.rds")) %>%
    rename(estimate = anyveh_est,
           moe = anyveh_moe) %>%
    pctna_recode(totcol = tothous, colname = pct_anyveh) %>%
    pivotfunct("anyvehicle")
  
  racedf <- readRDS(glue::glue("{geompath}/process_vars_race.rds")) %>%
    filter(!grepl("Not Hispanic", race_ethnicity)) %>%
    acsprocess::race_recode(race_col = race_ethnicity)%>%
    pctna_recode(totcol = pop_total, colname = pct_race) %>%
    pivotfunct("race")
  
  tenuredf <- readRDS(glue::glue("{geompath}/process_vars_tenure.rds")) %>%
    pctna_recode(totcol = tenure_overall, colname = pct_tenure) %>%
    pivotfunct("tenure")
  
  popdf <- readRDS(glue::glue("{geompath}/process_vars_pop.rds")) %>%
    rename(estimate_Population = estimate)%>%
    select(-c(label, concept, variable, moe, geography))
  
  mdbg_2020 <- read_rds(glue::glue("./data/md{geom}_2020.rds")) %>%
    rename_all(tolower) %>%
    st_transform(4326) 
  
  # if (grepl("place", geom)){
  #   browser()
  # }
  
  joineddf <- mdbg_2020 %>%
    left_join(povdf, by = "geoid") %>%
    left_join(tenuredf, by = "geoid") %>%
    left_join(vehicledf, by = "geoid") %>%
    left_join(agedf, by = "geoid") %>%
    left_join(popdf, by = "geoid") %>%
    left_join(racedf, by = "geoid")
  
  saveRDS(joineddf, glue::glue("{geompath}/acs_shp.rds"))
  
}

# run for each geography
purrr::walk(c("block group", "tract", "place", "county"), ~ readsave_spatial(.x))



# TAKOMA PARK ONLY ----

# read in tracts, block groups; intersect with ward boundaries
acsbg <- read_rds("./data/2020/block group/processed/acs_shp.rds")


acsshp_county <- read_rds("./data/mdcounty_2020.rds") %>%
  st_transform(4326)

  
acsshp_place <- read_rds("./data/mdplace_2020.rds") %>%
  st_transform(4326)

acsshp_place_processed <- read_rds("./data/2020/MD/place/processed/acs_shp.rds")

testplace <- acsshp_place_processed %>%
  sf::st_intersection(acsshp_county %>%
                        rename(countyfp = COUNTYFP) %>%
                        dplyr::select(
                          countyfp
                        ))

splitplace <- testplace %>%
  sf::st_drop_geometry() %>%
  dplyr::select(geoid, countyfp) %>%
  dplyr::group_by(geoid) %>%
  dplyr::mutate(numcounties = n())

testplace_join <- acsshp_place_processed %>%
  left_join(splitplace)

prepleafdown_placecounty(tidyyear = 2020, tidystate = "Maryland", basedir = "./data", .geomyear = 2020)

tiegeogs <- function(processedgeog_df, basegeog_df, commoncol, numcol){
  
  # browser()
  
  tiedf <- basegeog_df %>%
    sf::st_transform(4326) %>%
    dplyr::rename_all(tolower) %>%
    dplyr::select(dplyr::all_of(commoncol)) 
  
  splitdf <- processedgeog_df %>% 
    sf::st_intersection(tiedf) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(geoid, dplyr::all_of(commoncol)) %>%
    dplyr::group_by(geoid) %>%
    dplyr::mutate(!!dplyr::sym(numcol) := n()) %>%
    dplyr::distinct()
  
  outjoin <- processedgeog_df %>%
    dplyr::left_join(splitdf)
  
  outjoin
  
}

prepleafdown <- function(processgeog = "place", tiegeog = "county", tidyyear = 2020, tidystate = "Maryland", basedir = "./data", .geomyear = tidyyear){
  
  # browser()
  
  stateabr <- usa::states %>%
    dplyr::filter(name == tidystate) %>%
    dplyr::pull(abb)
  
  dirloc_process <- glue::glue("{basedir}/{tidyyear}/{stateabr}/{processgeog}/processed")
  
  processdf <- readr::read_rds(glue::glue("{dirloc_process}/acs_shp.rds")) %>%
    sf::st_transform(4326)

  outdf <- processdf
  
  purrr::walk(tiegeog, ~{
    
    basedf <- readr::read_rds(glue::glue("{basedir}/{.x}_{tidyyear}_{stateabr}.rds"))
    
    if (.x == "county subdivision"){
      
      commoncol <- "cousubfp"
      
      numcol <- "cousubnum"
      
    }
    
    else{
      
      commoncol <- paste0(.x, "fp")
      
      numcol <- paste0(.x, "num")
      
    }
    outdf <<- tiegeogs(processedgeog_df = outdf, basegeog_df = basedf, commoncol = commoncol, numcol = numcol)
    
  })
  
  readr::write_rds(outdf, glue::glue("{dirloc_process}/acsshp_leafdown.rds"))
  
}

prepleafdown()

prepleafdown(processgeog = "tract", tiegeog = c("place", "county subdivision"))

prepleafdown(processgeog = "block group", tiegeog = c("place", "county subdivision", "tract"))



acstract <- read_rds("./data/2020/tract/processed/acs_shp.rds")

tp_wards <- leafletwrappers::wards_new %>%
  st_transform(4326)

bg_exclude <- c("240317020003", "240317019001", "240317024012", "240317024023", "240317025021")

bg_tp <- acsbg %>%
  st_filter(y = tp_wards)  %>%
  filter(!geoid %in% bg_exclude)

# gen_acs_map(bg_tp)

tpbgs <- bg_tp$geoid

saveRDS(bg_tp, "./data/2020/takomapark/bg_tp.rds")

saveRDS(bg_tp, "./tpdemographicmap//data/bg_tp.rds")


tract_exclude <- c("24031702401", "24031702402", "24031702502", "24031702502", "24031702502", "24031702000", "24031701900")

tract_tp <- acstract %>%
  st_filter(y = tp_wards)  %>%
  filter(!geoid %in% tract_exclude)

# gen_acs_map(tract_tp) %>%
#   add_wards_new()

tptracts <- tract_tp$geoid

saveRDS(tract_tp, "./data/2020/takomapark/tract_tp.rds")

saveRDS(tract_tp, "./tpdemographicmap/data/tract_tp.rds")

dc_tract <- readRDS("./data/2020/DC/tract/processed/acs_shp.rds")

mont_tract <- acstract %>%
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

saveRDS(mont_tract, "./data/2020/takomapark/mont_tract.rds")

saveRDS(mont_tract, "./tpdemographicmap/data/mont_tract.rds")

dc_bg <- readRDS("./data/2020/DC/block group/processed/acs_shp.rds")


mont_bg <- acsbg %>%
  filter(
    countyfp %in% c("031", "033")
  ) %>%
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

saveRDS(mont_bg, "./data/2020/takomapark/mont_bg.rds")

saveRDS(mont_bg, "./tpdemographicmap/data/mont_bg.rds")


acsplace <- read_rds("./data/2020/place/processed/acs_shp.rds")

acscounty <- read_rds("./data/2020/county/processed/acs_shp.rds")

montborders <- acscounty %>%
  dplyr::filter(countyfp %in% c("031", "033")) %>%
  select(namelsad, countyfp)

dc_place <- readRDS("./data/2020/DC/place/processed/acs_shp.rds")

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

saveRDS(placecounty, "./data/2020/takomapark/mont_place.rds")

saveRDS(placecounty, "./tpdemographicmap//data/mont_place.rds")




# mont_place <- acsplace %>%
#   filter

place_tp <- acsplace %>%
  filter(grepl("Takoma Park", name.x))

saveRDS(place_tp, "./data/2020/takomapark/place_tp.rds")

saveRDS(place_tp, "./tpdemographicmap/data/place_tp.rds")


# saveRDS(place_tp, "./data/place_tp.rds")


place_tp$
  
  tp_grps <- c(leaflet_grps, "Ward boundaries")

tp_bg <- gen_acs_map(bg_tp, F) %>%
  add_wards_new() %>%
  layercontrolsquick(groups = tp_grps)

tp_tracts <- gen_acs_map(tract_tp, F) %>%
  add_wards_new() %>%
  layercontrolsquick(groups = tp_grps)

tp_place <- gen_acs_map(place_tp, F) %>%
  add_wards_new() %>%
  layercontrolsquick(groups = tp_grps)

# gen dc
acsmapping::prepallinone_acsmapoverall(tidystate = "District of Columbia")

acsmapping::prepallinone_acsmapoverall(tidystate = "District of Columbia")

acsmapping::loadtidydata_acsmapoverall(tidystate = "District of Columbia", downloadgeog = T)

acsmapping::readprocessdata_acsmapoverall(tidystate = "District of Columbia")

purrr::walk(c("block group", "place", "tract", "county"), ~ acsmapping::readsave_spatial_acsmapoverall(.x, tidystate = "District of Columbia"))

acsmapping::readprocessdata_acsmapoverall(tidystate = "Maryland")

# CREATE FUNCTIONS TO VISUALIZE DATA ----

# labtext <- "Population: {pop_total %>% tpfuncts::commafy()}{p}Race total: {estimate %>% tpfuncts::commafy()}{p}Percent race: {pct_race_nacode %>% round(0)}%"

# labs <- leafletwrappers::label_output(st_drop_geometry(filterrace), label_text = labtext)

# geomrace <- mdbg_2020 %>%
#   left_join(filterrace, by = "geoid")

# palbin <- leaflet::colorBin(palette = "Blues", bins = 10,domain = range(geomrace$pct_race_nacode))
# 
# palquantile <- leaflet::colorQuantile(palette = "Blues", domain = range(geomrace$pct_race_nacode))
# quantile(geomrace$pct_race_nacode, probs = seq(0, 1, 0.05))

# p <- "<p></p>"
# 
# labs <- label_output(filterdf, label_text = "Population: {pop_total %>% tpfuncts::commafy()}{p}Race total: {estimate %>% tpfuncts::commafy()}{p}Percent race: {pct_race_nacode %>% round(0)}%")
# 

# read in place data with merged field
acsshp_place <- read_rds("./data/2020/place/processed/acs_shp.rds")


# # standard function for visualizng acs variables
# # leaflet obj is base leaflet map adding layer to; colvisualize is column visualizing in data, as string; labtext is text fed to leafletwrappers::label_output() label_text parameter; acsdata is acs5 shapefile at given geometry, default to grab data from leaflet object; totcol is data-masked name of column representing universe of data for this layer (e.g., population, households), default estimate_Population; colors is fed to palette parameter of colorNumeric() function, default "Blues"; title is legend title, default null meaning it's derived from the variable name
# addpoly_df <- function(leafletobj, colvisualize, labtext, acsdata = leaflet::getMapData(leafletobj), totcol = estimate_Population, colors = "Blues",  title = NULL){
#   
#   # browser()
#   
#   # create labels for dataframe
#   p <- "<p></p>"
#   
#   labs <- leafletwrappers::label_output(st_drop_geometry(acsdata), label_text = labtext)
#   
#   # color polygons based on supplied color set
#   palnumeric <- leaflet::colorNumeric(domain = range(acsdata[[colvisualize]], na.rm = T), na.color = "#e3e3e3", palette = colors)
#   
#   # create grp from name of column
#   grp <- gsub("(estimate_)|(pct_)", "", colvisualize) %>%
#     gsub("\\.", " ", .)
#   
#   if (is.null(title)){
#     # standard title based on column name unless provided
#     title <- dplyr::case_when(
#       grepl("estimate", colvisualize) ~ paste0("Total ", tolower(grp)),
#       T ~ paste0("Percent ", tolower(grp))
#     )
#   }
#   
#   # browser()
#   
#   
#   leafletobj %>%
#     leafletwrappers::addpoly_legend(
#       df_select = sf::st_drop_geometry(acsdata),
#       pal_funct_select = palnumeric, 
#       variable_select = colvisualize, 
#       group_select = grp,
#       labels_select = labs,
#       title_select = title, 
#       .data = acsdata
#     )
#   
# }

# function to add race layer to dataframe
# addpoly_race <- function(leafletobj, colvisualize, acsdata = leaflet::getMapData(leafletobj), sizelabs = "12px") {
#   
#   # p <- "<p></p>"
#   
#   # identify appropriate labeling columns
#   est_col <- dplyr::case_when(
#     grepl("estimate", colvisualize) ~ colvisualize,
#     T ~ gsub("pct_", "estimate_", colvisualize)
#   )
#   pct_col <- dplyr::case_when(
#     grepl("pct_", colvisualize) ~ colvisualize,
#     T ~ gsub("estimate_", "pct_", colvisualize)
#   )
#   
#   labtext <- paste0("Name: {name.x}<p></p>Population: {estimate_Population %>% tpfuncts::commafy()}<p></p>Race total: {", est_col, " %>% tpfuncts::commafy()}<p></p>Percent race: {", pct_col, " %>% round(0)}%")
#   
#   leafletobj %>%
#     addpoly_acs(
#       acsdata = acsdata, 
#       colvisualize = colvisualize, 
#       labtext = labtext, 
#       totcol = estimate_Population,
#       sizelabs = sizelabs,
#       colors = "Blues")
#   
# }

# addgeog_set <- function(geogfile){
#   leaflet::leaflet(geogfile) %>%
#     
# }

bg_acs <- read_rds("./data/2020/place/processed/acs_shp.rds")

# groups to cycle through
grps_race <- c(
  "White",
  "Black",
  "Hispanic",
  "Asian",
  "AIAN",
  "NHPI",
  "Other",
  "Multiracial"
)

grps <- c(
  "Population",
  grps_race,
  "Under poverty line",
  "Under 2X poverty line",
  "Renter occupied",
  "No vehicle",
  "Under 18",
  "65 and over"
)



# quick function for population labels
# quicklab <- function(colsuffix, tot = "estimate_Population"){
#   
#   paste0("Place: {name.x}<p></p>Population: {", tot, " %>% tpfuncts::commafy()}<p></p>", gsub("\\.", " ", colsuffix), " total: {estimate_", colsuffix, " %>% tpfuncts::commafy()}<p></p>Percent ", gsub("\\.", " ", stringr::str_to_lower(colsuffix)), ": {pct_", colsuffix, " %>% round(0)}%")
#   
# } 

# addbounds <- function(leafobj, placetype, spatialdf){
#   leafobj %>%
#     leaflet::addPolygons(
#       group = placetype,
#       fill = F,
#       stroke = T,
#       weight = 0.5,
#       opacity = 0.5,
#       color = "black",
#       # label = ~ NAME,
#       data = spatialdf,
#       labelOptions = labelOptions(
#         noHide = T, 
#         direction = "center", 
#         textOnly = T, 
#         style = list(
#           `font-weight` = "bold", 
#           padding = "1px 1px", 
#           textsize = "9px")
#       )
#     )
# }


# leafobj <- leaflet(bg_acs) %>%
#   addProviderTiles(providers$CartoDB)

# add race layers to data
# walk(grps_race, ~{
#   
#   pct_col <- paste0("pct_", .x)
#   
#   leafobj <<- leafobj %>%
#     addpoly_race(pct_col)
# })
# 
# leafobj <- leafobj %>%
#   addpoly_df(colvisualize = "estimate_Population",
#              labtext = "Name: {name.x}<p></p>Total population: {estimate_Population}") %>%
#   addpoly_df(colvisualize = "pct_Under.poverty.line", 
#              labtext = quicklab("Under.poverty.line"),
#              colors = "Oranges") %>%
#   addpoly_df(colvisualize = "pct_Under.2X.poverty.line", 
#              labtext = quicklab("Under.2X.poverty.line"),
#              colors = "Oranges") %>%
#   addpoly_df(colvisualize = "pct_Renter.occupied", 
#              labtext = quicklab("Renter.occupied", tot = "tothous"),
#              colors = "YlGn", 
#              totcol = "tothous") %>%
#   addpoly_df(colvisualize = "pct_No.vehicle", 
#              labtext = quicklab("No.vehicle", tot = "tothous"),
#              colors = "PuRd", 
#              totcol = "tothous") %>%
#   addpoly_df(colvisualize = "pct_Under.18", 
#              labtext = quicklab("No.vehicle"),
#              colors = "PuRd") %>%
#   addpoly_df(colvisualize = "pct_65.and.over", 
#              labtext = quicklab("65.and.over"),
#              colors = "PuRd") %>%
#   addbounds(placetype = "Place boundaries", spatialdf = acsshp_place) %>%
#   addbounds(placetype = "County boundaries", spatialdf = acsshp_county ) %>%
#   leafletwrappers::layercontrolsquick(
#     groups = c(grps, "Place boundaries", "County boundaries"), 
#     hide = grps[-1]
#   )

# addpoly_acs <- function(leafletobj, colvisualize, labtext, acsdata = leaflet::getMapData(leafletobj), totcol = estimate_Population, colors = "Blues",  title = NULL, sizelabs = "10px"){
#   
#   # browser()
#   
#   # create labels for dataframe
#   p <- "<p></p>"
#   
#   labs <- leafletwrappers::label_output(
#     sf::st_drop_geometry(acsdata),
#     label_text = labtext
#   )
#   
#   # color polygons based on supplied color set
#   palnumeric <- leaflet::colorNumeric(domain = range(acsdata[[colvisualize]], na.rm = T), na.color = "#e3e3e3", palette = colors)
#   
#   # create grp from name of column
#   grp <- gsub("(estimate_)|(pct_)", "", colvisualize) %>%
#     gsub("\\.", " ", .)
#   
#   if (is.null(title)){
#     # standard title based on column name unless provided
#     title <- dplyr::case_when(
#       grepl("estimate", colvisualize) ~ paste0("Total ", tolower(grp)),
#       T ~ paste0("Percent ", tolower(grp))
#     )
#   }
#   
#   leafletobj %>%
#     leafletwrappers::addpoly_legend(
#       df_select = sf::st_drop_geometry(acsdata),
#       pal_funct_select = palnumeric,
#       variable_select = colvisualize,
#       group_select = grp,
#       labels_select = labs,
#       title_select = title,
#       .data = acsdata,
#       .label_textsize = sizelabs
#     )
#   
# }

# gen_acsmap_overall <- function(baseacsdata, pct_est = "pct_", inccountycontrols = T, sizelabs = "12px", ...){
#   # browser()
#   
#   # groups to cycle through
#   grps_race <- c(
#     "White",
#     "Black",
#     "Hispanic",
#     "Asian",
#     "AIAN",
#     "NHPI",
#     "Other",
#     "Multiracial"
#   )
#   
#   grps <- c(
#     "Population",
#     grps_race,
#     "Under poverty line",
#     "Under 2X poverty line",
#     "Renter occupied",
#     "No vehicle",
#     "Under 18",
#     "65 and over"
#   )
#   
#   # initialize leaflet map
#   leafobj <- leaflet::leaflet(baseacsdata) %>%
#     leaflet::addProviderTiles(providers$CartoDB)
#   
#   # add race layers to data
#   purrr::walk(grps_race, ~{
#     
#     pct_col <- paste0(pct_est, "_", .x)
#     
#     leafobj <<- leafobj %>%
#       addpoly_race(pct_col, sizelabs = sizelabs)
#   })
#   
#   leafobj <- leafobj %>%
#     addpoly_acs(
#       colvisualize = "estimate_Population",
#       labtext = "Name: {name.x}<p></p>Total population: {estimate_Population}",
#       sizelabs = sizelabs) %>%
#     addpoly_acs(
#       colvisualize = glue::glue("{pct_est}_Under.poverty.line"),
#       labtext = quicklab("Under.poverty.line"),
#       colors = "Oranges",
#       sizelabs = sizelabs) %>%
#     addpoly_acs(
#       colvisualize = glue::glue("{pct_est}_Under.2X.poverty.line"),
#       labtext = quicklab("Under.2X.poverty.line"),
#       colors = "Oranges",
#       sizelabs = sizelabs) %>%
#     addpoly_acs(
#       colvisualize = glue::glue("{pct_est}_Renter.occupied"),
#       labtext = quicklab("Renter.occupied", tot = "tothous"),
#       colors = "YlGn",
#       totcol = "tothous",
#       sizelabs = sizelabs) %>%
#     addpoly_acs(
#       colvisualize = glue::glue("{pct_est}_No.vehicle"),
#       labtext = quicklab("No.vehicle", tot = "tothous"),
#       colors = "PuRd",
#       totcol = "tothous",
#       sizelabs = sizelabs) %>%
#     addpoly_acs(
#       colvisualize = glue::glue("{pct_est}_Under.18"),
#       labtext = quicklab("No.vehicle"),
#       colors = "PuRd",
#       sizelabs = sizelabs) %>%
#     addpoly_acs(
#       colvisualize = glue::glue("{pct_est}_65.and.over"),
#       labtext = quicklab("65.and.over"),
#       colors = "PuRd",
#       sizelabs = sizelabs)
#   
#   if (inccountycontrols){
#     grps <- c(grps, "Place boundaries", "County boundaries")
#     
#     leafobj <- leafobj %>%
#       leafletwrappers::addbounds(placetype = "Place boundaries", spatialdf = acsshp_place) %>%
#       leafletwrappers::addbounds(placetype = "County boundaries", spatialdf = acsshp_county)
#     
#   }
#   
#   # create object in global environment for grps, so can modify
#   leaflet_grps <<- grps
#   
#   
#   leafobj %>%
#     leafletwrappers::layercontrolsquick(
#       groups = grps,
#       hide = grps[-1],
#       ...
#     )
#   
# }

# gen_acsmap_overall(dc_tract, pct_est = "estimate", sizelabs = "12px")

# gen_acs_map <- function(baseacsdata, pct_est = "pct", inccountycontrols = T){
#   
#   # browser()
#   
#   # groups to cycle through
#   grps_race <- c(
#     "White",
#     "Black",
#     "Hispanic",
#     "Asian",
#     "AIAN",
#     "NHPI",
#     "Other",
#     "Multiracial"
#   )
#   
#   grps <- c(
#     "Population",
#     grps_race,
#     "Under poverty line",
#     "Under 2X poverty line",
#     "Renter occupied",
#     "No vehicle",
#     "Under 18",
#     "65 and over"
#   )
#   
#   # initialize leaflet map
#   leafobj <- leaflet::leaflet(baseacsdata) %>%
#     leaflet::addProviderTiles(providers$CartoDB)
#   
#   # add race layers to data
#   purrr::walk(grps_race, ~{
#     
#     colname <- paste0(pct_est, "_", .x)
#     
#     leafobj <<- leafobj %>%
#       addpoly_race(colname)
#   })
#   
#   leafobj <- leafobj %>%
#     addpoly_df(
#       colvisualize = "estimate_Population",
#       labtext = "Name: {name.x}<p></p>Total population: {estimate_Population}") %>%
#     addpoly_df(
#       colvisualize = glue::glue("{pct_est}_Under.poverty.line"), 
#       labtext = quicklab("Under.poverty.line"),
#       colors = "Oranges") %>%
#     addpoly_df(
#       colvisualize = glue::glue("{pct_est}_Under.2X.poverty.line"), 
#       labtext = quicklab("Under.2X.poverty.line"),
#       colors = "Oranges") %>%
#     addpoly_df(
#       colvisualize = glue::glue("{pct_est}_Renter.occupied"), 
#       labtext = quicklab("Renter.occupied", tot = "tothous"),
#       colors = "YlGn", 
#       totcol = "tothous") %>%
#     addpoly_df(
#       colvisualize = glue::glue("{pct_est}_No.vehicle"), 
#       labtext = quicklab("No.vehicle", tot = "tothous"),
#       colors = "PuRd", 
#       totcol = "tothous") %>%
#     addpoly_df(
#       colvisualize = glue::glue("{pct_est}_Under.18"), 
#       labtext = quicklab("No.vehicle"),
#       colors = "PuRd") %>%
#     addpoly_df(
#       colvisualize = glue::glue("{pct_est}_65.and.over"), 
#       labtext = quicklab("65.and.over"),
#       colors = "PuRd")
#   
#   if (inccountycontrols){
#     grps <- c(grps, "Place boundaries", "County boundaries")
#     
#     leafobj <- leafobj %>%
#       addbounds(placetype = "Place boundaries", spatialdf = acsshp_place) %>%
#       addbounds(placetype = "County boundaries", spatialdf = acsshp_county)
#     
#   }
#   
#   # create object in global environment for grps, so can modify
#   leaflet_grps <<- grps
#   
#   
#   leafobj %>%
#     leafletwrappers::layercontrolsquick(
#       groups = grps, 
#       hide = grps[-1]
#     )
#       
# }

# test map functions

# 
# gen_acs_map(bg_acs, pct_est = "estimate")

# county_borders <- read_rds("./data/2020/county/processed/acs_shp.rds")

# gen_acs_map(county_borders)
