library(leaflet)
library(leafletwrappers)
library(sf)
library(tidyverse)
library(acsprocess)


pivotfunct <- function(df, namescol) {
  
  colspct <- grep("pct", colnames(df), value = T) %>%
    grep("(upper)|(lower)|(moe)", ., value = T, invert = T)
  
  df <- df %>%
    rename(pct := !!dplyr::sym(colspct))
  
  print(colspct)
  
  totcol <- grep("(tot)|(tenure_overall)", colnames(df), value = T)
  
  print(totcol)
  
  df %>%
    tidyr::pivot_wider(id_cols = c("geoid", "name", totcol), names_from = namescol, values_from = c("estimate", pct), names_repair = "universal")
}

# disabdf <- readRDS(glue::glue("./data/2020/block group/processed/process_vars_disab.rds")) %>%
#   dplyr::group_by(geoid, name, disab) %>%
#   dplyr::mutate(n = dplyr::n()) %>%
#   dplyr::filter(n > 1L) %>%
#   arrange(geoid, name, disab) %>%
# 

  # pivotfunct("disab")

readsave_spatial <- function(geom){
  
  agedf <- readRDS(glue::glue("./data/2020/{geom}/processed/process_vars_agesex.rds")) %>%
    pctna_recode(totcol = tot_people, colname = pct_age) %>%
    # rename(estimate = age_tot_est,
    #        moe = age_tot_moe) %>%
    pivotfunct( "agegrp")
  
  

  
  povdf <- readRDS(glue::glue("./data/2020/{geom}/processed/process_vars_pov_ratio.rds")) %>%
    incpov_recode(incpov_ratio) %>%
    group_by(name) %>%
    arrange(incpov_new) %>%
    dplyr::mutate(estimate = cumsum(estimate),
                  pct_incpov = estimate/ pop_tot) %>%
    dplyr::ungroup() %>%
    dplyr::filter(incpov_new %in% c(".50 to .99", "1.85 to 1.99")) %>%
    dplyr::mutate(incpov_ratio = dplyr::case_when(
      incpov_new == ".50 to .99" ~ "Under poverty line",
      incpov_new == "1.85 to 1.99"~ "Under 2X poverty line"
    )) %>%
    pctna_recode(totcol = pop_tot, colname = pct_incpov) %>%
    pivotfunct("incpov_ratio")
  
  vehicledf <- readRDS(glue::glue("./data/2020/{geom}/processed/process_vars_vehicle.rds")) %>%
    rename(estimate = anyveh_est,
           moe = anyveh_moe) %>%
    pctna_recode(totcol = tothous, colname = pct_anyveh) %>%
    pivotfunct("anyvehicle")
  
  racedf <- readRDS(glue::glue("./data/2020/place/processed/process_vars_race.rds")) %>%
    filter(!grepl("Not Hispanic", race_ethnicity)) %>%
    acsprocess::race_recode(race_col = race_ethnicity)%>%
    pctna_recode(totcol = pop_total, colname = pct_race) %>%
    pivotfunct("race")
  
  tenuredf <- readRDS(glue::glue("./data/2020/place/processed/process_vars_tenure.rds")) %>%
    pctna_recode(totcol = tenure_overall, colname = pct_tenure) %>%
    pivotfunct("tenure")
  
  popdf <- readRDS(glue::glue("./data/2020/{geom}/processed/process_vars_pop.rds")) %>%
    rename(estimate_Population = estimate)%>%
    select(-c(label, concept, variable, moe, geography))
  
  mdbg_2020 <- read_rds(glue::glue("./data/md{geom}_2020.rds")) %>%
    rename_all(tolower) %>%
    st_transform(4326) 
  
  joineddf <- mdbg_2020 %>%
    left_join(racedf) %>%
    left_join(povdf) %>%
    left_join(tenuredf) %>%
    left_join(vehicledf) %>%
    left_join(agedf) %>%
    left_join(popdf)
  
  saveRDS(joineddf, glue::glue("./data/2020/{geom}/processed/acs_shp.rds"))
  
}

walk(c("block group", "tract", "place", "county"), ~ readsave_spatial(.x))

# recode pct values if total pop less than 10 to avoid distortions
pctna_recode <- function(df, totcol, colname) {
  df %>%
    dplyr::mutate({{colname}} := ifelse({{totcol}} < 10, NA, {{colname}}))
}

testfuncts <- function(df, tot, col){
  
  viscol <- sym(col)
  
  df %>%
    pctna_recode(totcol = {{tot}}, colname = !!viscol)
}

testfuncts

racedf %>%
  testfuncts(pop_total, "pct_race")

  
mdplace_2020 <- read_rds("./data/mdplace_2020.rds")  %>%
  rename_all(tolower) %>%
  st_transform(4326)

mdcounties_2020 <- read_rds("./data/counties_2020.rds")  %>%
  rename_all(tolower) %>%
  st_transform(4326)

mdtract_2020 <- read_rds("./data/mdct_2020.rds")  %>%
  rename_all(tolower) %>%
  st_transform(4326)



filterrace <- racedf %>%
  filter(race == "Hispanic") 

labtext <- "Population: {pop_total %>% tpfuncts::commafy()}{p}Race total: {estimate %>% tpfuncts::commafy()}{p}Percent race: {pct_race_nacode %>% round(0)}%"


labs <- leafletwrappers::label_output(st_drop_geometry(filterrace), label_text = labtext)

geomrace <- mdbg_2020 %>%
  left_join(filterrace, by = "geoid")


palbin <- leaflet::colorBin(palette = "Blues", bins = 10,domain = range(geomrace$pct_race_nacode))

palquantile <- leaflet::colorQuantile(palette = "Blues", domain = range(geomrace$pct_race_nacode))
quantile(geomrace$pct_race_nacode, probs = seq(0, 1, 0.05))

p <- "<p></p>"

labs <- label_output(filterdf, label_text = "Population: {pop_total %>% tpfuncts::commafy()}{p}Race total: {estimate %>% tpfuncts::commafy()}{p}Percent race: {pct_race_nacode %>% round(0)}%")

acsshp_place <- read_rds("./data/2020/block group/processed/acs_shp.rds")

addpoly_df <- function(leafletobj, acsdata, colvisualize, totcol = estimate_Population, labtext, geogfile, colors = "Blues",  title = NULL){
  
  p <- "<p></p>"
  
  labs <- leafletwrappers::label_output(st_drop_geometry(acsdata), label_text = labtext)
  
  palnumeric <- leaflet::colorNumeric(domain = acsdata[[colvisualize]], na.color = "#e3e3e3", palette = colors)
  
  grp <- gsub("(estimate_)|(pct_)")
  
  title <- dplyr::case_when(
    # !is.null(title) ~ title,
    colvisualize == "estimate" ~ paste0("Total ", tolower(filtval)),
    T ~ paste0("Percent ", tolower(filtval))
  )
  
  # browser()
  
  leafletobj %>%
    leafletwrappers::addpoly_legend(
      df_select = sf::st_drop_geometry(acsdata),
      pal_funct_select = palnumeric, 
      variable_select = colvisualize, 
      group_select = filtval,
      labels_select = labs,
      title_select = title
    )
  
}

addpoly_race <- function(leafletobj, racedf, racefilt, geogfile, pctest = "pct_race") {
  
  labtext <- "Population: {pop_total %>% tpfuncts::commafy()}{p}Race total: {estimate %>% tpfuncts::commafy()}{p}Percent race: {pct_race_nacode %>% round(0)}%"
  
  leafletobj %>%
    addpoly_df(overalldf = racedf, filtcol = race,geogfile = geogfile, filtval = racefilt, labtext = labtext, colors = "Blues", totcol = pop_total, title = NULL, colvisualize = pctest)
  
}

addgeog_set <- function(geogfile){
  leaflet(geogfile) %>%
    
}

leaflet(mdbg_2020) %>%
  addTiles() 

leaflet(geomrace) %>%
  addProviderTiles(provider = providers$CartoDB) %>%


addbounds <- function(leafletobj, plcgrp = "Place boundaries", spatialdf = mdplace_2020){
  leafletobj %>%
    leaflet::addPolygons(
      stroke = T, 
      color = "black", 
      weight = 0.5, 
      opacity = 0.5, 
      group = plcgrp,
      fill = F, 
      labelOptions = labelOptions(
        noHide = T, 
        direction = "center", 
        textOnly = T, 
        style = list(
          `font-weight` = "bold", 
          padding = "1px 1px", 
          textsize = "9px")
      ), 
      data = spatialdf)
}


    