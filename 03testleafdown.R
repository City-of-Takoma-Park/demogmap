# purpose: experiment with leafdown map

library(tidyverse)
library(leaflet)
library(leafletwrappers)

# md geogs
md_bg <- read_rds("./data/2020/block group/processed/acs_shp.rds")
md_place <- read_rds("./data/2020/place/processed/acs_shp.rds")
md_county <- read_rds("./data/2020/county/processed/acs_shp.rds")

leafdown::Leafdown$new(spdfs_list = list(
  md_county, md_place, md_bg
), input = input, join_map_levels_by = c("countyfp" = "countyfp", ))

