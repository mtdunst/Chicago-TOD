# =============================================================================
# Chicago TOD: expressway helpers
#
# Shared by the Room to Grow pipeline (to drop sites that sit on expressway
# right-of-way) and tod_11_station_types.R (for the expressway penalty), so
# both use the same expressway layer and names.
#
#   source("tod_xway.R")
#   xw  <- load_expressways(crs = st_crs(sites), near = sites)
#   pot <- drop_expressway_sites(pot, xw)
# =============================================================================

library(tidyverse)
library(sf)

xway_cache <- "out/primary_roads.rds"

# Census TIGER primary roads (limited-access highways), cached after the
# first download. Ramps aren't in this layer.
load_expressways <- function(crs, near = NULL, pad_ft = 3000) {
  if (!file.exists(xway_cache)) {
    dir.create(dirname(xway_cache), showWarnings = FALSE, recursive = TRUE)
    saveRDS(tigris::primary_roads(year = 2023), xway_cache)
  }
  xw <- readRDS(xway_cache) %>% st_transform(crs)
  
  if (!is.null(near)) {
    box <- st_as_sfc(st_bbox(near)) %>% st_buffer(pad_ft)
    xw  <- xw[lengths(st_intersects(xw, box)) > 0, ]
  }
  
  # Names from route number and location. The splits near the Edens
  # junction and the Skyway are approximate.
  cc <- xw %>% st_geometry() %>% st_centroid() %>% st_transform(4326) %>% st_coordinates()
  xw %>%
    mutate(
      rt  = str_match(FULLNAME, "I-\\s*(\\d+)")[, 2],
      lat = cc[, 2], lon = cc[, 1],
      xname = case_when(
        rt == "290"                                ~ "Eisenhower Expressway",
        rt == "55"                                 ~ "Stevenson Expressway",
        rt == "190"                                ~ "Kennedy Expressway",
        rt == "94" & lat > 41.968                  ~ "Edens Expressway",
        rt %in% c("90", "94") & lat > 41.870       ~ "Kennedy Expressway",
        rt == "94" & lat <= 41.722                 ~ "Bishop Ford Freeway",
        rt == "90" & lat <= 41.775 & lon > -87.625 ~ "Chicago Skyway",
        rt %in% c("90", "94")                      ~ "Dan Ryan Expressway",
        !is.na(rt)                                 ~ paste0("I-", rt),
        str_detect(coalesce(FULLNAME, ""), "Expy|Expressway") ~
          str_replace(FULLNAME, "Expy$", "Expressway"),
        TRUE ~ NA_character_)
    ) %>%
    filter(!is.na(xname)) %>%
    select(xname, FULLNAME)
}

# Flag sites that are mostly expressway right-of-way: at least `min_share`
# of the site's area lies within `row_ft` of an expressway centerline.
# TIGER draws each direction of a divided expressway as its own line, so
# 75 ft from each covers the roadway and shoulders.
flag_expressway_sites <- function(sites, xw, row_ft = 75, min_share = 0.30) {
  stopifnot(inherits(sites, "sf"), "site_id" %in% names(sites))
  row <- xw %>% st_buffer(row_ft) %>% st_union() %>% st_make_valid()
  
  hit <- sites[lengths(st_intersects(sites, row)) > 0, "site_id"]
  if (!nrow(hit)) {
    return(sites %>% mutate(xway_row_share = 0, on_expressway = FALSE))
  }
  
  shares <- suppressWarnings(st_intersection(st_make_valid(hit), row)) %>%
    mutate(a_in = as.numeric(st_area(.))) %>%
    st_drop_geometry() %>%
    group_by(site_id) %>%
    summarise(a_in = sum(a_in), .groups = "drop") %>%
    left_join(hit %>% mutate(a = as.numeric(st_area(.))) %>% st_drop_geometry(),
              by = "site_id") %>%
    transmute(site_id, xway_row_share = pmin(1, a_in / a))
  
  sites %>%
    left_join(shares, by = "site_id") %>%
    mutate(xway_row_share = coalesce(xway_row_share, 0),
           on_expressway  = xway_row_share >= min_share)
}

# Drop flagged sites and write them to a CSV for review
drop_expressway_sites <- function(sites, xw, row_ft = 75, min_share = 0.30,
                                  log_file = "out/expressway_dropped.csv",
                                  keep_ids = character(0)) {
  flagged <- flag_expressway_sites(sites, xw, row_ft, min_share)
  drop    <- flagged %>% filter(on_expressway, !as.character(site_id) %in% keep_ids)
  
  drop %>%
    st_drop_geometry() %>%
    select(any_of(c("site_id", "station_name", "owner_group", "owner_main",
                    "acres", "acres_ws")), xway_row_share) %>%
    arrange(desc(xway_row_share)) %>%
    write_csv(log_file)
  
  message(nrow(drop), " sites dropped as expressway right-of-way. ",
          "Review them in ", log_file,
          "; put any to keep back with keep_ids.")
  
  flagged %>%
    filter(!site_id %in% drop$site_id) %>%
    select(-on_expressway, -xway_row_share)
}