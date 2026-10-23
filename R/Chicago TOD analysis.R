# =============================================================================
# Chicago TOD redevelopment analysis
# Part 1: Stations + walksheds, Part 2: Parcels with market values
# =============================================================================

library(tidyverse)
library(sf)

crs_il   <- 3435    # NAD83 / Illinois East (US ft)
walk_ft  <- 2640    # 1/2 mile
chi_twps <- as.character(70:77)   # Chicago's 8 assessor townships (City triad)

# ---- helpers ----------------------------------------------------------------

read_gtfs_stops <- function(url) {
  tmp <- tempfile(fileext = ".zip")
  download.file(url, tmp, mode = "wb", quiet = TRUE)
  read_csv(unz(tmp, "stops.txt"), col_types = cols(.default = "c"))
}

to_pin10 <- function(x) {
  x <- str_remove_all(as.character(x), "\\D")
  width <- if (max(nchar(x), na.rm = TRUE) > 10) 14 else 10
  str_sub(str_pad(x, width, pad = "0"), 1, 10)
}

sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# Approximate current levels of assessment by major class.
# EX (exempt) / RR (railroad) -> NA: no meaningful market value.
level_of_assessment <- function(class) {
  case_when(
    str_detect(class, "^[1236789]") ~ 0.10,
    str_detect(class, "^4")         ~ 0.20,
    str_detect(class, "^5")         ~ 0.25,
    TRUE                            ~ NA_real_
  )
}

# =============================================================================
# 1. STATIONS + WALKSHEDS
# =============================================================================

load_stations <- function() {
  cta <- read_gtfs_stops("https://www.transitchicago.com/downloads/sch_data/google_transit.zip") %>%
    filter(location_type == "1") %>%   # parent stations = L stations only
    transmute(stop_id, stop_name, mode = "CTA", stop_lat, stop_lon)
  
  metra <- read_gtfs_stops("https://schedules.metrarail.com/gtfs/schedule.zip") %>%
    transmute(stop_id, stop_name, mode = "Metra", stop_lat, stop_lon)
  
  bind_rows(cta, metra) %>%
    mutate(across(c(stop_lat, stop_lon), as.numeric)) %>%
    st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
    st_transform(crs_il)
}

# Collapse stops within tol_ft of each other into one station complex
# (e.g. Lake/State-Lake). Raise tol_ft to also merge Clinton/Ogilvie-type pairs.
merge_station_complexes <- function(stops, tol_ft = 300) {
  clusters <- stops %>%
    st_buffer(tol_ft / 2) %>%
    st_union() %>%
    st_cast("POLYGON") %>%
    st_sf(geometry = .) %>%
    mutate(station_id = row_number())
  
  stops %>%
    st_join(clusters, join = st_intersects) %>%
    group_by(station_id) %>%
    summarise(
      station_name = paste(unique(stop_name), collapse = " / "),
      modes        = paste(sort(unique(mode)), collapse = " + "),
      n_stops      = n(),
      .groups      = "drop"
    ) %>%
    st_centroid()
}

build_walksheds <- function(stations, dist_ft = walk_ft) {
  stations %>% st_buffer(dist_ft)
}

# =============================================================================
# 2. PARCELS
# =============================================================================

# Assessed values for Chicago townships, paged from the Cook County portal
load_assessed_values <- function(year = 2025, page = 500000) {
  where <- sprintf("year = %d AND township_code IN (%s)",
                   year, paste0("'", chi_twps, "'", collapse = ","))
  base <- paste0(
    "https://datacatalog.cookcountyil.gov/resource/uzyt-m557.csv?",
    "$select=pin,class,township_code,",
    "mailed_land,mailed_bldg,mailed_tot,",
    "certified_land,certified_bldg,certified_tot,",
    "board_land,board_bldg,board_tot",
    "&$where=", URLencode(where, reserved = TRUE),
    "&$order=pin&$limit=", format(page, scientific = FALSE)
  )
  
  out <- list(); offset <- 0
  repeat {
    chunk <- read_csv(paste0(base, "&$offset=", format(offset, scientific = FALSE)),
                      col_types = cols(pin = "c", class = "c", township_code = "c",
                                       .default = "d"))
    out <- c(out, list(chunk))
    if (nrow(chunk) < page) break
    offset <- offset + page
  }
  
  bind_rows(out) %>%
    mutate(
      pin    = str_pad(pin, 14, pad = "0"),
      land_av = coalesce(board_land, certified_land, mailed_land),
      bldg_av = coalesce(board_bldg, certified_bldg, mailed_bldg),
      tot_av  = coalesce(board_tot,  certified_tot,  mailed_tot)
    ) %>%
    select(pin, class, township_code, land_av, bldg_av, tot_av)
}

to_market_value <- function(av) {
  av %>%
    mutate(
      loa     = level_of_assessment(class),
      land_mv = land_av / loa,
      bldg_mv = bldg_av / loa,
      tot_mv  = tot_av  / loa
    )
}

# Condo units / divisions (14-digit PINs) -> one record per 10-digit land PIN
roll_up_pin10 <- function(mv) {
  mv %>%
    mutate(pin10 = str_sub(pin, 1, 10)) %>%
    group_by(pin10) %>%
    summarise(
      n_pins     = n(),
      class      = class[which.max(replace_na(tot_av, -1))],  # class of largest PIN
      any_exempt = any(class %in% c("EX", "RR")),
      all_exempt = all(class %in% c("EX", "RR")),
      across(c(land_mv, bldg_mv, tot_mv), sum_or_na),
      .groups    = "drop"
    )
}

# County parcel polygons (download the latest "ccgisdata - Parcel" layer
# from datacatalog.cookcountyil.gov). Clipped to walksheds by point-on-surface.
load_parcel_geometry <- function(path, walksheds, pin_col = "pin10") {
  ws <- walksheds %>% st_union()
  
  geom <- st_read(path, quiet = TRUE) %>%
    st_transform(crs_il) %>%
    st_make_valid() %>%
    transmute(pin10 = to_pin10(.data[[pin_col]]))
  
  geom <- geom[st_intersects(st_point_on_surface(geom), ws, sparse = FALSE)[, 1], ]
  
  # Union only the pin10s that appear more than once (condo / multipart)
  dup <- geom %>% filter(duplicated(pin10) | duplicated(pin10, fromLast = TRUE))
  
  bind_rows(
    geom %>% filter(!pin10 %in% dup$pin10),
    dup  %>% group_by(pin10) %>% summarise(.groups = "drop")
  )
}

build_tod_parcels <- function(parcel_path, walksheds, year = 2025,
                              min_sqft = 2000, pin_col = "pin10") {
  geom <- load_parcel_geometry(parcel_path, walksheds, pin_col)
  vals <- load_assessed_values(year) %>% to_market_value() %>% roll_up_pin10()
  
  n_unmatched <- sum(!geom$pin10 %in% vals$pin10)
  message(n_unmatched, " parcel polygons in walksheds have no Chicago assessor record (ROW, suburbs, etc.)")
  
  geom %>%
    inner_join(vals, by = "pin10") %>%
    mutate(
      sqft        = as.numeric(st_area(geometry)),
      acres       = sqft / 43560,
      mv_per_acre = tot_mv / acres,
      ilr         = bldg_mv / land_mv     # improvement-to-land ratio
    ) %>%
    filter(sqft >= min_sqft)
}

# =============================================================================
# RUN
# =============================================================================

stops     <- load_stations()
stations  <- stops %>% merge_station_complexes()
stations

walksheds <- stations %>% build_walksheds()
walksheds

parcels   <- build_tod_parcels("C:\\Users\\mdunst\\Downloads\\ccgisdata_-_Parcel_2021_20261019.geojson", walksheds, year = 2025)
parcels

# Quick checks
parcels %>% st_drop_geometry() %>% count(all_exempt)
parcels %>% st_drop_geometry() %>% summary()




nominal_floor <- 1000      # total market value below this = nominal / effectively exempt
drop_classes  <- c("241")  # side lots under common ownership with adjacent residence
rank_exclude  <- c("299")  # residential condos: in baselines, but fragmented ownership

sum_or_na <- function(x) if (all(is.na(x))) NA_real_ else sum(x, na.rm = TRUE)

# =============================================================================
# 2b. PARCEL CLEANUP
# =============================================================================

# Commercial condo / air-rights divisions: several pin10s share one lot, either
# as identical copies or as near-identical / nested polygons (vertical
# subdivisions like Illinois Center). Group polygons whose interiors overlap
# by at least min_overlap of the smaller one, and sum their values.
cluster_ids <- function(n, i, j) {
  lab <- seq_len(n)
  repeat {
    m   <- pmin(lab[i], lab[j])
    upd <- tapply(c(m, m), c(i, j), min)
    idx <- as.integer(names(upd))
    new <- lab
    new[idx] <- pmin(lab[idx], upd)
    if (identical(new, lab)) break
    lab <- new
  }
  lab
}

group_stacked_sites <- function(parcels, min_overlap = 0.9) {
  geom <- st_geometry(parcels)
  area <- as.numeric(st_area(geom))
  
  # Candidate pairs: one polygon's interior point falls inside another polygon.
  # Fast point-in-polygon test; edge-sharing neighbors never qualify, while
  # identical and nested (air-rights) polygons always do.
  pts   <- st_point_on_surface(geom)
  hits  <- st_intersects(pts, geom)
  pairs <- tibble(i = rep(seq_along(hits), lengths(hits)), j = unlist(hits)) %>%
    filter(i != j) %>%
    mutate(a = pmin(i, j), b = pmax(i, j)) %>%
    distinct(a, b) %>%
    rename(i = a, j = b) %>%
    mutate(
      inter = map2_dbl(i, j, ~ sum(as.numeric(st_area(st_intersection(geom[.x], geom[.y]))))),
      share = inter / pmin(area[i], area[j])
    ) %>%
    filter(share >= min_overlap)
  
  message(nrow(pairs), " overlapping polygon pairs grouped")
  
  keyed <- parcels %>%
    mutate(
      geom_key = cluster_ids(n(), pairs$i, pairs$j),
      area_tmp = area
    )
  
  attrs <- keyed %>%
    st_drop_geometry() %>%
    group_by(geom_key) %>%
    summarise(
      site_id    = min(pin10),
      pin10s     = paste(sort(pin10), collapse = ";"),
      n_pin10    = n(),
      n_pins     = sum(n_pins),
      class      = class[which.max(replace_na(tot_mv, -1))],
      any_exempt = any(any_exempt),
      all_exempt = all(all_exempt),
      across(c(land_mv, bldg_mv, tot_mv), sum_or_na),
      .groups    = "drop"
    )
  
  # Keep the largest footprint in each group as the site geometry
  keyed %>%
    arrange(desc(area_tmp)) %>%
    distinct(geom_key, .keep_all = TRUE) %>%
    select(geom_key) %>%
    inner_join(attrs, by = "geom_key") %>%
    select(-geom_key)
}

# Which sites extend past the walkshed edge? Fully vectorized: a site is
# entirely inside if every vertex is within dist_ft of its nearest station.
find_edge_sites <- function(sites, stations, dist_ft = walk_ft) {
  near <- st_nearest_feature(st_point_on_surface(st_geometry(sites)), stations)
  sxy  <- st_coordinates(stations)[near, , drop = FALSE]
  
  # Cast to one type so the L columns are consistent (L3 = feature index)
  xy  <- st_coordinates(st_cast(st_geometry(sites), "MULTIPOLYGON"))
  fid <- xy[, "L3"]
  d   <- sqrt((xy[, "X"] - sxy[fid, "X"])^2 + (xy[, "Y"] - sxy[fid, "Y"])^2)
  
  as.vector(tapply(d, fid, max)) > dist_ft
}

# Walkshed area for edge sites only, against individual (indexed) walksheds
clip_edge_sites <- function(edge, walksheds) {
  edge %>%
    select(site_id) %>%
    st_intersection(walksheds %>% select(geometry)) %>%
    group_by(site_id) %>%
    summarise(.groups = "drop") %>%             # unions overlapping circles
    mutate(sqft_ws = as.numeric(st_area(geometry))) %>%
    st_drop_geometry()
}

# Group stacked lots, clip edge sites, flag nominal values, recompute metrics
prep_parcels <- function(parcels, walksheds) {
  stations_pts <- st_centroid(st_geometry(walksheds))   # circle centers = stations
  
  sites <- parcels %>%
    group_stacked_sites() %>%
    filter(!class %in% drop_classes)
  
  on_edge <- find_edge_sites(sites, stations_pts)
  message(sum(on_edge), " of ", nrow(sites), " sites cross a walkshed edge; clipping those only")
  
  clipped <- clip_edge_sites(sites[on_edge, ], walksheds)
  
  sites %>%
    left_join(clipped, by = "site_id") %>%
    mutate(
      sqft        = as.numeric(st_area(geometry)),
      acres       = sqft / 43560,
      acres_ws    = coalesce(sqft_ws, sqft) / 43560,
      share_in_ws = acres_ws / acres,
      tot_mv_raw  = tot_mv,
      nominal     = !all_exempt & (is.na(tot_mv) | tot_mv < nominal_floor),
      no_value    = all_exempt | nominal,
      across(c(land_mv, bldg_mv, tot_mv), ~ if_else(no_value, NA_real_, .x)),
      mv_per_acre = tot_mv / acres,
      ilr         = if_else(land_mv > 0, bldg_mv / land_mv, NA_real_)
    ) %>%
    select(-sqft_ws)
}

# =============================================================================
# 3. ASSIGN EACH SITE TO ITS NEAREST STATION
# =============================================================================

assign_nearest_station <- function(sites, stations) {
  pts <- st_point_on_surface(st_geometry(sites))
  idx <- st_nearest_feature(pts, stations)
  
  sites %>%
    mutate(
      station_id   = stations$station_id[idx],
      station_name = stations$station_name[idx],
      modes        = stations$modes[idx],
      dist_ft      = as.numeric(st_distance(pts, stations[idx, ], by_element = TRUE))
    )
}

# =============================================================================
# 4. VALUE-GAP SCORING
# =============================================================================

# Baselines from valued sites only (no exempt or nominal parcels)
station_baselines <- function(tod, min_n = 25) {
  tod %>%
    st_drop_geometry() %>%
    filter(!no_value, mv_per_acre > 0) %>%
    group_by(station_id, station_name, modes) %>%
    summarise(
      n_taxable       = n(),
      acres_taxable   = sum(acres_ws),
      med_mv_acre     = median(mv_per_acre),
      med_log_mv_acre = median(log(mv_per_acre)),
      mad_log_mv_acre = mad(log(mv_per_acre)),
      med_ilr         = median(ilr, na.rm = TRUE),
      .groups         = "drop"
    ) %>%
    mutate(thin_sample = n_taxable < min_n)
}

score_value_gap <- function(tod, baselines) {
  tod %>%
    left_join(baselines %>% select(-station_name, -modes), by = "station_id") %>%
    group_by(station_id) %>%
    mutate(
      z_mv_acre    = (log(mv_per_acre) - med_log_mv_acre) / mad_log_mv_acre,
      ratio_to_med = mv_per_acre / med_mv_acre,
      
      # Gap on the walkshed share of the site only. No-value sites get the
      # full implied value (used for the public-land ranking in step 5).
      implied_mv   = med_mv_acre * acres_ws,
      gap_dollars  = implied_mv - coalesce(mv_per_acre, 0) * acres_ws,
      
      # Scale-free size of the gap: walkshed acres x shortfall vs station median.
      # Unlike gap_dollars, this doesn't favor high-value downtown stations.
      gap_acres    = acres_ws * pmax(0, 1 - coalesce(ratio_to_med, 0)),
      
      ilr_pctile   = percent_rank(ilr)
    ) %>%
    ungroup() %>%
    mutate(
      underbuilt = z_mv_acre <= -1.5,
      soft_site  = ilr_pctile <= 0.10,
      both_flags = underbuilt & soft_site
    )
}

rank_candidates <- function(scored, n = 100, rank_by = gap_dollars) {
  scored %>%
    st_drop_geometry() %>%
    filter(!no_value, !is.na(z_mv_acre), gap_dollars > 0,
           !class %in% rank_exclude) %>%
    arrange(desc({{ rank_by }})) %>%
    select(site_id, n_pin10, pin10s, station_name, modes, dist_ft, class,
           acres, acres_ws, share_in_ws, tot_mv, mv_per_acre, med_mv_acre,
           ratio_to_med, z_mv_acre, ilr, ilr_pctile, gap_dollars, gap_acres,
           underbuilt, soft_site, both_flags, thin_sample) %>%
    slice_head(n = n)
}

summarise_stations <- function(scored, baselines) {
  scored %>%
    st_drop_geometry() %>%
    group_by(station_id) %>%
    summarise(
      n_sites           = n(),
      n_underbuilt      = sum(underbuilt, na.rm = TRUE),
      n_both_flags      = sum(both_flags, na.rm = TRUE),
      acres_underbuilt  = sum(acres_ws[underbuilt %in% TRUE]),
      gap_underbuilt    = sum(gap_dollars[underbuilt %in% TRUE]),
      gap_acres_ub      = sum(gap_acres[underbuilt %in% TRUE]),
      n_no_value        = sum(no_value),
      n_nominal         = sum(nominal),
      acres_no_value    = sum(acres_ws[no_value]),
      .groups           = "drop"
    ) %>%
    left_join(baselines, by = "station_id") %>%
    arrange(desc(gap_underbuilt))
}

# =============================================================================
# RUN
# =============================================================================

# prep_parcels is the slow step: cache it and reload on reruns
if (file.exists("out/sites.rds")) {
  sites <- readRDS("out/sites.rds")
} else {
  sites <- parcels %>% prep_parcels(walksheds)
  dir.create("out", showWarnings = FALSE)
  saveRDS(sites, "out/sites.rds")
}
sites

tod       <- sites %>% assign_nearest_station(stations)
baselines <- tod %>% station_baselines()
baselines

scored    <- tod %>% score_value_gap(baselines)
scored

top_overall     <- scored %>% rank_candidates(n = 100)
top_overall

top_by_acres    <- scored %>% rank_candidates(n = 100, rank_by = gap_acres)
top_by_acres

station_summary <- scored %>% summarise_stations(baselines)
station_summary

# ---- Checks -----------------------------------------------------------------

# How much did cleanup change things?
tibble(
  pin10s_in     = nrow(parcels),
  sites_out     = nrow(sites),
  stacked_sites = sum(sites$n_pin10 > 1),
  nominal_sites = sum(sites$nominal),
  clipped_sites = sum(sites$share_in_ws < 0.999)
)

# Downtown baselines should now have much tighter spreads
baselines %>% arrange(desc(mad_log_mv_acre)) %>% slice_head(n = 10)

# Underbuilt by class
scored %>% st_drop_geometry() %>% filter(underbuilt) %>% count(class, sort = TRUE)

# ---- Export for review ------------------------------------------------------

dir.create("out", showWarnings = FALSE)
write_csv(station_summary, "out/station_summary.csv")
write_csv(top_overall, "out/top_overall.csv")
write_csv(top_by_acres, "out/top_by_acres.csv")
scored %>% st_drop_geometry() %>% filter(underbuilt) %>%
  count(class, sort = TRUE) %>% write_csv("out/underbuilt_by_class.csv")





transit_groups <- c("CTA", "Metra", "Pace", "NITA/RTA")
public_groups  <- c(transit_groups, "City of Chicago", "Cook County Land Bank",
                    "CHA", "CPS / PBC", "Chicago Park District", "City Colleges",
                    "MWRD", "Cook County", "Forest Preserve", "State of Illinois",
                    "Federal")
jd_exclude     <- c("Chicago Park District", "Forest Preserve")  # not realistic JD land
linear_ft      <- 150   # max "thickness" for a parcel to count as right-of-way strip

# ---- helpers ----------------------------------------------------------------

soda_pages <- function(base, page = 500000) {
  out <- list(); offset <- 0
  repeat {
    chunk <- read_csv(paste0(base, "&$limit=", format(page, scientific = FALSE),
                             "&$offset=", format(offset, scientific = FALSE)),
                      col_types = cols(.default = "c"))
    out <- c(out, list(chunk))
    if (nrow(chunk) < page) break
    offset <- offset + page
  }
  bind_rows(out)
}

# =============================================================================
# 5a. OWNER NAMES (Assessor - Parcel Addresses), cached
# =============================================================================

load_owner_names <- function(year = 2026, cache = "out/owners.rds") {
  if (file.exists(cache)) return(readRDS(cache))
  
  root <- "https://datacatalog.cookcountyil.gov/resource/3723-97qp.csv?"
  all_cols  <- names(read_csv(paste0(root, "$limit=1"), col_types = cols(.default = "c")))
  name_cols <- all_cols %>%
    str_subset("name") %>%
    str_subset("township|nbhd|city|street|unit", negate = TRUE)
  message("Owner/taxpayer name columns: ", paste(name_cols, collapse = ", "))
  
  base <- paste0(root,
                 "$select=", paste(c("pin", name_cols), collapse = ","),
                 "&$where=", URLencode(sprintf("year = %d", year), reserved = TRUE),
                 "&$order=pin")
  
  owners <- soda_pages(base)
  if (nrow(owners) == 0) stop("No rows for ", year, "; try year = ", year - 1)
  
  owners <- owners %>% mutate(pin = str_pad(pin, 14, pad = "0"))
  dir.create(dirname(cache), showWarnings = FALSE)
  saveRDS(owners, cache)
  owners
}

# Roll pin14 names up to sites (a site can hold several pin10s)
roll_names <- function(owners, cols, site_pins) {
  if (length(cols) == 0) {
    return(tibble(site_id = character(), main = character(), text = character()))
  }
  owners %>%
    mutate(pin10 = str_sub(pin, 1, 10)) %>%
    semi_join(site_pins, by = "pin10") %>%
    select(pin10, all_of(cols)) %>%
    pivot_longer(all_of(cols), names_to = "src", values_to = "name") %>%
    filter(!is.na(name), str_trim(name) != "") %>%
    mutate(name = str_squish(str_to_upper(name))) %>%
    inner_join(site_pins, by = "pin10") %>%
    count(site_id, name, sort = TRUE) %>%
    group_by(site_id) %>%
    summarise(
      main    = first(name),                                   # most frequent name
      text    = str_trunc(paste(name, collapse = " | "), 400),
      .groups = "drop"
    )
}

# Owner of record and taxpayer kept separate: public owner + private taxpayer
# usually means transit/public land leased to a business
attach_owners <- function(scored, owners) {
  name_cols  <- setdiff(names(owners), "pin")
  owner_cols <- str_subset(name_cols, "owner")
  if (length(owner_cols) == 0) owner_cols <- name_cols
  tax_cols   <- setdiff(name_cols, owner_cols)
  message("Owner-of-record columns: ", paste(owner_cols, collapse = ", "),
          " | taxpayer/mail columns: ", paste(tax_cols, collapse = ", "))
  
  site_pins <- scored %>%
    st_drop_geometry() %>%
    select(site_id, pin10s) %>%
    separate_rows(pin10s, sep = ";") %>%
    rename(pin10 = pin10s)
  
  own <- roll_names(owners, owner_cols, site_pins) %>%
    rename(owner_main = main, owner_text = text)
  tax <- roll_names(owners, tax_cols, site_pins) %>%
    rename(taxpayer_main = main, taxpayer_text = text)
  
  scored %>%
    left_join(own, by = "site_id") %>%
    left_join(tax, by = "site_id")
}

# =============================================================================
# 5b. CITY-OWNED LAND INVENTORY (Chicago Data Portal), cached
# =============================================================================

load_city_inventory <- function(cache = "out/city_land.rds") {
  if (file.exists(cache)) return(readRDS(cache))
  inv <- soda_pages("https://data.cityofchicago.org/resource/aksk-kvfp.csv?$order=:id")
  dir.create(dirname(cache), showWarnings = FALSE)
  saveRDS(inv, cache)
  inv
}

# The inventory includes historically owned land: keep current holdings only.
# Prints the status values so you can confirm the filter is right.
city_owned_pin10s <- function(inv) {
  pin_col    <- str_subset(names(inv), "pin")[1]
  status_col <- str_subset(names(inv), "status")[1]
  message("City inventory: PIN column '", pin_col, "', status column '", status_col, "'")
  
  if (!is.na(status_col)) {
    print(count(inv, .data[[status_col]], sort = TRUE))
    inv <- inv %>%
      filter(str_detect(str_to_lower(.data[[status_col]]), "own"),
             !str_detect(str_to_lower(.data[[status_col]]), "sold|former"))
  }
  
  inv %>% pull(all_of(pin_col)) %>% to_pin10() %>% unique()
}

# =============================================================================
# 5c. CLASSIFY OWNERS
# =============================================================================

# Order matters: specific agencies before the generic "City of Chicago" match.
# Patterns include the Assessor's truncations and misspellings seen in QA.
classify_owner <- function(x) {
  x <- str_to_upper(coalesce(x, ""))
  case_when(
    str_detect(x, "^(STREET|EXPRESSWAY|ALLEY)\\b")                              ~ "Right-of-way",
    str_detect(x, "CHICAGO TRANSIT AUTH|\\bCTA\\b")                              ~ "CTA",
    str_detect(x, "METRA|COMMUTER RAIL|NORTHEAST(ERN)? ILL(INOIS)? REG|\\bNIRC") ~ "Metra",
    str_detect(x, "PACE SUBURBAN|SUBURBAN BUS")                                  ~ "Pace",
    str_detect(x, "REGIONAL TRANSP(ORTATION)? AUTH|NORTHERN ILLINOIS TRANSIT|\\bRTA\\b|\\bNITA\\b") ~ "NITA/RTA",
    str_detect(x, "CHICAGO H(OU)?SING AUTH")                                     ~ "CHA",
    str_detect(x, "BOARD OF ED|BD OF ED|BRD OF ED|CHICAGO PUBLIC SCHOOL|PUB(LIC)? B(UI)?LD(IN)?G COMM") ~ "CPS / PBC",
    str_detect(x, "PARK DIST")                                                   ~ "Chicago Park District",
    str_detect(x, "LAND BANK")                                                   ~ "Cook County Land Bank",
    str_detect(x, "CITY COLLEGES|COMMUNITY COLLEGE DIST|CITY ?JR COLLEGE")      ~ "City Colleges",
    str_detect(x, "WATER RECL|\\bMWRD\\b|SANITARY DIST")                         ~ "MWRD",
    str_detect(x, "FOREST PRE")                                                  ~ "Forest Preserve",
    str_detect(x, "COUNTY OF COOK|COOK COUNTY")                                  ~ "Cook County",
    str_detect(x, "CITY OF CHI|CITY OF CHGO|CHICAGO CITY OF|CHICAGO DEPT\\.? ?OF") ~ "City of Chicago",
    str_detect(x, paste0("STATE OF IL|ILLINOIS DEPT|IL DEPT|\\bIDOT\\b|IL(LINOIS)? MEDICAL DIST|",
                         "UNIV(ERSITY)? OF ILL|CHICAGO STATE UNIV|NORTHEASTERN IL|",
                         "IL SPORTS FAC|TOLL H(WY|IGHWAY)|IL HISTORIC PRES|METRO PIER")) ~ "State of Illinois",
    str_detect(x, "UNITED STATES|U ?S GENERAL SERV|U S A\\b|GENERAL SERVICES ADMIN|POSTAL SERVICE|\\bHUD\\b") ~ "Federal",
    str_detect(x, "RAILROAD|RAILWAY|\\bRR\\b|R R CO|UNION PACIFIC|BNSF|BURLINGTON NORTHERN|NORFOLK SOUTHERN|\\bCSX\\b|CANADIAN NATIONAL|ILLINOIS CENTRAL|CANADIAN PACIFIC|SOO LINE") ~ "Railroad",
    TRUE ~ NA_character_
  )
}

flag_public <- function(owned, city_pins) {
  city_sites <- owned %>%
    st_drop_geometry() %>%
    select(site_id, pin10s) %>%
    separate_rows(pin10s, sep = ";") %>%
    filter(pin10s %in% city_pins) %>%
    distinct(site_id) %>%
    pull(site_id)
  
  owned %>%
    mutate(
      og               = classify_owner(owner_text),
      tg               = classify_owner(taxpayer_text),
      owner_group      = coalesce(og, tg),
      city_inv         = site_id %in% city_sites,
      owner_group      = if_else(is.na(owner_group) & city_inv, "City of Chicago", owner_group),
      is_transit       = owner_group %in% transit_groups,
      is_public        = owner_group %in% public_groups,
      
      # Public owner on one record, private name on the other: likely a lease
      private_party    = case_when(
        is_public & !is.na(owner_main)    & is.na(og) ~ owner_main,
        is_public & !is.na(taxpayer_main) & is.na(tg) &
          !str_detect(taxpayer_main, "\\bLLP\\b|TAX DEPT|TAX SERV|TAXPAYER OF|^ATTN|PROPERTY TAX") ~ taxpayer_main
      ),
      leased_out       = !is.na(private_party),
      
      # Long, thin parcels = rail or street right-of-way, not development sites
      perim_ft         = as.numeric(st_length(st_boundary(geometry))),
      thickness_ft     = 2 * sqft / perim_ft,
      compactness      = 4 * pi * sqft / perim_ft^2,
      linear           = thickness_ft < linear_ft & compactness < 0.3,
      
      exempt_unmatched = no_value & !is_public & !owner_group %in% c("Railroad", "Right-of-way")
    ) %>%
    select(-og, -tg)
}

# =============================================================================
# 5d. SUMMARIES AND RANKINGS
# =============================================================================

summarise_public_owners <- function(pub) {
  pub %>%
    st_drop_geometry() %>%
    filter(is_public | owner_group %in% "Railroad") %>%
    group_by(owner_group, is_transit) %>%
    summarise(
      n_sites    = n(),
      n_stations = n_distinct(station_id),
      acres_ws   = sum(acres_ws),
      implied_mv = sum(implied_mv, na.rm = TRUE),
      .groups    = "drop"
    ) %>%
    arrange(desc(acres_ws))
}

# Public sites have no (or nominal) value, so rank on land and implied value
rank_public <- function(pub, n = 100, rank_by = acres_ws, groups = public_groups,
                        exclude = jd_exclude) {
  pub %>%
    st_drop_geometry() %>%
    filter(owner_group %in% groups, !owner_group %in% exclude, !linear) %>%
    arrange(desc({{ rank_by }})) %>%
    select(site_id, n_pin10, owner_group, owner_main, taxpayer_main, leased_out,
           private_party, station_name, modes, dist_ft, class, no_value, acres,
           acres_ws, thickness_ft, implied_mv, tot_mv_raw, med_mv_acre, gap_acres) %>%
    slice_head(n = n)
}

public_by_station <- function(pub) {
  pub %>%
    st_drop_geometry() %>%
    group_by(station_id, station_name, modes) %>%
    summarise(
      acres_public  = sum(acres_ws[is_public]),
      acres_transit = sum(acres_ws[is_transit]),
      acres_transit_sites = sum(acres_ws[is_transit & !linear]),   # net of ROW strips
      acres_city_lb = sum(acres_ws[owner_group %in% c("City of Chicago", "Cook County Land Bank")]),
      implied_mv_public = sum(implied_mv[is_public], na.rm = TRUE),
      n_public      = sum(is_public),
      .groups       = "drop"
    ) %>%
    arrange(desc(acres_public))
}

# Public land with a private taxpayer: existing leases / concessions
public_leases <- function(pub) {
  pub %>%
    st_drop_geometry() %>%
    filter(leased_out) %>%
    arrange(owner_group, desc(acres_ws)) %>%
    select(site_id, owner_group, private_party, station_name, modes, class,
           acres_ws, tot_mv_raw, linear)
}

# QA: biggest exempt/nominal owners we did NOT classify as public
unmatched_owners <- function(pub, n = 75) {
  pub %>%
    st_drop_geometry() %>%
    filter(exempt_unmatched) %>%
    group_by(owner_main, taxpayer_main) %>%
    summarise(n_sites = n(), acres_ws = sum(acres_ws), .groups = "drop") %>%
    arrange(desc(acres_ws)) %>%
    slice_head(n = n)
}

# =============================================================================
# RUN
# =============================================================================

owners    <- load_owner_names(year = 2026)
owners

city_pins <- load_city_inventory() %>% city_owned_pin10s()
length(city_pins)

pub <- scored %>%
  attach_owners(owners) %>%
  flag_public(city_pins)
saveRDS(pub, "out/pub.rds")

# Coverage check: how many sites got a name, and how they classified
pub %>% st_drop_geometry() %>% summarise(pct_named = mean(!is.na(owner_main)))
pub %>% st_drop_geometry() %>% count(owner_group, sort = TRUE)

public_owners   <- pub %>% summarise_public_owners()
public_owners

top_public      <- pub %>% rank_public(n = 100)
top_public

top_public_mv   <- pub %>% rank_public(n = 100, rank_by = implied_mv)
top_public_mv

top_transit     <- pub %>% rank_public(n = 100, groups = transit_groups)
top_transit

station_public  <- pub %>% public_by_station()
station_public

unmatched       <- pub %>% unmatched_owners()
unmatched

leases          <- pub %>% public_leases()
leases
leases %>% count(owner_group, sort = TRUE)

# How much transit land is right-of-way vs. site-shaped?
pub %>% st_drop_geometry() %>% filter(is_transit) %>%
  group_by(owner_group, linear) %>%
  summarise(n = n(), acres_ws = sum(acres_ws), .groups = "drop")

exclude_sites <- c("1208100006") 

# ---- Export for review ------------------------------------------------------

write_csv(public_owners,  "out/public_owners.csv")
write_csv(top_public,     "out/top_public.csv")
write_csv(top_public_mv,  "out/top_public_mv.csv")
write_csv(top_transit,    "out/top_transit.csv")
write_csv(station_public, "out/station_public.csv")
write_csv(unmatched,      "out/unmatched_owners.csv")
write_csv(leases,         "out/public_leases.csv")








if (!exists("exclude_sites")) exclude_sites <- c("1208100006")   # O'Hare airfield; add others

housing_cats <- c("Business / commercial", "Downtown", "Multifamily / townhouse",
                  "Single-family")

# =============================================================================
# LOAD AND CLASSIFY ZONING
# =============================================================================

load_zoning <- function(cache = "out/zoning.rds") {
  if (file.exists(cache)) return(readRDS(cache))
  
  z <- st_read("https://data.cityofchicago.org/resource/dj47-wfun.geojson?$limit=50000",
               quiet = TRUE) %>%
    st_transform(crs_il) %>%
    st_make_valid() %>%
    select(zone_class)
  
  saveRDS(z, cache)
  z
}

zone_category <- function(zc) {
  zc <- str_to_upper(str_squish(zc))
  case_when(
    is.na(zc)                  ~ "Unzoned / ROW",
    str_detect(zc, "^PMD")     ~ "PMD (protected industrial)",
    str_detect(zc, "^M[0-9]")  ~ "Manufacturing",
    str_detect(zc, "^PD")      ~ "Planned development",
    str_detect(zc, "^D[CXRS]") ~ "Downtown",
    str_detect(zc, "^[BC][0-9]") ~ "Business / commercial",
    str_detect(zc, "^R[MT]")   ~ "Multifamily / townhouse",
    str_detect(zc, "^RS")      ~ "Single-family",
    str_detect(zc, "^POS")     ~ "Parks / open space",
    str_detect(zc, "^T$")      ~ "Transportation",
    TRUE                       ~ "Other"
  )
}

# Base FAR by district. Approximate: ignores transit-served-location bonuses,
# so it understates what's achievable near stations. M districts left NA
# because housing isn't allowed there anyway.
base_far <- function(zc) {
  zc     <- str_to_upper(str_squish(zc))
  suffix <- suppressWarnings(as.numeric(str_extract(zc, "(?<=-)[0-9.]+$")))
  bc_far <- case_when(suffix == 1 ~ 1.2, suffix == 1.5 ~ 1.5, suffix == 2 ~ 2.2,
                      suffix == 3 ~ 3.0, suffix == 5 ~ 5.0)
  case_when(
    str_detect(zc, "^D[CXRS]-")  ~ suffix,
    str_detect(zc, "^[BC][0-9]-") ~ bc_far,
    zc == "RS-1"   ~ 0.5,  zc == "RS-2"   ~ 0.65, zc == "RS-3" ~ 0.9,
    zc == "RT-3.5" ~ 1.05, zc == "RT-4"   ~ 1.2,
    zc == "RM-4.5" ~ 1.7,  zc == "RM-5"   ~ 2.0,  zc == "RM-5.5" ~ 2.5,
    zc == "RM-6"   ~ 4.4,  zc == "RM-6.5" ~ 6.6,
    TRUE ~ NA_real_
  )
}

# Point-on-surface join: fast, but large split-zoned sites get one district
attach_zoning <- function(sites, zoning) {
  pts       <- st_point_on_surface(st_geometry(sites))
  hits      <- st_intersects(pts, zoning)
  first_hit <- map_int(hits, ~ if (length(.x)) .x[1] else NA_integer_)
  
  sites %>%
    mutate(
      zone_class    = zoning$zone_class[first_hit],
      zone_cat      = zone_category(zone_class),
      base_far      = base_far(zone_class),
      # Housing allowed as of right (C3 excluded: no residential)
      housing_ok    = zone_cat %in% housing_cats & !str_detect(coalesce(zone_class, ""), "^C3"),
      industrial    = zone_cat %in% c("PMD (protected industrial)", "Manufacturing"),
      base_cap_sqft = base_far * acres_ws * 43560
    )
}

# =============================================================================
# SUMMARIES AND RANKINGS
# =============================================================================

# Where does the underbuilt land sit, zoning-wise?
underbuilt_by_zoning <- function(zoned) {
  zoned %>%
    st_drop_geometry() %>%
    filter(underbuilt %in% TRUE, !no_value, !site_id %in% exclude_sites) %>%
    group_by(zone_cat) %>%
    summarise(
      n_sites   = n(),
      acres_ws  = sum(acres_ws),
      gap_acres = sum(gap_acres),
      .groups   = "drop"
    ) %>%
    mutate(share_gap_acres = gap_acres / sum(gap_acres)) %>%
    arrange(desc(gap_acres))
}

# Taxable candidates by gap_acres, with zoning; `exclude_zones` drops categories
rank_zoned <- function(zoned, n = 100, rank_by = gap_acres, exclude_zones = character()) {
  zoned %>%
    st_drop_geometry() %>%
    filter(!no_value, !is.na(z_mv_acre), gap_dollars > 0,
           !class %in% rank_exclude, !site_id %in% exclude_sites,
           !zone_cat %in% exclude_zones) %>%
    arrange(desc({{ rank_by }})) %>%
    select(site_id, n_pin10, station_name, modes, class, zone_class, zone_cat,
           housing_ok, base_far, acres_ws, tot_mv, ratio_to_med, gap_acres,
           gap_dollars, base_cap_sqft, owner_main, both_flags) %>%
    slice_head(n = n)
}

# How much of each station's underbuilt land is industrial-zoned?
station_zoning <- function(zoned) {
  zoned %>%
    st_drop_geometry() %>%
    filter(underbuilt %in% TRUE, !no_value, !site_id %in% exclude_sites) %>%
    group_by(station_id, station_name, modes) %>%
    summarise(
      # Subsets first: summarise() would otherwise overwrite gap_acres with the total
      gap_acres_housing  = sum(gap_acres[housing_ok], na.rm = TRUE),
      gap_acres_pmd      = sum(gap_acres[zone_cat == "PMD (protected industrial)"], na.rm = TRUE),
      gap_acres_mfg      = sum(gap_acres[zone_cat == "Manufacturing"], na.rm = TRUE),
      gap_acres_pd       = sum(gap_acres[zone_cat == "Planned development"], na.rm = TRUE),
      gap_acres          = sum(gap_acres, na.rm = TRUE),
      .groups            = "drop"
    ) %>%
    mutate(share_industrial = (gap_acres_pmd + gap_acres_mfg) / gap_acres) %>%
    arrange(desc(gap_acres_housing))
}

# Developable public land (no parks/forest preserve, no ROW strips) by zoning
public_by_zoning <- function(zoned) {
  zoned %>%
    st_drop_geometry() %>%
    filter(is_public, !linear, !owner_group %in% jd_exclude,
           !site_id %in% exclude_sites) %>%
    group_by(transit = is_transit, zone_cat) %>%
    summarise(n_sites = n(), acres_ws = sum(acres_ws), .groups = "drop") %>%
    arrange(transit, desc(acres_ws))
}

# =============================================================================
# RUN
# =============================================================================

if (!exists("pub")) pub <- readRDS("out/pub.rds")

zoning <- load_zoning()
zoning

zoned <- pub %>% attach_zoning(zoning)
saveRDS(zoned, "out/zoned.rds")

# Check the classifier: anything landing in "Other" needs a pattern
zoned %>% st_drop_geometry() %>% filter(zone_cat == "Other") %>%
  count(zone_class, sort = TRUE)

ub_zoning         <- zoned %>% underbuilt_by_zoning()
ub_zoning

top_zoned         <- zoned %>% rank_zoned(n = 100)
top_zoned

# Realistic housing/mixed-use list: drop protected and general industrial land
top_housing       <- zoned %>% rank_zoned(n = 100, exclude_zones = c("PMD (protected industrial)",
                                                                     "Manufacturing",
                                                                     "Parks / open space",
                                                                     "Transportation",
                                                                     "Unzoned / ROW"))
top_housing

st_zoning         <- zoned %>% station_zoning()
st_zoning

pub_zoning        <- zoned %>% public_by_zoning()
pub_zoning

# ---- Export for review ------------------------------------------------------

write_csv(ub_zoning,   "out/underbuilt_by_zoning.csv")
write_csv(top_zoned,   "out/top_zoned.csv")
write_csv(top_housing, "out/top_housing.csv")
write_csv(st_zoning,   "out/station_zoning.csv")
write_csv(pub_zoning,  "out/public_by_zoning.csv")










# ---- Policy assumptions (edit these: they drive the density results) -------

# Target FAR by station typology: what a station area *should* support,
# independent of current zoning
target_far_tbl <- tribble(
  ~typology,            ~target_far,
  "Downtown",           8,
  "CTA neighborhood",   3,
  "Metra neighborhood", 2
)
downtown_ft   <- 7920    # stations within 1.5 mi of State/Madison = Downtown
inner_ft      <- 1320    # within 1/4 mile of the station...
inner_bonus   <- 1.25    # ...target FAR is 25% higher
sqft_per_unit <- 1000    # gross building sq ft per housing unit

min_acres_private <- 0.25   # drops individual house lots from the private pipeline
not_sites   <- c("Parks / open space", "Transportation", "Unzoned / ROW")
# Public owners whose land a JD program could realistically offer directly;
# everyone else (CPS, CHA, State, Federal, MWRD...) is a partnership tier
pipeline_owners <- c("CTA", "Metra", "Pace", "NITA/RTA", "City of Chicago",
                     "Cook County Land Bank", "Cook County")

# =============================================================================
# TYPOLOGY AND POTENTIAL
# =============================================================================

station_typology <- function(stations) {
  loop_center <- st_sfc(st_point(c(-87.6278, 41.8819)), crs = 4326) %>%
    st_transform(crs_il)
  
  stations %>%
    mutate(dist_loop_ft = as.numeric(st_distance(st_geometry(stations), loop_center))) %>%
    st_drop_geometry() %>%
    transmute(
      station_id,
      dist_loop_ft,
      typology = case_when(
        dist_loop_ft <= downtown_ft ~ "Downtown",
        str_detect(modes, "CTA")    ~ "CTA neighborhood",
        TRUE                        ~ "Metra neighborhood"
      )
    )
}

add_potential <- function(zoned, typ) {
  zoned %>%
    left_join(typ, by = "station_id") %>%
    left_join(target_far_tbl, by = "typology") %>%
    mutate(
      target_far      = target_far * if_else(dist_ft <= inner_ft, inner_bonus, 1),
      
      # Density potential (gross: existing buildings on underbuilt sites are,
      # by definition, low-value relative to the station)
      potential_sqft  = target_far * acres_ws * 43560,
      potential_units = potential_sqft / sqft_per_unit,
      
      # Net of what's there: scale by the value shortfall vs. the station
      # median (same logic as gap_acres). No-value public land counts in full.
      net_units       = potential_units * (1 - pmin(coalesce(ratio_to_med, 0), 1)),
      
      # Zoning call-out: does as-of-right zoning reach the target?
      zoning_status = case_when(
        zone_cat == "PMD (protected industrial)"      ~ "Protected industrial (PMD)",
        zone_cat == "Manufacturing"                   ~ "Industrial: rezoning needed",
        zone_cat == "Planned development"             ~ "PD: check entitlement",
        housing_ok & base_cap_sqft >= potential_sqft  ~ "Zoning allows target",
        housing_ok                                    ~ "Upzoning needed",
        TRUE                                          ~ "Other / not applicable"
      ),
      zoning_cap_share = if_else(housing_ok, base_cap_sqft / potential_sqft, NA_real_),
      zoning_short_units = if_else(housing_ok,
                                   pmax(0, potential_sqft - base_cap_sqft) / sqft_per_unit,
                                   NA_real_)
    )
}

# Candidate universe: underbuilt private sites + developable public land
candidate_sites <- function(pot) {
  pot %>%
    st_drop_geometry() %>%
    filter(
      !site_id %in% exclude_sites,
      !class %in% rank_exclude,
      !linear,
      !owner_group %in% c(jd_exclude, "Right-of-way"),
      !zone_cat %in% not_sites,
      !str_detect(coalesce(owner_main, ""), "CEMETER|^SCI M"),   # cemetery operators
      (underbuilt %in% TRUE & !no_value & acres_ws >= min_acres_private) | is_public
    ) %>%
    mutate(
      site_type   = case_when(is_transit ~ "Transit-owned",
                              is_public  ~ "Other public",
                              TRUE       ~ "Private"),
      public_tier = case_when(!is_public                      ~ NA_character_,
                              owner_group %in% pipeline_owners ~ "Pipeline",
                              TRUE                             ~ "Partnership")
    )
}

# =============================================================================
# OUTPUTS
# =============================================================================

rank_potential <- function(cands, n = 100, rank_by = net_units, types = NULL) {
  cands %>%
    filter(is.null(types) | site_type %in% types) %>%
    arrange(desc({{ rank_by }})) %>%
    select(site_id, site_type, public_tier, owner_group, owner_main, station_name,
           modes, typology, dist_ft, class, acres_ws, tot_mv, ratio_to_med,
           target_far, potential_units, net_units, gap_dollars, implied_mv,
           zone_class, zoning_status, base_far, zoning_cap_share,
           zoning_short_units) %>%
    slice_head(n = n)
}

# The call-out: how much potential is blocked or uncertain under current zoning
zoning_callout <- function(cands) {
  cands %>%
    group_by(site_type, public_tier, zoning_status) %>%
    summarise(
      n_sites         = n(),
      acres_ws        = sum(acres_ws),
      net_units       = sum(net_units),
      short_units     = sum(zoning_short_units, na.rm = TRUE),
      potential_units = sum(potential_units),
      .groups         = "drop"
    ) %>%
    group_by(site_type, public_tier) %>%
    mutate(share_net_units = net_units / sum(net_units)) %>%
    ungroup() %>%
    arrange(site_type, public_tier, desc(net_units))
}

# Subsets are computed before the totals: summarise() would otherwise
# overwrite net_units with its own sum before the later lines use it
station_potential <- function(cands) {
  cands %>%
    group_by(station_id, station_name, modes, typology) %>%
    summarise(
      n_sites           = n(),
      units_private     = sum(net_units[site_type == "Private"]),
      units_pipeline    = sum(net_units[public_tier %in% "Pipeline"]),
      units_partnership = sum(net_units[public_tier %in% "Partnership"]),
      units_transit     = sum(net_units[site_type == "Transit-owned"]),
      units_ok_zoning   = sum(net_units[zoning_status == "Zoning allows target"]),
      units_upzone      = sum(net_units[zoning_status == "Upzoning needed"]),
      units_industrial  = sum(net_units[str_detect(zoning_status, "industrial|Industrial")]),
      units_pd          = sum(net_units[zoning_status == "PD: check entitlement"]),
      gap_dollars       = sum(gap_dollars[site_type == "Private"], na.rm = TRUE),
      acres_ws          = sum(acres_ws),
      net_units         = sum(net_units),
      .groups           = "drop"
    ) %>%
    mutate(share_blocked = (units_upzone + units_industrial) / net_units) %>%
    arrange(desc(net_units))
}

# =============================================================================
# RUN
# =============================================================================

if (!exists("zoned")) zoned <- readRDS("out/zoned.rds")

typ   <- station_typology(stations)
typ %>% count(typology)

pot   <- zoned %>% add_potential(typ)
cands <- pot %>% candidate_sites()
cands %>% count(site_type, public_tier)
cands %>% group_by(site_type, public_tier) %>%
  summarise(sites = n(), net_units = sum(net_units), .groups = "drop")

top_units       <- cands %>% rank_potential(n = 100)
top_units

top_dollars     <- cands %>% rank_potential(n = 100, rank_by = gap_dollars, types = "Private")
top_dollars

top_public_units <- cands %>% rank_potential(n = 100, types = c("Transit-owned", "Other public"))
top_public_units

callout         <- cands %>% zoning_callout()
callout

st_potential    <- cands %>% station_potential()
st_potential

# ---- Export for review ------------------------------------------------------
source("tod_xway.R")
xw  <- load_expressways(crs = st_crs(pot), near = pot)
pot <- drop_expressway_sites(pot, xw)

saveRDS(pot, "out/pot.rds")
write_csv(top_units,        "out/top_units.csv")
write_csv(top_dollars,      "out/top_dollars.csv")
write_csv(top_public_units, "out/top_public_units.csv")
write_csv(callout,          "out/zoning_callout.csv")
write_csv(st_potential,     "out/station_potential.csv")









res_char_id <- "x54s-btds"   # Assessor - Single and Multi-Family Improvement Characteristics
com_val_id  <- "csik-bsws"   # Assessor - Commercial Valuation Data
res_year    <- 2025          # characteristics year for residential
open_coverage     <- 0.15    # building coverage below this = open land
apply_calibration <- FALSE   # TRUE scales footprint estimates by the County/footprint ratio
fp_bins <- c(0, 1000, 2500, 5000, 10000, 25000, 50000, 100000, Inf)   # footprint sq ft
if (!exists("chi_twps")) chi_twps <- as.character(70:77)

# Any SF that isn't from County records is an estimate; dashboards should
# show `sf_estimated` / `existing_sf_source` wherever existing SF appears.

# ---- helpers ----------------------------------------------------------------

soda_pages <- function(base, page = 500000) {
  out <- list(); offset <- 0
  repeat {
    chunk <- read_csv(paste0(base, "&$limit=", format(page, scientific = FALSE),
                             "&$offset=", format(offset, scientific = FALSE)),
                      col_types = cols(.default = "c"))
    out <- c(out, list(chunk))
    if (nrow(chunk) < page) break
    offset <- offset + page
  }
  bind_rows(out)
}

cc_root    <- function(id) paste0("https://datacatalog.cookcountyil.gov/resource/", id, ".csv?")
cc_columns <- function(id) names(read_csv(paste0(cc_root(id), "$limit=1"),
                                          col_types = cols(.default = "c")))
find_col <- function(cols, pattern, what) {
  hit <- str_subset(cols, regex(pattern, ignore_case = TRUE))
  message(what, ": ", if (length(hit)) paste(hit, collapse = ", ") else "NONE FOUND")
  hit[1]
}

# =============================================================================
# 1. COUNTY BUILDING SF (plain table pulls, cached; no spatial work)
# =============================================================================

load_res_sf <- function(year = res_year, cache = "out/res_sf.rds") {
  if (file.exists(cache)) return(readRDS(cache))
  
  cols   <- cc_columns(res_char_id)
  sf_col <- find_col(cols, "bldg_sf|bldg_sq|building_sq", "Residential building SF column")
  tw_col <- find_col(cols, "^township_code$", "Residential township column")
  stopifnot(!is.na(sf_col))
  
  where <- sprintf("year = %d", year)
  if (!is.na(tw_col)) {
    where <- paste0(where, sprintf(" AND %s IN (%s)", tw_col,
                                   paste0("'", chi_twps, "'", collapse = ",")))
  }
  base <- paste0(cc_root(res_char_id), "$select=pin,", sf_col,
                 "&$where=", URLencode(where, reserved = TRUE), "&$order=pin")
  
  out <- soda_pages(base) %>%
    transmute(pin10   = str_sub(str_pad(pin, 14, pad = "0"), 1, 10),
              bldg_sf = suppressWarnings(as.numeric(.data[[sf_col]]))) %>%
    filter(bldg_sf > 0) %>%
    group_by(pin10) %>%                       # sums multiple buildings ("cards")
    summarise(res_sf = sum(bldg_sf), .groups = "drop")
  
  saveRDS(out, cache)
  out
}

# Commercial data covers one triad per year, so keep each property's latest year.
# Rows can repeat per class/model group: identical SF values count once.
load_com_sf <- function(cache = "out/com_sf.rds") {
  if (file.exists(cache)) return(readRDS(cache))
  
  cols    <- cc_columns(com_val_id)
  sf_col  <- find_col(cols, "bldg.?sf|bldg_?sq|building.?s(q|f)", "Commercial building SF column")
  key_col <- find_col(cols, "^keypin$|key_?pin", "Key PIN column")
  pin_col <- find_col(cols, "^pins$", "PIN list column")
  yr_col  <- find_col(cols, "^year$|tax_?year", "Year column")
  stopifnot(!is.na(sf_col), !is.na(key_col), !is.na(yr_col))
  
  base <- paste0(cc_root(com_val_id), "$select=",
                 paste(na.omit(c(key_col, pin_col, yr_col, sf_col)), collapse = ","),
                 "&$order=", key_col)
  
  out <- soda_pages(base) %>%
    transmute(
      keypin  = str_remove_all(.data[[key_col]], "\\D"),
      pins    = if (!is.na(pin_col)) .data[[pin_col]] else NA_character_,
      year    = as.integer(.data[[yr_col]]),
      bldg_sf = suppressWarnings(as.numeric(str_remove_all(.data[[sf_col]], "[^0-9.]")))
    ) %>%
    filter(bldg_sf > 0) %>%
    group_by(keypin) %>%
    filter(year == max(year)) %>%
    group_by(keypin, year) %>%
    summarise(com_sf = sum(unique(bldg_sf)), pins = first(pins), .groups = "drop")
  
  saveRDS(out, cache)
  out
}

site_pin_lookup <- function(pot) {
  pot %>%
    st_drop_geometry() %>%
    select(site_id, pin10s, sqft) %>%
    separate_rows(pin10s, sep = ";") %>%
    rename(pin10 = pin10s)
}

# Residential SF by PIN; commercial SF spread across the sites its PINs fall in
# (by land area), so multi-parcel properties aren't double counted
county_sf_by_site <- function(lookup, res, com) {
  res_site <- res %>%
    inner_join(lookup, by = "pin10") %>%
    group_by(site_id) %>%
    summarise(res_sf = sum(res_sf), .groups = "drop")
  
  com_pins <- com %>%
    mutate(pin10 = map2(pins, keypin, ~ unique(c(
      str_sub(.y, 1, 10),
      str_sub(str_extract_all(str_remove_all(coalesce(.x, ""), "-"), "\\d{10,14}")[[1]], 1, 10)
    )))) %>%
    select(keypin, com_sf, pin10) %>%
    unnest(pin10)
  
  com_site <- com_pins %>%
    inner_join(lookup, by = "pin10") %>%
    distinct(keypin, com_sf, site_id, sqft) %>%
    group_by(keypin) %>%
    mutate(w = sqft / sum(sqft)) %>%
    ungroup() %>%
    group_by(site_id) %>%
    summarise(com_sf = sum(com_sf * w), .groups = "drop")
  
  full_join(res_site, com_site, by = "site_id") %>%
    mutate(county_sf = coalesce(res_sf, 0) + coalesce(com_sf, 0))
}

# =============================================================================
# 2. FOOTPRINTS WITH MISSING STORIES KEPT AS MISSING
# =============================================================================

# Full read (the column-only query dropped geometry last time). New cache name
# so the old defaulted-to-1 cache can't be picked up by mistake.
load_buildings_v2 <- function(path, cache = "out/buildings_v2.rds") {
  if (file.exists(cache)) return(readRDS(cache))
  
  if (str_detect(path, "\\.zip$")) {
    dir <- file.path(tempdir(), "buildings"); unzip(path, exdir = dir); path <- dir
  }
  if (dir.exists(path)) {
    path <- list.files(path, pattern = "\\.(shp|geojson|gpkg)$", full.names = TRUE,
                       recursive = TRUE)[1]
  }
  message("Reading ", path)
  
  b <- st_read(path, quiet = TRUE) %>% st_transform(crs_il)
  story_col <- str_subset(names(b), regex("^stor", ignore_case = TRUE))[1]
  message("Stories column: ", story_col)
  
  pts <- tibble(
    bldg_id     = seq_len(nrow(b)),
    stories_raw = suppressWarnings(as.numeric(b[[story_col]])),
    foot_sqft   = as.numeric(st_area(b))
  ) %>%
    mutate(stories_raw = if_else(stories_raw >= 1, stories_raw, NA_real_)) %>%  # 0 = missing
    st_sf(geometry = st_centroid(st_geometry(b)))
  
  saveRDS(pts, cache)
  pts
}

# Building -> site assignment for candidate sites, cached and incremental
bldg_site_lookup <- function(pot, site_ids, bldg, cache = "out/bldg_site.rds") {
  cached <- if (file.exists(cache)) readRDS(cache) else
    list(lookup = tibble(bldg_id = integer(), site_id = character()), sites = character())
  
  todo <- setdiff(site_ids, cached$sites)
  message(length(todo), " sites to join (", length(cached$sites), " cached)")
  if (length(todo) == 0) return(cached$lookup)
  
  polys <- pot %>% filter(site_id %in% todo) %>% select(site_id)
  bb    <- st_bbox(polys)
  xy    <- st_coordinates(bldg)
  pts   <- bldg[xy[, 1] >= bb["xmin"] & xy[, 1] <= bb["xmax"] &
                  xy[, 2] >= bb["ymin"] & xy[, 2] <= bb["ymax"], "bldg_id"]
  
  hits <- pts %>%
    st_join(polys, join = st_intersects, left = FALSE) %>%
    st_drop_geometry()
  
  cached <- list(lookup = bind_rows(cached$lookup, hits), sites = c(cached$sites, todo))
  saveRDS(cached, cache)
  cached$lookup
}

# Missing stories -> median of known stories for similar footprints in the
# same station typology (falls back to footprint size alone, then to 1)
impute_stories <- function(bldg, lookup, pot, min_n = 30) {
  b <- bldg %>%
    st_drop_geometry() %>%
    inner_join(lookup, by = "bldg_id") %>%
    left_join(pot %>% st_drop_geometry() %>% select(site_id, typology), by = "site_id") %>%
    mutate(fp_bin = cut(foot_sqft, fp_bins, include.lowest = TRUE))
  
  known    <- b %>% filter(!is.na(stories_raw))
  by_group <- known %>% group_by(typology, fp_bin) %>%
    summarise(med_g = median(stories_raw), n_g = n(), .groups = "drop")
  by_bin   <- known %>% group_by(fp_bin) %>%
    summarise(med_b = median(stories_raw), .groups = "drop")
  
  b %>%
    left_join(by_group, by = c("typology", "fp_bin")) %>%
    left_join(by_bin, by = "fp_bin") %>%
    mutate(
      imputed    = is.na(stories_raw),
      stories    = coalesce(stories_raw, if_else(n_g >= min_n, med_g, NA_real_), med_b, 1),
      floor_sqft = foot_sqft * stories
    )
}

footprint_by_site <- function(b_imp) {
  b_imp %>%
    group_by(site_id) %>%
    summarise(
      n_bldgs       = n(),
      foot_sqft     = sum(foot_sqft),
      floor_fp      = sum(floor_sqft),
      floor_imputed = sum(floor_sqft[imputed]),
      max_stories   = max(stories),
      .groups       = "drop"
    )
}

# =============================================================================
# 3. CALIBRATE AND COMBINE
# =============================================================================

# On sites with both sources: how does the footprint estimate compare?
calibrate <- function(pot_c, fp_site, county) {
  pot_c %>%
    st_drop_geometry() %>%
    select(site_id, typology) %>%
    inner_join(fp_site, by = "site_id") %>%
    inner_join(county %>% filter(county_sf > 0), by = "site_id") %>%
    filter(floor_fp > 0) %>%
    mutate(any_imputed = floor_imputed > 0) %>%
    group_by(typology, any_imputed) %>%
    summarise(
      n_sites             = n(),
      med_ratio           = median(county_sf / floor_fp),   # >1 = footprints undercount
      med_implied_stories = median(county_sf / foot_sqft),
      .groups             = "drop"
    )
}

attach_existing_sf <- function(pot_c, fp_site, county, calib) {
  factors <- calib %>%
    group_by(typology) %>%
    summarise(calib_factor = weighted.mean(med_ratio, n_sites), .groups = "drop")
  
  pot_c %>%
    left_join(fp_site, by = "site_id") %>%
    left_join(county %>% select(site_id, county_sf), by = "site_id") %>%
    left_join(factors, by = "typology") %>%
    mutate(
      across(c(n_bldgs, foot_sqft, floor_fp, floor_imputed), ~ coalesce(.x, 0)),
      floor_est = floor_fp * if (apply_calibration) coalesce(calib_factor, 1) else 1,
      
      existing_sf = if_else(coalesce(county_sf, 0) > 0, county_sf, floor_est),
      existing_sf_source = case_when(
        coalesce(county_sf, 0) > 0 ~ "County records",
        n_bldgs == 0               ~ "Estimate: no buildings in footprint layer",
        floor_imputed > 0          ~ "Estimate: footprints, imputed stories",
        TRUE                       ~ "Estimate: footprints, recorded stories"
      ),
      sf_estimated  = existing_sf_source != "County records",
      share_imputed = if_else(floor_fp > 0, floor_imputed / floor_fp, NA_real_),
      
      coverage        = foot_sqft / sqft,
      existing_far    = existing_sf / sqft,
      open_land       = coverage < open_coverage,
      far_headroom    = target_far - existing_far,
      net_units_value = net_units,      # earlier value-based estimate, for comparison
      net_units       = pmax(0, far_headroom) * acres_ws * 43560 / sqft_per_unit
    )
}

# =============================================================================
# 4. OUTPUTS
# =============================================================================

rank_potential <- function(cands, n = 100, rank_by = net_units, types = NULL) {
  cands %>%
    filter(is.null(types) | site_type %in% types) %>%
    arrange(desc({{ rank_by }})) %>%
    select(site_id, site_type, public_tier, owner_group, owner_main, station_name,
           modes, typology, class, acres_ws, coverage, existing_sf, existing_sf_source,
           sf_estimated, existing_far, max_stories, open_land, target_far, far_headroom,
           net_units, gap_dollars, zone_class, zoning_status, base_far, zoning_cap_share) %>%
    slice_head(n = n)
}

source_summary <- function(cands) {
  cands %>%
    group_by(site_type, existing_sf_source) %>%
    summarise(n_sites = n(), net_units = sum(net_units), .groups = "drop") %>%
    group_by(site_type) %>%
    mutate(share_units = net_units / sum(net_units)) %>%
    ungroup()
}

open_land_summary <- function(cands) {
  cands %>%
    group_by(site_type, public_tier, open_land) %>%
    summarise(n_sites = n(), acres_ws = sum(acres_ws), net_units = sum(net_units),
              .groups = "drop")
}

# =============================================================================
# RUN
# =============================================================================

if (!exists("pot")) pot <- readRDS("out/pot.rds")
cand_ids <- pot %>% candidate_sites() %>% pull(site_id)

# Footprints (one full read, cached)
bldg <- load_buildings_v2("C:\\Users\\mdunst\\Downloads\\Building_Footprints_20261020.geojson")   # your file path
bldg %>% st_drop_geometry() %>%
  summarise(n = n(), share_missing_stories = mean(is.na(stories_raw)))

lookup  <- bldg_site_lookup(pot, cand_ids, bldg)
b_imp   <- impute_stories(bldg, lookup, pot)
b_imp %>% count(typology, imputed)          # how much imputation, by typology
fp_site <- footprint_by_site(b_imp)

# County records (watch the printed column names on first run)
res    <- load_res_sf()
com    <- load_com_sf()
county <- county_sf_by_site(site_pin_lookup(pot), res, com)

pot_c <- pot %>% filter(site_id %in% cand_ids)
calib <- calibrate(pot_c, fp_site, county)
calib

pot_e <- attach_existing_sf(pot_c, fp_site, county, calib)
saveRDS(pot_e, "out/pot_existing.rds")

# Sanity checks
pot_e %>% st_drop_geometry() %>% count(existing_sf_source)
pot_e %>% st_drop_geometry() %>%
  group_by(typology) %>%
  summarise(med_far = median(existing_far), p90_far = quantile(existing_far, 0.9),
            share_open = mean(open_land), share_estimated = mean(sf_estimated),
            .groups = "drop")

cands <- pot_e %>% candidate_sites()

sources      <- cands %>% source_summary()
sources
open_summary <- cands %>% open_land_summary()
open_summary
top_units    <- cands %>% rank_potential(n = 100)
top_units
top_open_public <- cands %>% filter(open_land) %>%
  rank_potential(n = 100, types = c("Transit-owned", "Other public"))
top_open_public
callout      <- cands %>% zoning_callout()
callout
st_potential <- cands %>% station_potential()
st_potential

# ---- Export for review ------------------------------------------------------

write_csv(calib,           "out/sf_calibration.csv")
write_csv(sources,         "out/sf_sources.csv")
write_csv(open_summary,    "out/open_land_summary_sf.csv")
write_csv(top_units,       "out/top_units_sf.csv")
write_csv(top_open_public, "out/top_open_public_sf.csv")
write_csv(callout,         "out/zoning_callout_sf.csv")
write_csv(st_potential,    "out/station_potential_sf.csv")







# ---- Density assumptions: realistic but ambitious ---------------------------

fourplex_far  <- 1.5    # traditional 4-flat on a standard 25' x 125' city lot
courtyard_far <- 2.5    # traditional 3-4 story courtyard apartment building
small_lot_ac  <- 0.12   # under ~5,200 sq ft (1-2 standard lots): fourplex
large_lot_ac  <- 0.50   # at/above this: full station target; in between: courtyard
mm_min_units  <- 4      # every single-family lot near a station -> at least a fourplex

# Minimum lot area per dwelling unit, from the Chicago Zoning Ordinance
# (R districts: 17-2-0303; B and C districts: 17-3-0402). Units allowed =
# lot area / this value, also capped by FAR. Transit bonuses are not modeled.
mla_sqft <- c("RS-1" = 6250, "RS-2" = 5000, "RS-3" = 2500,
              "RT-3.5" = 1250, "RT-4" = 1000,
              "RM-4.5" = 700, "RM-5" = 400, "RM-5.5" = 400, "RM-6" = 300, "RM-6.5" = 115)
bc_mla   <- c("1" = 2500, "1.5" = 1350, "2" = 1000, "3" = 400, "5" = 200)   # by dash number

# Active public campuses: shown, but excludable from the public scenario
campus_pattern <- "UNIV|COLLEGE|CHICAGO STATE|INST OF TECH|CITY ?JR"

# Assessor single-family classes (one unit today)
sf_classes <- c("202", "203", "204", "205", "206", "207", "208", "209", "210",
                "234", "278", "295")

allowed_units_zoning <- function(zone_class, lot_sqft, base_far) {
  zc        <- str_to_upper(str_squish(coalesce(zone_class, "")))
  far_units <- floor(coalesce(base_far, 0) * lot_sqft / sqft_per_unit)
  dash      <- str_match(zc, "^[BC][0-9]-([0-9.]+)$")[, 2]
  mla       <- coalesce(unname(mla_sqft[zc]), unname(bc_mla[dash]))
  case_when(
    str_starts(zc, "R") & !is.na(mla) ~ pmax(1, pmin(far_units, floor(lot_sqft / mla))),  # an existing home is always allowed
    !is.na(mla)                       ~ pmin(far_units, floor(lot_sqft / mla)),
    TRUE                              ~ far_units
  )
}

# =============================================================================
# 1. LOT-SIZE DENSITY TIERS
# =============================================================================

lot_tier_of <- function(acres, typology) {
  case_when(
    typology == "Downtown" ~ "Station target (Downtown)",
    acres < small_lot_ac   ~ "Fourplex",
    acres < large_lot_ac   ~ "Courtyard building",
    TRUE                   ~ "Station target"
  )
}

# Re-derive targets, net units, as-of-right units, and zoning status.
# Idempotent: the station-level target is kept in station_far.
retarget <- function(x) {
  if (!"station_far" %in% names(x)) x <- x %>% mutate(station_far = target_far)
  
  x %>%
    mutate(
      lot_tier   = lot_tier_of(acres, typology),
      target_far = case_when(
        lot_tier == "Fourplex"           ~ pmin(station_far, fourplex_far),
        lot_tier == "Courtyard building" ~ pmin(station_far, courtyard_far),
        TRUE                             ~ station_far
      ),
      potential_sqft = target_far * acres_ws * 43560,
      far_headroom   = target_far - existing_far,
      net_units      = pmax(0, far_headroom) * acres_ws * 43560 / sqft_per_unit,
      
      # What current zoning allows without a rezoning (PD/industrial = 0 here)
      aor_far   = if_else(housing_ok, pmin(coalesce(base_far, 0), target_far), 0),
      units_aor = pmax(0, aor_far - existing_far) * acres_ws * 43560 / sqft_per_unit,
      units_aor = if_else(zone_cat %in% c("Single-family", "Multifamily / townhouse", "Business / commercial"),
                          pmin(units_aor, allowed_units_zoning(zone_class, sqft, base_far) * share_in_ws),
                          units_aor),
      
      zoning_status = case_when(
        zone_cat == "PMD (protected industrial)"     ~ "Protected industrial (PMD)",
        zone_cat == "Manufacturing"                  ~ "Industrial: rezoning needed",
        zone_cat == "Planned development"            ~ "PD: check entitlement",
        housing_ok & base_cap_sqft >= potential_sqft ~ "Zoning allows target",
        housing_ok                                   ~ "Upzoning needed",
        TRUE                                         ~ "Other / not applicable"
      ),
      zoning_short_units = if_else(housing_ok,
                                   pmax(0, potential_sqft - base_cap_sqft) / sqft_per_unit,
                                   NA_real_)
    )
}

# =============================================================================
# 2. MISSING MIDDLE: EVERY SINGLE-FAMILY LOT -> FOURPLEX
# =============================================================================

# Unit-based rather than FAR-based: existing units come from the Assessor
# class (County records), so these sites are not flagged as estimates.
missing_middle_sites <- function(pot, exclude_ids) {
  pot %>%
    st_drop_geometry() %>%
    filter(class %in% sf_classes,
           !site_id %in% exclude_ids,
           !site_id %in% exclude_sites) %>%
    mutate(
      site_type          = "Private",
      public_tier        = NA_character_,
      lot_tier           = "Fourplex (missing middle)",
      target_far         = fourplex_far,
      existing_units     = 1,
      target_units       = pmax(mm_min_units, floor(fourplex_far * sqft / sqft_per_unit)),
      net_units          = (target_units - existing_units) * share_in_ws,
      allowed_units      = allowed_units_zoning(zone_class, sqft, base_far),
      units_aor          = pmax(0, pmin(target_units, allowed_units) - existing_units) * share_in_ws,
      zoning_status      = if_else(allowed_units >= target_units,
                                   "Zoning allows target", "Upzoning needed"),
      zoning_short_units = pmax(0, target_units - allowed_units) * share_in_ws,
      existing_sf_source = "County records (single-family class)",
      sf_estimated       = FALSE,
      open_land          = FALSE
    )
}

# =============================================================================
# 3. SCENARIOS
# =============================================================================

flag_scenarios <- function(cands, mm) {
  bind_rows(
    cands %>% mutate(in_mm = FALSE),
    mm    %>% mutate(in_mm = TRUE)
  ) %>%
    mutate(
      in_max       = TRUE,
      in_near_term = !in_mm & open_land %in% TRUE &
        (site_type == "Private" | public_tier %in% "Pipeline"),
      in_public    = site_type != "Private",
      in_transit   = site_type == "Transit-owned",
      
      active_campus       = in_public & str_detect(coalesce(owner_main, ""), campus_pattern),
      in_public_no_campus = in_public & !active_campus,
      
      # Two kinds of estimate: vacant-per-footprints is far firmer than imputed stories
      sf_confidence = case_when(
        str_starts(existing_sf_source, "County records")            ~ "High: County records",
        existing_sf_source == "Estimate: no buildings in footprint layer" ~ "Medium: vacant per 2021 footprints",
        existing_sf_source == "Estimate: footprints, recorded stories"    ~ "Medium: footprints, recorded stories",
        existing_sf_source == "Estimate: footprints, imputed stories"     ~ "Low: footprints, imputed stories",
        TRUE ~ "Unknown"
      ),
      sf_low_conf = str_starts(sf_confidence, "Low")
    )
}

scenarios <- tribble(
  ~scenario,                          ~flag,          ~unit_col,
  "Maximum build-out",                "in_max",       "net_units",
  "Near-term opportunity",            "in_near_term", "net_units",
  "Public land (all public owners)",  "in_public",    "net_units",
  "Public land, excl. active campuses", "in_public_no_campus", "net_units",
  "Transit agency-owned",             "in_transit",   "net_units",
  "Missing middle (fourplexes)",      "in_mm",        "net_units",
  "As-of-right (current zoning)",     "in_max",       "units_aor"
)

scenario_summary <- function(all_sites) {
  pmap_dfr(scenarios, function(scenario, flag, unit_col) {
    d <- all_sites %>% filter(.data[[flag]])
    u <- d[[unit_col]]
    tibble(
      scenario,
      n_sites    = nrow(d),
      n_stations = n_distinct(d$station_id),
      acres_ws   = sum(d$acres_ws),
      units      = sum(u, na.rm = TRUE),
      # Share of units resting on estimated (non-County) existing SF
      share_units_estimated = sum(u[d$sf_estimated %in% TRUE], na.rm = TRUE) /
        sum(u, na.rm = TRUE),
      share_units_low_conf  = sum(u[d$sf_low_conf %in% TRUE], na.rm = TRUE) /
        sum(u, na.rm = TRUE)
    )
  })
}

# Long table (station x scenario) for the dashboard
station_scenarios <- function(all_sites) {
  pmap_dfr(scenarios, function(scenario, flag, unit_col) {
    all_sites %>%
      filter(.data[[flag]]) %>%
      group_by(station_id, station_name, modes, typology) %>%
      summarise(
        n_sites         = n(),
        units           = sum(.data[[unit_col]], na.rm = TRUE),
        units_estimated = sum(.data[[unit_col]][sf_estimated %in% TRUE], na.rm = TRUE),
        units_low_conf  = sum(.data[[unit_col]][sf_low_conf %in% TRUE], na.rm = TRUE),
        .groups         = "drop"
      ) %>%
      mutate(scenario = scenario,
             share_estimated = units_estimated / units,
             share_low_conf  = units_low_conf / units)
  })
}

tier_summary <- function(all_sites) {
  all_sites %>%
    group_by(lot_tier) %>%
    summarise(n_sites = n(), acres_ws = sum(acres_ws), net_units = sum(net_units),
              units_aor = sum(units_aor), .groups = "drop") %>%
    arrange(desc(net_units))
}

# =============================================================================
# RUN
# =============================================================================

if (!exists("pot"))   pot   <- readRDS("out/pot.rds")
if (!exists("pot_e")) pot_e <- readRDS("out/pot_existing.rds")

pot_r <- pot_e %>% retarget()
cands <- pot_r %>% candidate_sites()

mm <- missing_middle_sites(pot, exclude_ids = cands$site_id)
nrow(mm)

all_sites <- flag_scenarios(cands, mm)

tiers <- all_sites %>% tier_summary()
tiers

scen <- all_sites %>% scenario_summary()
scen

st_scen <- all_sites %>% station_scenarios()
st_scen %>% filter(scenario == "Near-term opportunity") %>% arrange(desc(units)) %>% slice_head(n = 15)

# Dashboard-ready site layer: geometry + scenario flags + SF source flags
dashboard_cols <- c(
  "site_id", "pin10s", "station_id", "station_name", "modes", "typology",
  "site_type", "public_tier", "owner_group", "owner_main", "class", "acres",
  "acres_ws", "lot_tier", "existing_sf", "existing_sf_source", "sf_estimated",
  "existing_far", "existing_units", "target_far", "net_units", "units_aor",
  "gap_dollars", "zone_class", "zoning_status", "zoning_short_units",
  "sf_confidence", "sf_low_conf", "active_campus",
  "in_max", "in_near_term", "in_public", "in_public_no_campus", "in_transit", "in_mm"
)
dashboard_sites <- pot %>%
  select(site_id) %>%
  inner_join(all_sites %>% select(any_of(dashboard_cols)), by = "site_id")
saveRDS(dashboard_sites, "out/dashboard_sites.rds")
saveRDS(scenarios,       "out/scenarios.rds")

# ---- Export for review ------------------------------------------------------

write_csv(tiers,   "out/lot_tiers.csv")
write_csv(scen,    "out/scenario_summary.csv")
write_csv(st_scen, "out/station_scenarios.csv")
















