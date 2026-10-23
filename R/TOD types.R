# =============================================================================
# Chicago TOD: station types (separate page at docs/types/)
#
# Gives every station a best-fit type, the reasons for it, and a short reason
# each other type fits less well, under two service scenarios: today's
# schedules and "Transit Future".
#
# Run after tod_10_site_data.R in the same R session. It uses these objects:
#   attrs       site-level table (one row per site)
#   stations    sf points with station_id, station_name, modes
#   st_context  station_id, homes_today, income, renter_share
#   blocks      2020 Census blocks (GEOID), only for the LODES jobs step
#
# Writes:
#   out/station_types.rds        scores, reasons, and inputs for every station
#   docs/types/                  the page (copied from site_types/) plus data
# =============================================================================
setwd("C:\\Users\\mdunst\\OneDrive - Cambridge Systematics\\Documents\\GitHub\\Chicago-TOD")

library(tidyverse)
library(lubridate)
library(sf)
library(jsonlite)

# =============================================================================
# SETTINGS
# =============================================================================

# Column names in `attrs`. Change the right-hand side if yours differ.
col <- c(
  site_type = "site_type",     # "Private" / "Other public" / "Transit-owned"
  zone_cat  = "zone_cat",      # zoning category from the zoning step
  mv_acre   = "mv_per_acre",   # market value per acre (from the value step)
  units     = "net_units",     # new homes at the station target
  acres     = "acres_ws"       # site acres inside the walkshed
)

ind_cats   <- c("PMD (protected industrial)", "Manufacturing")
walk_ft    <- get0("walk_ft", ifnotfound = 2640)  # half mile
rail_match_ft <- 700           # GTFS rail stop -> nearest station within this
bus_ft     <- 1320             # bus stops within a quarter mile count
ao_min_units <- 150            # "Affordable: opportunity" needs at least this
# much public-land capacity (homes)
share_ft   <- 1760             # rail lines at stations within 1/3 mile
# (station point to station point) count here too
lodes_year <- 2022
ind_naics  <- c("CNS05", "CNS06", "CNS08")  # manufacturing, wholesale,
# transportation and warehousing
high_job_q <- 0.75             # map: blocks above this jobs-per-acre quantile
xway_ft    <- 500              # expressway influence distance
xway_max_pen    <- 15          # most points taken off the housing types
xway_full_share <- 0.40        # share of the catchment that gets the full penalty

# Optional: ARO areas as polygons with a field `aro_area` holding one of
# "Downtown", "Inclusionary", "Community Preservation", "Low-Moderate Income".
# Leave the file out and the ARO adjustment is skipped.
aro_file   <- "inputs/aro_areas.geojson"

gtfs_feeds <- c(
  CTA   = "https://www.transitchicago.com/downloads/sch_data/google_transit.zip",
  Metra = "https://schedules.metrarail.com/gtfs/schedule.zip"
)
gtfs_dir   <- "out/gtfs"           # delete this folder to refresh the feeds

site_src   <- "site_types"         # hand-written page files
site_out   <- "docs/types"         # published copy

# =============================================================================
# TRANSIT FUTURE ASSUMPTIONS
# Service is never reduced: each route gets the better of today's schedule
# and the target below.
# =============================================================================

future <- list(
  # 'L': every 6 minutes all day on every line that serves the station today
  l_headway = 6,
  
  # Metra: Regional Rail 2.0 targets from Metra's draft Systemwide Network
  # Plan (October 2026), inner-zone service. Minutes between trains.
  # HC and the South Shore Line are not in the plan and keep today's service.
  metra = tribble(
    ~route_id, ~branch,          ~peak, ~offpeak,
    "UP-N",    "main",            15,    30,
    "UP-NW",   "main",            15,    30,
    "UP-W",    "main",            20,    30,
    "MD-N",    "main",            30,    30,
    "MD-W",    "main",            30,    30,
    "NCS",     "main",            30,    30,
    "BNSF",    "main",            30,    30,
    "SWS",     "main",            15,    30,
    "RI",      "main",            15,    30,
    "RI",      "beverly",         30,    30,
    "ME",      "main",            15,    30,
    "ME",      "south_chicago",   30,    60,
    "ME",      "blue_island",     30,    60
  ),
  
  # Bus: Bus Vision is still in engagement, so "built out" is modeled as
  # every Better Streets for Buses corridor route at Frequent Network
  # standard (10 minutes or better) and every other route at 15 minutes or
  # better, the threshold the Bus Vision Framing Report used for a usable grid.
  bus_frequent_routes = c("1", "3", "4", "X4", "6", "8", "8A", "9", "X9", "12",
                          "15", "26", "28", "35", "49", "X49", "49B", "53",
                          "53A", "55", "63", "66", "74", "79", "80", "95",
                          "100", "108", "112", "115", "134", "135", "136",
                          "143", "146", "147", "148", "151", "J14", "N5", "N9"),
  bus_frequent_headway = 10,
  bus_other_headway    = 15
)

# Stations that don't exist yet. Shown on the map but not scored, because
# their walksheds aren't in the parcel analysis. Coordinates are approximate:
# replace them with the agencies' locations, or add these to `stations` in
# step 1 and rerun the pipeline to score them.
future_stations <- tribble(
  ~name,                         ~project,                          ~lat,     ~lng,
  "103rd St (Red Line)",         "Red Line Extension",              41.7069, -87.6328,
  "111th St (Red Line)",         "Red Line Extension",              41.6925, -87.6331,
  "Michigan Ave (Red Line)",     "Red Line Extension",              41.6838, -87.6210,
  "130th St (Red Line)",         "Red Line Extension",              41.6588, -87.6066,
  "Fulton Market (Metra)",       "Metra infill station (SNP)",      41.8890, -87.6668,
  "Humboldt Park (Metra)",       "Metra infill station (SNP)",      41.9030, -87.7060
)

# =============================================================================
# 1. INPUTS FROM THE EXISTING PIPELINE
# =============================================================================

stopifnot(exists("attrs"), exists("stations"), exists("st_context"))

sites <- if (inherits(attrs, "sf")) st_drop_geometry(attrs) else attrs

# Fall back to out/pot.rds for any listed column `attrs` doesn't carry
missing_cols <- setdiff(col, names(sites))
if (length(missing_cols) && file.exists("out/pot.rds")) {
  pot_extra <- readRDS("out/pot.rds")
  if (inherits(pot_extra, "sf")) pot_extra <- st_drop_geometry(pot_extra)
  take <- intersect(missing_cols, names(pot_extra))
  if (length(take) && "site_id" %in% names(pot_extra)) {
    sites <- sites %>%
      left_join(pot_extra %>% distinct(site_id, .keep_all = TRUE) %>%
                  select(site_id, all_of(take)), by = "site_id")
  }
}
missing_cols <- setdiff(col, names(sites))
if (length(missing_cols)) {
  stop("attrs is missing: ", paste(missing_cols, collapse = ", "),
       ". Point `col` at the right columns.")
}

flag_col <- function(df, nm) {
  if (nm %in% names(df)) coalesce(as.logical(df[[nm]]), FALSE) else rep(FALSE, nrow(df))
}

sites <- sites %>%
  select(-any_of(setdiff(names(col), col))) %>%   # drop columns the renames would collide with
  rename(all_of(col)) %>%
  mutate(skip = flag_col(., "in_mm") | flag_col(., "active_campus")) %>%
  filter(!skip)

downtown_ids <- if ("typology" %in% names(sites)) {
  sites %>% filter(typology == "Downtown") %>% distinct(station_id) %>% pull()
} else character(0)

feat <- sites %>%
  group_by(station_id) %>%
  summarise(
    ind_acres     = sum(acres[zone_cat %in% ind_cats], na.rm = TRUE),
    ws_acres      = sum(acres, na.rm = TRUE),
    mv_acre       = median(mv_acre[site_type == "Private" & mv_acre > 0], na.rm = TRUE),
    cap_units     = sum(units, na.rm = TRUE),
    pub_units     = sum(units[site_type != "Private"], na.rm = TRUE),
    transit_units = sum(units[site_type == "Transit-owned"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(ind_share = if_else(ws_acres > 0, ind_acres / ws_acres, 0),
         mv_acre   = if_else(is.nan(mv_acre), NA_real_, mv_acre))

st_pts <- stations %>%
  select(station_id, station_name, modes) %>%
  filter(station_id %in% feat$station_id)

# =============================================================================
# 2. JOBS NEAR EACH STATION (LODES)
# =============================================================================

if (!requireNamespace("lehdr", quietly = TRUE)) {
  stop("This step needs the lehdr package: install.packages(\"lehdr\")")
}

wac <- lehdr::grab_lodes(state = "il", year = lodes_year, lodes_type = "wac",
                         job_type = "JT00", segment = "S000", agg_geo = "block")
names(wac) <- toupper(names(wac))
wac <- wac %>%
  mutate(across(c(C000, all_of(ind_naics)), as.numeric)) %>%
  transmute(GEOID = W_GEOCODE, jobs = C000,
            ind_jobs = rowSums(across(all_of(ind_naics))))

# Block polygons: the pipeline's `blocks` if it has polygons, else TIGER
blk_src <- if (exists("blocks") &&
               any(st_geometry_type(blocks) %in% c("POLYGON", "MULTIPOLYGON"))) {
  blocks
} else {
  tigris::blocks("IL", "Cook", year = 2020)
}

blk <- blk_src %>%
  rename(any_of(c(GEOID = "GEOID20"))) %>%
  select(GEOID) %>%
  st_transform(st_crs(st_pts)) %>%
  inner_join(wac, by = "GEOID") %>%
  filter(jobs > 0) %>%
  mutate(acres_blk = as.numeric(st_area(.)) / 43560,
         ja        = jobs / pmax(acres_blk, 0.1))
blk_pt <- suppressWarnings(st_point_on_surface(blk))

# Station counts: each block goes to its nearest station, so jobs aren't
# double-counted where walksheds overlap
b_idx  <- st_nearest_feature(blk_pt, st_pts)
b_dist <- as.numeric(st_distance(blk_pt, st_pts[b_idx, ], by_element = TRUE))
jobs <- blk_pt %>%
  st_drop_geometry() %>%
  mutate(station_id = st_pts$station_id[b_idx]) %>%
  filter(b_dist <= walk_ft) %>%
  group_by(station_id) %>%
  summarise(jobs = sum(jobs), ind_jobs = sum(ind_jobs), .groups = "drop")

# Map layers: every block inside each station's half mile
blk_near <- st_is_within_distance(st_pts, blk_pt, dist = walk_ft)
job_thr  <- quantile(blk$ja[unique(unlist(blk_near))], high_job_q, na.rm = TRUE)


# =============================================================================
# 3. SERVICE FROM GTFS: TODAY AND TRANSIT FUTURE
# =============================================================================

hms_sec <- function(x) {
  p <- str_split_fixed(x, ":", 3)
  as.numeric(p[, 1]) * 3600 + as.numeric(p[, 2]) * 60 + as.numeric(p[, 3])
}

read_feed <- function(url, agency) {
  dir.create(gtfs_dir, showWarnings = FALSE, recursive = TRUE)
  zip <- file.path(gtfs_dir, paste0(agency, ".zip"))
  if (!file.exists(zip)) download.file(url, zip, mode = "wb", quiet = TRUE)
  files <- unzip(zip, list = TRUE)$Name
  rd <- function(f, ...) {
    if (!f %in% files) return(NULL)
    read_csv(unz(zip, f), col_types = cols(.default = "c"), progress = FALSE, ...)
  }
  list(
    agency         = agency,
    routes         = rd("routes.txt"),
    trips          = rd("trips.txt"),
    stops          = rd("stops.txt"),
    calendar       = rd("calendar.txt"),
    calendar_dates = rd("calendar_dates.txt"),
    zip            = zip
  )
}

active_services <- function(f, d) {
  wd  <- c("sunday", "monday", "tuesday", "wednesday",
           "thursday", "friday", "saturday")[as.POSIXlt(d)$wday + 1]
  ids <- character(0)
  if (!is.null(f$calendar)) {
    ids <- f$calendar %>%
      filter(.data[[wd]] == "1", ymd(start_date) <= d, ymd(end_date) >= d) %>%
      pull(service_id)
  }
  if (!is.null(f$calendar_dates)) {
    cd  <- f$calendar_dates %>% rename(cdate = date) %>% filter(ymd(cdate) == d)
    ids <- union(setdiff(ids, cd$service_id[cd$exception_type == "2"]),
                 cd$service_id[cd$exception_type == "1"])
  }
  ids
}

# A typical Wednesday: the one with the most trips in the next two months
pick_day <- function(f) {
  days <- Sys.Date() + 0:62
  days <- days[as.POSIXlt(days)$wday == 3]
  n <- map_int(days, ~ sum(f$trips$service_id %in% active_services(f, .x)))
  days[which.max(n)]
}

departures <- function(f) {
  d     <- pick_day(f)
  trips <- f$trips %>%
    filter(service_id %in% active_services(f, d)) %>%
    select(trip_id, route_id, any_of("direction_id")) %>%
    left_join(f$routes %>% select(route_id, route_type, any_of(c(
      "route_short_name", "route_long_name", "route_color"))), by = "route_id")
  
  # stop_times is large, so keep only this day's trips while reading
  keep <- trips$trip_id
  st <- read_csv_chunked(
    unz(f$zip, "stop_times.txt"),
    DataFrameCallback$new(function(x, pos) {
      x[x$trip_id %in% keep, intersect(c("trip_id", "arrival_time", "departure_time",
                                         "stop_id", "stop_sequence"), names(x))]
    }),
    chunk_size = 500000, col_types = cols(.default = "c"), progress = FALSE
  ) %>%
    mutate(t   = hms_sec(coalesce(departure_time, arrival_time)),
           seq = as.integer(stop_sequence)) %>%
    left_join(f$stops %>% select(stop_id, stop_name), by = "stop_id")
  
  # Trips without a direction_id get one from their last stop
  ends <- st %>%
    group_by(trip_id) %>%
    summarise(first_stop = stop_name[which.min(seq)],
              last_stop  = stop_name[which.max(seq)], .groups = "drop")
  
  message(f$agency, ": using service on ", d)
  if (!"direction_id" %in% names(trips)) trips$direction_id <- NA_character_
  st %>%
    inner_join(trips, by = "trip_id") %>%
    left_join(ends, by = "trip_id") %>%
    mutate(agency = f$agency,
           dir    = coalesce(na_if(direction_id, ""), last_stop))
}

feeds <- imap(gtfs_feeds, read_feed)
dep   <- map_dfr(feeds, departures)

win_of <- function(t) case_when(
  t >= 10 * 3600 & t < 15 * 3600 ~ "mid",    # 10 a.m.-3 p.m.
  t >=  7 * 3600 & t <  9 * 3600 ~ "peak",   #  7-9 a.m.
  TRUE ~ NA_character_
)
win_hours <- c(mid = 5, peak = 2)

stops_sf <- map_dfr(feeds, ~ .x$stops %>% mutate(agency = .x$agency)) %>%
  filter(!is.na(stop_lat), !is.na(stop_lon)) %>%
  st_as_sf(coords = c("stop_lon", "stop_lat"), crs = 4326) %>%
  st_transform(st_crs(st_pts))

# ---- Rail -------------------------------------------------------------------

rail_dep <- dep %>% filter(route_type %in% c("1", "2"))

rail_stops <- stops_sf %>%
  semi_join(rail_dep %>% distinct(agency, stop_id), by = c("agency", "stop_id"))
r_idx  <- st_nearest_feature(rail_stops, st_pts)
r_dist <- as.numeric(st_distance(rail_stops, st_pts[r_idx, ], by_element = TRUE))
rail_stop_station <- rail_stops %>%
  st_drop_geometry() %>%
  mutate(station_id = st_pts$station_id[r_idx]) %>%
  filter(r_dist <= rail_match_ft) %>%
  select(agency, stop_id, station_id)

# Branches on the Metra Electric and Rock Island: a stop is on a branch if no
# trip that serves it also reaches the main-line terminal
branch_tbl <- rail_dep %>%
  filter(agency == "Metra", route_id %in% c("ME", "RI")) %>%
  distinct(route_id, stop_id, trip_id, first_stop, last_stop) %>%
  mutate(ends = paste(first_stop, last_stop)) %>%
  group_by(route_id, stop_id) %>%
  summarise(
    main = any(str_detect(ends, if_else(route_id == "ME", "University Park", "Joliet"))),
    sc   = any(str_detect(ends, regex("93rd|South Chicago", ignore_case = TRUE))),
    .groups = "drop"
  ) %>%
  mutate(branch = case_when(
    main                  ~ "main",
    route_id == "ME" & sc ~ "south_chicago",
    route_id == "ME"      ~ "blue_island",
    TRUE                  ~ "beverly"
  )) %>%
  select(route_id, stop_id, branch)

rail_routes_info <- map_dfr(feeds, ~ .x$routes %>% mutate(agency = .x$agency)) %>%
  filter(route_type %in% c("1", "2")) %>%
  transmute(agency, route_id,
            route_name = coalesce(na_if(route_long_name, ""), route_short_name, route_id),
            color = paste0("#", coalesce(na_if(route_color, ""), "56636E")))

rail_home <- rail_dep %>%
  inner_join(rail_stop_station, by = c("agency", "stop_id")) %>%
  mutate(win = win_of(t)) %>%
  filter(!is.na(win)) %>%
  distinct(station_id, agency, route_id, dir, trip_id, win) %>%
  count(station_id, agency, route_id, dir, win) %>%
  group_by(station_id, agency, route_id, win) %>%
  summarise(tph = max(n) / win_hours[first(win)], .groups = "drop") %>%
  pivot_wider(names_from = win, values_from = tph, names_prefix = "tph_",
              values_fill = 0) %>%
  { if (!"tph_mid"  %in% names(.)) mutate(., tph_mid  = 0) else . } %>%
  { if (!"tph_peak" %in% names(.)) mutate(., tph_peak = 0) else . } %>%
  left_join(
    rail_dep %>%
      inner_join(rail_stop_station, by = c("agency", "stop_id")) %>%
      distinct(station_id, route_id, stop_id) %>%
      inner_join(branch_tbl, by = c("route_id", "stop_id")) %>%
      group_by(station_id, route_id) %>%
      summarise(branch = first(branch), .groups = "drop"),
    by = c("station_id", "route_id")
  ) %>%
  mutate(branch = coalesce(branch, "main")) %>%
  left_join(rail_routes_info, by = c("agency", "route_id")) %>%
  left_join(future$metra, by = c("route_id", "branch")) %>%
  mutate(
    # Transit Future: never below today's service
    f_mid  = case_when(
      agency == "CTA"   & tph_mid  > 0 ~ pmax(tph_mid,  60 / future$l_headway),
      agency == "Metra" & !is.na(offpeak) & (tph_mid + tph_peak) > 0 ~ pmax(tph_mid, 60 / offpeak),
      TRUE ~ tph_mid),
    f_peak = case_when(
      agency == "CTA"   & tph_peak > 0 ~ pmax(tph_peak, 60 / future$l_headway),
      agency == "Metra" & !is.na(peak) & (tph_mid + tph_peak) > 0 ~ pmax(tph_peak, 60 / peak),
      TRUE ~ tph_peak)
  ) %>%
  select(station_id, agency, route_id, route_name, color, branch,
         tph_mid, tph_peak, f_mid, f_peak)

message("Metra branch assignments (check these look right):")
print(rail_home %>% filter(branch != "main") %>%
        left_join(st_drop_geometry(st_pts), by = "station_id") %>%
        count(route_id, branch, station_name))


# ---- Rail lines at nearby stations count too ---------------------------------
# Each station gets every line that stops at it or at any station within
# share_ft, counted once at its closest stop.

st_nb <- st_is_within_distance(st_pts, st_pts, dist = share_ft)
nb_pairs <- tibble(
  i = rep(seq_len(nrow(st_pts)), lengths(st_nb)),
  j = unlist(st_nb)
) %>%
  mutate(station_id = st_pts$station_id[i],
         nb_id      = st_pts$station_id[j],
         dist_ft    = as.numeric(st_distance(st_pts[i, ], st_pts[j, ], by_element = TRUE)))

rail_svc <- nb_pairs %>%
  select(station_id, nb_id, dist_ft) %>%
  inner_join(rail_home %>% rename(nb_id = station_id), by = "nb_id",
             relationship = "many-to-many") %>%
  group_by(station_id, agency, route_id) %>%
  arrange(dist_ft, desc(tph_mid + tph_peak), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(at_name = st_pts$station_name[match(nb_id, st_pts$station_id)],
         route_name = if_else(agency == "Metra" & !str_detect(route_name, "Metra"),
                              paste("Metra", route_name), route_name))

# ---- Bus --------------------------------------------------------------------

bus_dep <- dep %>% filter(agency == "CTA", route_type == "3")

bus_stops <- stops_sf %>%
  filter(agency == "CTA") %>%
  semi_join(bus_dep %>% distinct(stop_id), by = "stop_id")
near <- st_is_within_distance(st_pts, bus_stops, dist = bus_ft)
bus_stop_station <- tibble(
  station_id = rep(st_pts$station_id, lengths(near)),
  stop_id    = bus_stops$stop_id[unlist(near)]
)

bus_names <- feeds$CTA$routes %>%
  filter(route_type == "3") %>%
  transmute(route_id,
            bus_label = str_squish(paste(coalesce(route_short_name, route_id),
                                         coalesce(route_long_name, ""))))

bus_svc <- bus_dep %>%
  mutate(win = win_of(t)) %>%
  filter(win == "mid") %>%
  inner_join(bus_stop_station, by = "stop_id", relationship = "many-to-many") %>%
  distinct(station_id, route_id, dir, trip_id) %>%
  count(station_id, route_id, dir) %>%
  group_by(station_id, route_id) %>%
  summarise(tph = max(n) / win_hours[["mid"]], .groups = "drop") %>%
  mutate(f_tph = if_else(route_id %in% future$bus_frequent_routes,
                         pmax(tph, 60 / future$bus_frequent_headway),
                         pmax(tph, 60 / future$bus_other_headway))) %>%
  left_join(bus_names, by = "route_id")

# ---- Service tiers ------------------------------------------------------------
# Midday weekday service. "Frequent" means every 10 minutes or better
# (6+ an hour). Tiers, highest that applies:
#   1 Car-free:     frequent 'L' + frequent bus; or 2+ frequent 'L' lines;
#                   or frequent 'L' + Metra every 15 min or better
#   2 Car-light:    frequent 'L'; or 'L' every 11-15 min + frequent bus;
#                   or all-day rail + 2+ frequent buses
#   3 Transit-supported: any 'L'; or Metra every 30 min or better + frequent
#                   bus; or 2+ frequent buses
#   4 Commuter rail: everything else

tier_labels <- c("Car-free", "Car-light", "Transit-supported", "Commuter rail")
tier_svc    <- c(100, 75, 45, 15)
tier_build  <- c("mid- and high-rise buildings",
                 "mid-rise buildings up to about six to eight stories",
                 "courtyard buildings of three to four stories",
                 "two-flats and four-flats")

summarise_service <- function(mcol, pcol, bcol) {
  rs <- rail_svc %>%
    mutate(m = .data[[mcol]], p = .data[[pcol]]) %>%
    group_by(station_id) %>%
    summarise(
      freq_L       = sum(agency == "CTA" & m >= 6),
      mid_L        = sum(agency == "CTA" & m >= 4 & m < 6),
      any_L        = sum(agency == "CTA" & m > 0),
      metra15      = any(agency == "Metra" & m >= 4),
      metra30      = any(agency == "Metra" & m >= 2),
      n_lines      = sum(m + p > 0),
      n_freq_lines = sum((agency == "CTA" & m >= 6) | (agency == "Metra" & m >= 4)),
      .groups = "drop")
  bs <- bus_svc %>%
    group_by(station_id) %>%
    summarise(freq_bus = sum(.data[[bcol]] >= 6), .groups = "drop")
  
  st_pts %>%
    st_drop_geometry() %>%
    select(station_id) %>%
    left_join(rs, by = "station_id") %>%
    left_join(bs, by = "station_id") %>%
    mutate(
      across(c(freq_L, mid_L, any_L, n_lines, n_freq_lines, freq_bus),
             ~ coalesce(as.numeric(.x), 0)),
      across(c(metra15, metra30), ~ coalesce(.x, FALSE)),
      all_day = any_L > 0 | metra30,
      tier = case_when(
        (freq_L >= 1 & freq_bus >= 1) | freq_L >= 2 | (freq_L >= 1 & metra15) ~ 1L,
        freq_L >= 1 | (mid_L >= 1 & freq_bus >= 1) | (all_day & freq_bus >= 2) ~ 2L,
        any_L >= 1 | (metra30 & freq_bus >= 1) | freq_bus >= 2                 ~ 3L,
        TRUE                                                                    ~ 4L),
      tier_label = tier_labels[tier],
      svc        = tier_svc[tier],
      intensity  = tier_build[tier],
      # Job centers: how many lines meet (6 lines = full marks) and how many
      # of them are frequent
      reach = 0.7 * 100 * pmin(1, pmax(0, n_lines - 1) / 5) +
        0.3 * 100 * pmin(1, n_freq_lines / 3)
    )
}

# Plain description of a station's service, e.g.
# "Red Line every 7 minutes plus the 9 Ashland bus"
and_list <- function(x) {
  if (length(x) <= 1) return(paste(x, collapse = ""))
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

describe_service <- function(id, mcol, bcol) {
  rl <- rail_svc %>% filter(station_id == id, .data[[mcol]] > 0) %>%
    arrange(desc(.data[[mcol]]))
  bl <- bus_svc %>% filter(station_id == id, .data[[bcol]] >= 6) %>%
    arrange(desc(.data[[bcol]]))
  rail <- if (nrow(rl)) {
    shown <- head(rl, 2)
    txt <- and_list(sprintf("%s every %d minutes", shown$route_name,
                            round(60 / shown[[mcol]])))
    if (nrow(rl) > 2) txt <- sprintf("%s, among %d lines", txt, nrow(rl))
    txt
  } else "no midday trains"
  bus <- if (nrow(bl) == 1) sprintf("the %s bus", bl$bus_label) else
    if (nrow(bl) == 2) sprintf("the %s buses", and_list(bl$bus_label)) else
      if (nrow(bl) > 2)  sprintf("%d frequent bus routes", nrow(bl)) else ""
  if (nzchar(bus)) paste(rail, "plus", bus) else rail
}

service <- list(
  today  = summarise_service("tph_mid", "tph_peak", "tph") %>%
    mutate(svc_text = map_chr(station_id, describe_service, mcol = "tph_mid", bcol = "tph")),
  future = summarise_service("f_mid", "f_peak", "f_tph") %>%
    mutate(svc_text = map_chr(station_id, describe_service, mcol = "f_mid", bcol = "f_tph"))
)

# =============================================================================
# 4. EXPRESSWAYS
# =============================================================================
# Share of each station's half-mile catchment within xway_ft of an
# expressway (Census TIGER primary roads). Names come from the route number
# and location, so check the printout.

source("tod_xway.R")   # shared with the Room to Grow pipeline

st_buf <- st_buffer(st_pts %>% select(station_id), walk_ft)
xw <- load_expressways(crs = st_crs(st_pts), near = st_buf, pad_ft = xway_ft)

message("Expressway segments by name (check these):")
print(count(st_drop_geometry(xw), xname))

xw_buf   <- xw %>% st_buffer(xway_ft) %>% group_by(xname) %>% summarise(.groups = "drop")
buf_area <- pi * walk_ft^2

xw_by_name <- suppressWarnings(st_intersection(st_buf, xw_buf)) %>%
  mutate(a = as.numeric(st_area(.))) %>%
  st_drop_geometry()
xw_total <- suppressWarnings(st_intersection(st_buf, st_sf(geometry = st_union(xw_buf)))) %>%
  mutate(a = as.numeric(st_area(.))) %>%
  st_drop_geometry() %>%
  group_by(station_id) %>%
  summarise(xway_share = sum(a) / buf_area, .groups = "drop")

xway <- xw_total %>%
  left_join(xw_by_name %>%
              filter(a / buf_area >= 0.02) %>%
              group_by(station_id) %>%
              arrange(desc(a), .by_group = TRUE) %>%
              summarise(xway_names = list(xname), .groups = "drop"),
            by = "station_id") %>%
  mutate(xway_pen = xway_max_pen * pmin(1, xway_share / xway_full_share))

xway_note <- function(share, nms) {
  if (is.null(share) || is.na(share) || share < 0.05 || !length(nms)) return(NA_character_)
  many <- length(nms) > 1
  sprintf(paste0("About %d%% of the area within the station\u2019s catchment is within %d feet of %s, ",
                 "which, in %s current state, %s with noise and air pollution, traffic safety hazards, ",
                 "and %s not optimal for housing."),
          as.integer(5 * round(100 * share / 5)), xway_ft,
          and_list(paste("the", nms)),
          if (many) "their" else "its", if (many) "come" else "comes",
          if (many) "are" else "is")
}

# =============================================================================
# 5. OPTIONAL: ARO AREAS
# =============================================================================

aro <- tibble(station_id = st_pts$station_id[0], aro_area = character())
if (file.exists(aro_file)) {
  aro_poly <- st_read(aro_file, quiet = TRUE) %>% st_transform(st_crs(st_pts))
  aro <- st_join(st_pts, aro_poly["aro_area"], left = FALSE) %>%
    st_drop_geometry() %>%
    distinct(station_id, .keep_all = TRUE) %>%
    select(station_id, aro_area)
}

aro_adj <- tribble(
  ~aro_area,                 ~type, ~adj,
  "Downtown",                "hi",   8,
  "Inclusionary",            "mr",   8,
  "Inclusionary",            "ao",   8,
  "Inclusionary",            "ai",  -8,
  "Community Preservation",  "pf",  12,
  "Community Preservation",  "mr",  -6,
  "Low-Moderate Income",     "ai",  10,
  "Low-Moderate Income",     "mr",  -6
)

# =============================================================================
# 6. FACTORS, TYPES, AND SCORES
# =============================================================================

pct <- function(x) 100 * percent_rank(x)

base <- st_pts %>%
  st_drop_geometry() %>%
  left_join(feat, by = "station_id") %>%
  left_join(st_context %>% select(station_id, homes_today, income, renter_share),
            by = "station_id") %>%
  left_join(jobs, by = "station_id") %>%
  left_join(aro,  by = "station_id") %>%
  left_join(xway, by = "station_id") %>%
  mutate(
    jobs       = coalesce(jobs, 0),
    ind_jobs   = coalesce(ind_jobs, 0),
    ind_job_share = if_else(jobs > 0, ind_jobs / jobs, 0),
    xway_share = coalesce(xway_share, 0),
    xway_pen   = coalesce(xway_pen, 0),
    downtown   = station_id %in% downtown_ids,
    mv         = pct(mv_acre),
    inc        = pct(income),
    rent       = pct(renter_share),
    cap        = pct(cap_units),
    pub        = if_else(coalesce(pub_units, 0) > 0,
                         pct(if_else(pub_units > 0, pub_units, NA_real_)), 0),
    jobs_p     = pct(jobs),
    ind_jobs_p = pct(ind_jobs),
    ind_mix    = 100 * pmin(1, ind_job_share / 0.4),
    ind        = 100 * pmin(1, coalesce(ind_share, 0) / 0.4),
    dtn        = if_else(downtown, 100, 0),
    low_mv     = 100 - mv,
    low_inc    = 100 - inc,
    low_cap    = 100 - cap,
    press      = pmin(100, 2 * pmax(0, mv - inc))   # land values running ahead of incomes
  )

types <- tribble(
  ~type, ~comp,       ~w,
  "hi",  "reach",     0.40, "hi", "jobs",    0.35, "hi", "mv",   0.15, "hi", "dtn",     0.10,
  "mr",  "mv",        0.35, "mr", "svc",     0.25, "mr", "cap",  0.25, "mr", "inc",     0.15,
  "ao",  "pub",       0.35, "ao", "mv",      0.25, "ao", "svc",  0.25, "ao", "inc",     0.15,
  "ai",  "low_mv",    0.30, "ai", "cap",     0.30, "ai", "svc",  0.20, "ai", "low_inc", 0.20,
  "pf",  "press",     0.40, "pf", "rent",    0.35, "pf", "low_cap", 0.25,
  "ir",  "ind_jobs",  0.50, "ir", "ind_mix", 0.35, "ir", "ind",  0.15
)
housing_types <- c("mr", "ao", "ai")

type_names <- c(
  hi = "Job center", mr = "Market-rate housing",
  ao = "Affordable housing on public land", ai = "Affordable housing, investment-led",
  pf = "Protect existing homes first", ir = "Keep industry"
)

score_scenario <- function(scn) {
  sv <- service[[scn]] %>%
    select(station_id, svc, reach, tier, tier_label, intensity, svc_text,
           n_lines, freq_bus)
  b  <- base %>% left_join(sv, by = "station_id")
  
  long <- b %>%
    select(station_id, reach, jobs = jobs_p, svc, mv, dtn, cap, inc, pub, low_mv,
           low_inc, press, rent, low_cap, ind_jobs = ind_jobs_p, ind_mix, ind) %>%
    pivot_longer(-station_id, names_to = "comp", values_to = "v") %>%
    inner_join(types, by = "comp", relationship = "many-to-many")
  
  scores <- long %>%
    filter(!is.na(v)) %>%
    group_by(station_id, type) %>%
    summarise(score = sum(w * v) / sum(w), .groups = "drop") %>%
    left_join(b %>% select(station_id, aro_area, pub_units, n_lines, xway_pen),
              by = "station_id") %>%
    left_join(aro_adj, by = c("aro_area", "type")) %>%
    mutate(
      score = score + coalesce(adj, 0),
      score = if_else(type %in% housing_types, score - xway_pen, score),
      gated = (type == "ao" & coalesce(pub_units, 0) < ao_min_units) |
        (type == "hi" & n_lines < 2),
      score = if_else(gated & type == "ao", score * 0.6, score),
      score = if_else(gated & type == "hi", score * 0.5, score),
      score = pmin(100, pmax(0, score))
    ) %>%
    select(station_id, type, score, gated, adj)
  
  list(b = b, long = long, scores = scores)
}

# =============================================================================
# 7. PLAIN-LANGUAGE REASONS
# =============================================================================

fmt_n   <- function(x) if (is.na(x) || x < 50) "fewer than 50" else
  if (x >= 1000) format(signif(round(x), 2), big.mark = ",") else format(round(x, -1), big.mark = ",")
fmt_usd <- function(x) {
  if (is.na(x)) return("not available")
  if (x >= 1e6) sprintf("$%.1fM", x / 1e6) else sprintf("$%sK", round(x / 1e3))
}
p0 <- function(x) round(coalesce(x, 50))

why_yes <- function(comp, r) {
  switch(comp,
         reach    = sprintf("As a multi-route transfer station, %s is well suited to be a job center with its larger-than-average labor pool. It has %d rail lines within a third of a mile.",
                            r$station_name, as.integer(r$n_lines)),
         jobs     = sprintf("About %s jobs within a half mile, more than at %d%% of station areas.", fmt_n(r$jobs), p0(r$jobs_p)),
         svc      = sprintf("%s service: %s.", r$tier_label, r$svc_text),
         mv       = sprintf("Land is worth more than at %d%% of station areas (about %s an acre).", p0(r$mv), fmt_usd(r$mv_acre)),
         dtn      = "It's part of the downtown office market.",
         cap      = sprintf("Room for about %s new homes on underused land, more than at %d%% of station areas.", fmt_n(r$cap_units), p0(r$cap)),
         inc      = sprintf("Household incomes are higher than at %d%% of station areas.", p0(r$inc)),
         pub      = sprintf("Public land could hold about %s homes%s.", fmt_n(r$pub_units),
                            if (coalesce(r$transit_units, 0) >= 50) sprintf(", %s of them on transit-agency land", fmt_n(r$transit_units)) else ""),
         low_mv   = sprintf("Land costs less than at %d%% of station areas, so subsidy goes further.", p0(r$low_mv)),
         low_inc  = sprintf("Incomes are lower than at %d%% of station areas, so affordable homes meet a local need.", p0(r$low_inc)),
         press    = "Land values are well ahead of local incomes, a common sign of displacement pressure.",
         rent     = sprintf("%d%% of households rent.", round(100 * coalesce(r$renter_share, 0))),
         low_cap  = "Little vacant or underused land, so growth here would press on existing homes.",
         ind_jobs = sprintf("About %s industrial jobs (manufacturing, wholesale, and logistics) within a half mile, more than at %d%% of station areas.",
                            fmt_n(r$ind_jobs), p0(r$ind_jobs_p)),
         ind_mix  = sprintf("Industrial work makes up %d%% of the jobs nearby.", round(100 * r$ind_job_share)),
         ind      = sprintf("%d%% of the walkshed is zoned for industry.", round(100 * coalesce(r$ind_share, 0))),
         ""
  )
}

svc_short <- function(r) {
  if (r$tier == 3) sprintf("Transit-supported service (%s) isn't enough for most households to go car-light.", r$svc_text)
  else sprintf("Mostly commuter-rail service (%s), so most households here would still need a car.", r$svc_text)
}

why_not <- function(type, comp, r) {
  switch(paste(type, comp),
         "hi reach"     = sprintf("%d rail lines meet here, fewer than at the city's main transfer stations, so its labor pool is smaller.", as.integer(r$n_lines)),
         "hi jobs"      = sprintf("Few jobs nearby today (about %s within a half mile), so there's no employment center to build on.", fmt_n(r$jobs)),
         "hi mv"        = "Land values are too low to support office or lab construction.",
         "hi dtn"       = "It's outside the downtown office market, and office demand outside it is thin.",
         "mr mv"        = sprintf("Land values are lower than at %d%% of station areas, so rents likely won't cover new construction without help.", p0(r$low_mv)),
         "mr svc"       = svc_short(r),
         "mr cap"       = "Little underused land left to build on.",
         "mr inc"       = "Local incomes are below what new market-rate rents would require.",
         "ao pub"       = if (coalesce(r$pub_units, 0) < 50) "There's little or no public land here, and that's what makes affordable housing work in a costly market." else
           sprintf("Public land here holds only about %s homes, too little to anchor a project.", fmt_n(r$pub_units)),
         "ao mv"        = "Land is inexpensive here, so affordable housing doesn't need a public-land write-down to work.",
         "ao svc"       = svc_short(r),
         "ao inc"       = "Incomes are already low, so this isn't where affordable homes add the most access to opportunity.",
         "ai low_mv"    = sprintf("Land is expensive (more than at %d%% of station areas), so subsidy dollars go less far.", p0(r$mv)),
         "ai cap"       = "Not much underused land for new buildings.",
         "ai svc"       = svc_short(r),
         "ai low_inc"   = "Incomes are relatively high, so market-rate or inclusionary housing fits better.",
         "pf press"     = "Land values aren't running ahead of incomes, so displacement pressure looks modest.",
         "pf rent"      = "Most households own, so renter displacement is a smaller risk.",
         "pf low_cap"   = "There's plenty of underused land, so new buildings don't have to come at existing homes' expense.",
         "ir ind_jobs"  = sprintf("Few industrial jobs nearby (about %s), so there's little industry to protect.", fmt_n(r$ind_jobs)),
         "ir ind_mix"   = sprintf("Industrial work is only %d%% of the jobs nearby.", round(100 * r$ind_job_share)),
         "ir ind"       = sprintf("Little industrial zoning (%d%% of the walkshed).", round(100 * coalesce(r$ind_share, 0))),
         ""
  )
}

aro_line <- function(area) if (is.na(area)) "" else
  sprintf("The City's ARO map puts it in a %s area.", area)

explain <- function(scn) {
  s <- score_scenario(scn)
  
  ranked <- s$scores %>%
    group_by(station_id) %>%
    arrange(desc(score), .by_group = TRUE) %>%
    mutate(rank = row_number(), margin = score - dplyr::lead(score),
           best_type = first(type)) %>%
    ungroup()
  
  rows <- split(s$b, s$b$station_id)
  
  ranked %>%
    left_join(s$long %>% select(station_id, type, comp, w, v),
              by = c("station_id", "type"), relationship = "many-to-many") %>%
    group_by(station_id, type, score, gated, adj, rank, best_type) %>%
    summarise(
      reasons = list({
        r  <- rows[[as.character(first(station_id))]]
        t  <- first(type)
        ok <- !is.na(v)
        if (first(rank) == 1) {
          # Best fit: the two factors contributing most, among those where
          # the station actually stands out
          strong <- ok & v >= 50
          pool   <- if (any(strong)) strong else ok
          top <- comp[pool][order(-(w[pool] * v[pool]))][1:min(2, sum(pool))]
          c(map_chr(top, why_yes, r = r),
            if (!is.na(first(adj)) && first(adj) > 0) aro_line(r$aro_area))
        } else if (first(gated) && t == "ao") {
          why_not("ao", "pub", r)
        } else if (first(gated) && t == "hi") {
          sprintf("With a single rail line, %s reaches a smaller labor pool than the multi-route transfer stations best suited to be job centers.",
                  r$station_name)
        } else {
          # Others: the factor with the biggest shortfall, but never one the
          # station is actually fine on
          weak <- ok & v < 50
          if (any(weak)) {
            worst <- comp[weak][which.max(w[weak] * (100 - v[weak]))]
            why_not(t, worst, r)
          } else {
            sprintf("It fits here too, but %s scores higher on balance.",
                    tolower(type_names[[first(best_type)]]))
          }
        }
      }),
      .groups = "drop"
    ) %>%
    left_join(ranked %>% select(station_id, type, margin), by = c("station_id", "type")) %>%
    left_join(s$b %>% select(station_id, tier, tier_label, intensity, svc_text,
                             n_lines, freq_bus),
              by = "station_id")
}

res <- list(today = explain("today"), future = explain("future"))

# =============================================================================
# 8. ASSEMBLE AND WRITE
# =============================================================================

stamp <- format(Sys.time(), "%Y%m%d%H%M")

st_ll <- st_pts %>%
  st_transform(4326) %>%
  mutate(lng = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry()

scenario_block <- function(x, id, scn) {
  e   <- x %>% filter(station_id == id) %>% arrange(rank)
  rl  <- rail_svc %>% filter(station_id == id) %>% arrange(desc(tph_mid + tph_peak))
  bus <- bus_svc %>% filter(station_id == id)
  mid  <- if (scn == "today") rl$tph_mid  else rl$f_mid
  peak <- if (scn == "today") rl$tph_peak else rl$f_peak
  btph <- if (scn == "today") bus$tph else bus$f_tph
  list(
    best      = e$type[1],
    lead      = round(e$margin[1], 1),
    tier      = e$tier[1],
    tier_label = e$tier_label[1],
    intensity = e$intensity[1],
    svc_text  = e$svc_text[1],
    n_lines   = e$n_lines[1],
    freq_bus  = sum(btph >= 6),
    scores    = set_names(as.list(round(e$score)), e$type),
    reasons   = set_names(map(e$reasons, unname), e$type),
    lines     = map(seq_len(nrow(rl)), ~ list(
      name  = rl$route_name[.x], color = rl$color[.x],
      at    = if (rl$nb_id[.x] != id) rl$at_name[.x] else NULL,
      dist  = if (rl$nb_id[.x] != id) round(rl$dist_ft[.x], -1) else NULL,
      mid   = if (mid[.x]  > 0) round(60 / mid[.x], 1)  else NULL,
      peak  = if (peak[.x] > 0) round(60 / peak[.x], 1) else NULL))
  )
}

out_stations <- base %>%
  left_join(st_ll %>% select(station_id, lng, lat), by = "station_id") %>%
  arrange(station_name) %>%
  pmap(function(...) {
    r <- list(...)
    list(
      id = r$station_id, name = r$station_name, modes = r$modes,
      lat = round(r$lat, 5), lng = round(r$lng, 5),
      downtown = r$downtown, aro = r$aro_area,
      note = xway_note(r$xway_share, r$xway_names),
      facts = list(
        homes_today = r$homes_today, income = r$income,
        renter_share = r$renter_share, mv_acre = r$mv_acre,
        cap_units = r$cap_units, pub_units = r$pub_units,
        transit_units = r$transit_units, ind_share = r$ind_share,
        jobs = r$jobs, ind_jobs = r$ind_jobs, xway_share = r$xway_share),
      today  = scenario_block(res$today,  r$station_id, "today"),
      future = scenario_block(res$future, r$station_id, "future")
    )
  })

assumptions <- list(
  l_headway = future$l_headway,
  metra = future$metra,
  bus_frequent_headway = future$bus_frequent_headway,
  bus_other_headway = future$bus_other_headway,
  bus_frequent_routes = future$bus_frequent_routes,
  aro = nrow(aro) > 0
)

payload <- list(
  generated = format(Sys.Date(), "%B %d, %Y"),
  version   = stamp,
  walk_ft   = walk_ft,
  share_ft  = share_ft,
  xway_ft   = xway_ft,
  types     = imap(type_names, ~ list(id = .y, name = .x)),
  stations  = out_stations,
  future_stations = future_stations,
  assumptions = assumptions
)

saveRDS(list(base = base, service = service, res = res, rail_svc = rail_svc,
             bus_svc = bus_svc, xway = xway), "out/station_types.rds")

dir.create(file.path(site_out, "data", "st"), recursive = TRUE, showWarnings = FALSE)

writeLines(paste0("window.TYPES_DATA = ",
                  toJSON(payload, auto_unbox = TRUE, na = "null", null = "null", digits = 6),
                  ";"),
           file.path(site_out, "data", "types.js"), useBytes = TRUE)

# ---- Map layers for each station, loaded when the station is selected ---------

fc_json <- function(x) {
  if (is.null(x) || !nrow(x)) return('{"type":"FeatureCollection","features":[]}')
  tmp <- tempfile(fileext = ".geojson")
  st_write(x, tmp, quiet = TRUE, delete_dsn = TRUE,
           layer_options = c("COORDINATE_PRECISION=5", "RFC7946=YES"))
  paste(readLines(tmp, warn = FALSE), collapse = "")
}

site_geom <- if (inherits(attrs, "sf")) {
  attrs %>% select(site_id)
} else if (file.exists("out/pot.rds")) {
  g <- readRDS("out/pot.rds")
  if (inherits(g, "sf") && "site_id" %in% names(g)) g %>% select(site_id) %>% distinct(site_id, .keep_all = TRUE) else NULL
} else NULL

if (is.null(site_geom) || !"site_id" %in% names(sites)) {
  message("No site geometry found (attrs or out/pot.rds with site_id), so the ",
          "parcel layers are skipped. The job blocks still draw.")
  map_sites <- NULL
} else {
  owner_col <- intersect(c("owner_main", "owner_name", "owner"), names(sites))[1]
  map_sites <- site_geom %>%
    st_transform(st_crs(st_pts)) %>%
    inner_join(sites %>%
                 transmute(site_id, station_id, site_type, zone_cat, units,
                           o = if (!is.na(owner_col)) .data[[owner_col]] else NA_character_),
               by = "site_id") %>%
    mutate(k   = case_when(site_type == "Transit-owned" ~ "tr",
                           site_type != "Private"       ~ "pub",
                           TRUE                         ~ "ub"),
           ind = as.integer(zone_cat %in% ind_cats),
           u   = round(units)) %>%
    filter(k != "ub" | u > 0 | ind == 1) %>%
    st_simplify(preserveTopology = TRUE, dTolerance = 3)
}

blk_map <- blk %>%
  mutate(hi = as.integer(ja >= job_thr & jobs >= 25),
         ii = as.integer(ind_jobs >= 25),
         j = round(jobs), ij = round(ind_jobs), ja = round(ja, 1)) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 5)

unlink(list.files(file.path(site_out, "data", "st"), full.names = TRUE))
for (i in seq_len(nrow(st_pts))) {
  id <- st_pts$station_id[i]
  s_sites <- if (!is.null(map_sites)) map_sites %>% filter(station_id == id) %>%
    select(k, ind, u, o) %>% st_transform(4326) else NULL
  s_blk <- blk_map[blk_near[[i]], ] %>% filter(hi == 1 | ii == 1) %>%
    select(j, ij, ja, hi, ii) %>% st_transform(4326)
  writeLines(sprintf('(window.TYPES_SITES = window.TYPES_SITES || {})["%s"] = {"sites":%s,"blocks":%s};',
                     id, fc_json(s_sites), fc_json(s_blk)),
             file.path(site_out, "data", "st", paste0(id, ".js")), useBytes = TRUE)
}

# Rail lines for the map: reuse the cached routes from the dashboard step
if (file.exists("out/rail_routes.rds")) {
  rr  <- readRDS("out/rail_routes.rds") %>% st_transform(4326)
  ccol <- intersect(c("color", "route_color", "colour"), names(rr))[1]
  rr <- rr %>%
    mutate(color = if (!is.na(ccol)) .data[[ccol]] else "#56636E",
           color = if_else(str_starts(color, "#"), color, paste0("#", color))) %>%
    select(color)
  writeLines(paste0("window.TYPES_RAIL = ", fc_json(rr), ";"),
             file.path(site_out, "data", "rail.js"), useBytes = TRUE)
} else {
  writeLines("window.TYPES_RAIL = null;", file.path(site_out, "data", "rail.js"))
}

# Copy the hand-written page with a cache-busting stamp
for (f in c("index.html", "app.js", "styles.css")) {
  txt <- readLines(file.path(site_src, f), warn = FALSE, encoding = "UTF-8")
  if (f == "index.html") txt <- str_replace_all(txt, fixed("{{VERSION}}"), stamp)
  writeLines(txt, file.path(site_out, f), useBytes = TRUE)
}

# ---- Quick check ------------------------------------------------------------

summary_tbl <- map_dfr(out_stations, ~ tibble(
  station = .x$name, tier_today = .x$today$tier, tier_future = .x$future$tier,
  today = .x$today$best, future = .x$future$best))
message("Best type, today vs Transit Future:")
print(count(summary_tbl, today, future))
message("Service tiers, today vs Transit Future:")
print(count(summary_tbl, tier_today, tier_future))
message(sum(summary_tbl$today != summary_tbl$future), " stations change type.")

check_names <- c("Clybourn", "35th", "Logan Square", "Clark/Lake")
print(summary_tbl %>% filter(str_detect(station, paste(check_names, collapse = "|"))))