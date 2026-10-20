# =============================================================================
# Chicago TOD redevelopment analysis
# Part 7: Data + generated pages for the public site
#
# Writes into docs/:
#   data/*.js        data files the page loads (work from file:// and Pages)
#   wards/*.html     one printable summary per ward (+ map images)
#   data.html        "Explore the data" tables
#   preview.png      social preview image
#   index.html, app.js, styles.css   copied from site/ (hand-written assets)
#
# Inputs: out/dashboard_sites.rds, out/stations.rds, out/rail_routes.rds,
#         a downloaded ward boundary file, a Census API key (tidycensus)
# Needs the tod_05 / tod_08 settings in session for the methods text.
# =============================================================================
setwd("C:\\Users\\mdunst\\OneDrive - Cambridge Systematics\\Documents\\GitHub\\Chicago-TOD")


library(tidyverse)
library(sf)
library(jsonlite)
library(tidycensus)
library(htmltools)
library(DT)
library(scales)
# ---- Settings ---------------------------------------------------------------

site_src    <- "site
"     # folder holding index.html, app.js, styles.css
docs        <- "docs"
wards_path  <- "https://data.cityofchicago.org/api/v3/views/p293-wvbd/query.geojson"   # City data portal download
author_name <- "Michael Dunst"
repo_url    <- "https://github.com/mtdunst/Chicago-TOD"
site_url    <- "https://mtdunst.github.io/Chicago-TOD/"
n_map_sites <- 2500       # largest sites drawn as parcels
acs_year    <- 2023
if (!exists("crs_il"))  crs_il  <- 3435
if (!exists("walk_ft")) walk_ft <- 2640
# census_api_key("YOUR KEY", install = TRUE)   # once, if you haven't already

for (d in c(docs, file.path(docs, "data"), file.path(docs, "wards"))) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ---- Plain-language scenarios and policy levers ----------------------------

# Display order = button order on the page
scen_def <- tribble(
  ~key,        ~flag,                 ~unit_col,   ~label,                                    ~description,
  "near",      "in_near_term",        "net_units", "Build on open land first",                "Vacant lots and surface parking, on private land and on land the City, County, Land Bank, or a transit agency could offer directly. No demolition.",
  "aor",       "in_max",              "aor",       "What today's zoning allows",              "Only the homes current zoning already permits, on the same sites as a full build-out.",
  "mm",        "in_mm",               "net_units", "If fourplexes were legal near stations",  "Every single-family lot within a half mile of a station becomes a fourplex.",
  "public",    "in_public",           "net_units", "If public land were offered",             "Every publicly owned site, including schools, Chicago Housing Authority land, and state land.",
  "public_nc", "in_public_no_campus", "net_units", "Public land, without active campuses",    "The same public land, leaving out university and college campuses.",
  "transit",   "in_transit",          "net_units", "If transit agencies built on their land", "Land owned by CTA, Metra, Pace, and the regional transit authority.",
  "max",       "in_max",              "net_units", "Full build-out",                          "Every underused site at its target density, plus fourplexes on single-family lots."
)

lever_def <- tribble(
  ~key,         ~label,                                    ~color,
  "base",       "Allowed by today's zoning",               "#41B6E6",
  "fourplex",   "Allow fourplexes near stations",          "#E4002B",
  "city",       "Offer City, County, and Land Bank land",  "#B5001F",
  "transit",    "Build on transit agency land",            "#1C5D99",
  "partner",    "Partner with other public landowners",    "#3C7A5A",
  "industrial", "Rezone underused industrial land",        "#8A5A2B",
  "pd",         "Finish approved planned developments",    "#6B4C9A",
  "upzone",     "Upzone other underused private sites",    "#56636E"
)

plain_status <- function(x) {
  recode(x,
         "Zoning allows target"        = "Zoning already allows this",
         "Upzoning needed"             = "Needs a zoning change",
         "Industrial: rezoning needed" = "Zoned for industry",
         "Protected industrial (PMD)"  = "Protected industrial district",
         "PD: check entitlement"       = "Planned development (approved plan)",
         .default = "Other zoning")
}
conf_level <- function(x) {
  case_when(str_starts(x, "High") ~ "solid", str_starts(x, "Medium") ~ "likely",
            str_starts(x, "Low") ~ "rough", TRUE ~ "likely")
}
# Same rounding as the page: honest precision, never exact counts
approx_r <- function(n) {
  if (is.na(n)) return("-")
  if (n < 50) return("fewer than 50")
  r <- if (n < 1000) round(n / 50) * 50 else { p <- 10^(floor(log10(n)) - 1); round(n / p) * p }
  comma(r)
}

# ---- Helpers: write data as JS so the page works from file:// too ----------

write_js <- function(obj, name, file) {
  writeLines(paste0("window.", name, " = ",
                    toJSON(obj, auto_unbox = TRUE, digits = NA, na = "null", null = "null"), ";"),
             file, useBytes = TRUE)
}
geo_js <- function(x, name, file, precision = 5) {
  tmp <- tempfile(fileext = ".geojson")
  st_write(st_transform(x, 4326), tmp, driver = "GeoJSON", quiet = TRUE, delete_dsn = TRUE,
           layer_options = c(paste0("COORDINATE_PRECISION=", precision), "RFC7946=YES"))
  writeLines(paste0("window.", name, " = ", paste(readLines(tmp, warn = FALSE), collapse = ""), ";"),
             file, useBytes = TRUE)
}

# =============================================================================
# 1. LOAD
# =============================================================================

sites    <- readRDS("out/dashboard_sites.rds")
stations <- readRDS("out/stations.rds") %>% st_transform(crs_il)
rail     <- readRDS("out/rail_routes.rds")

wards <- st_read(wards_path, quiet = TRUE) %>% st_transform(crs_il)
ward_col <- str_subset(names(wards), regex("^ward", ignore_case = TRUE))[1]
wards <- wards %>% transmute(ward = as.integer(.data[[ward_col]])) %>% st_make_valid()
city  <- st_union(wards)

# Census: 2020 block housing counts, ACS tract income and renters (cached)
load_blocks <- function(cache = "out/blocks.rds") {
  if (file.exists(cache)) return(readRDS(cache))
  b <- get_decennial(geography = "block", variables = "H1_001N", year = 2020, sumfile = "pl",
                     state = "IL", county = "Cook", geometry = TRUE) %>%
    st_transform(crs_il) %>% transmute(GEOID, homes = value)
  saveRDS(b, cache); b
}
load_tracts <- function(cache = "out/tracts_acs.rds") {
  if (file.exists(cache)) return(readRDS(cache))
  t <- get_acs(geography = "tract", year = acs_year, state = "IL", county = "Cook",
               variables = c(income = "B19013_001", hh = "B25003_001", renters = "B25003_003"),
               output = "wide", geometry = TRUE) %>%
    st_transform(crs_il) %>% transmute(GEOID, income = incomeE, hh = hhE, renters = rentersE)
  saveRDS(t, cache); t
}
blocks <- load_blocks()
tracts <- load_tracts()

# =============================================================================
# 2. SITE-LEVEL PREP: levers, wards, confidence
# =============================================================================

attrs <- sites %>%
  st_drop_geometry() %>%
  mutate(
    aor   = coalesce(pmin(units_aor, net_units), 0),     # allowed today, never above target
    gain  = pmax(0, net_units - aor),                      # what a policy change adds
    lever = case_when(
      in_mm                                  ~ "fourplex",
      site_type == "Transit-owned"           ~ "transit",
      public_tier %in% "Pipeline"            ~ "city",
      public_tier %in% "Partnership"         ~ "partner",
      str_detect(zoning_status, "ndustrial") ~ "industrial",
      zoning_status == "PD: check entitlement" ~ "pd",
      TRUE                                   ~ "upzone"
    ),
    conf = conf_level(sf_confidence)
  )

# Ward for every site (interior point join)
w_hit <- st_intersects(st_point_on_surface(st_geometry(sites)), wards)
attrs$ward <- wards$ward[map_int(w_hit, ~ if (length(.x)) .x[1] else NA_integer_)]

scenario_units <- function(df, group = character()) {
  pmap_dfr(scen_def %>% select(key, flag, unit_col), function(key, flag, unit_col) {
    df %>%
      filter(.data[[flag]]) %>%
      group_by(across(all_of(group))) %>%
      summarise(units = sum(.data[[unit_col]], na.rm = TRUE),
                est   = sum(.data[[unit_col]][sf_estimated %in% TRUE], na.rm = TRUE),
                low   = sum(.data[[unit_col]][sf_low_conf %in% TRUE], na.rm = TRUE),
                sites = n(), .groups = "drop") %>%
      mutate(key = key,
             conf = case_when(units == 0 ~ "solid", low / units > 0.25 ~ "rough",
                              est / units > 0.5 ~ "likely", TRUE ~ "solid"))
  })
}
lever_units <- function(df, group = character()) {
  base <- df %>% group_by(across(all_of(group))) %>%
    summarise(units = sum(aor), .groups = "drop") %>% mutate(lever = "base")
  gains <- df %>% group_by(across(all_of(c(group, "lever")))) %>%
    summarise(units = sum(gain), .groups = "drop")
  bind_rows(base, gains)
}

# =============================================================================
# 3. STATION CONTEXT: homes today, income, renters
# =============================================================================

bpts <- blocks %>% filter(homes > 0) %>% st_point_on_surface()
bpts <- bpts[lengths(st_intersects(bpts, city)) > 0, ]
b_idx <- st_nearest_feature(bpts, stations)
b_dst <- as.numeric(st_distance(bpts, stations[b_idx, ], by_element = TRUE))

st_context <- bpts %>%
  st_drop_geometry() %>%
  mutate(station_id = stations$station_id[b_idx], tract = str_sub(GEOID, 1, 11)) %>%
  filter(b_dst <= walk_ft) %>%
  left_join(st_drop_geometry(tracts), by = c("tract" = "GEOID")) %>%
  group_by(station_id) %>%
  summarise(homes_today  = sum(homes),
            income       = weighted.mean(income, homes, na.rm = TRUE),
            renter_share = weighted.mean(renters / hh, homes, na.rm = TRUE),
            .groups = "drop")

# =============================================================================
# 4. CITYWIDE, STATIONS, WARDS
# =============================================================================

city_scen  <- scenario_units(attrs)
city_lever <- lever_units(attrs)
u <- function(k) city_scen$units[city_scen$key == k]

st_scen  <- scenario_units(attrs, "station_id")
st_lever <- lever_units(attrs, "station_id")
w_scen   <- scenario_units(attrs %>% filter(!is.na(ward)), "ward")
w_lever  <- lever_units(attrs %>% filter(!is.na(ward)), "ward")

st_ll <- stations %>% st_transform(4326)
st_ll <- st_ll %>%
  mutate(lng = st_coordinates(st_ll)[, 1], lat = st_coordinates(st_ll)[, 2]) %>%
  st_drop_geometry() %>%
  select(station_id, station_name, modes, lng, lat)

typ_lookup  <- attrs %>% distinct(station_id, typology)
st_wards    <- attrs %>% filter(!is.na(ward)) %>% count(station_id, ward) %>%
  group_by(station_id) %>% summarise(wards = list(sort(ward)), .groups = "drop")

top_sites <- attrs %>%
  filter(!in_mm) %>%
  group_by(station_id) %>%
  slice_max(net_units, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  transmute(station_id, id = site_id,
            owner   = str_to_title(coalesce(owner_main, "Owner not listed")),
            type    = site_type, units = round(net_units), allowed = round(aor),
            zoning  = coalesce(zone_class, ""), status = plain_status(zoning_status),
            conf, tier = lot_tier, acres = round(acres_ws, 1))

as_named <- function(df, key_col, val_col) setNames(as.list(round(df[[val_col]])), df[[key_col]])

station_ids <- unique(st_scen$station_id)
stations_out <- map(station_ids, function(sid) {
  b  <- st_ll %>% filter(station_id == sid)
  cx <- st_context %>% filter(station_id == sid)
  s  <- st_scen %>% filter(station_id == sid)
  l  <- st_lever %>% filter(station_id == sid)
  list(
    id = sid, name = b$station_name, modes = b$modes,
    typology = typ_lookup$typology[typ_lookup$station_id == sid][1],
    lat = round(b$lat, 5), lng = round(b$lng, 5),
    wards = { wl <- st_wards$wards[st_wards$station_id == sid]; if (length(wl)) wl[[1]] else list() },
    homes_today  = if (nrow(cx)) round(cx$homes_today) else NULL,
    income       = if (nrow(cx) && !is.nan(cx$income)) round(cx$income, -2) else NULL,
    renter_share = if (nrow(cx) && !is.nan(cx$renter_share)) round(cx$renter_share, 2) else NULL,
    units  = as_named(s, "key", "units"),
    conf   = setNames(as.list(s$conf), s$key),
    levers = as_named(l, "lever", "units"),
    top    = top_sites %>% filter(station_id == sid) %>% select(-station_id)
  )
})

ward_ids  <- sort(unique(w_scen$ward))
st_in_ward <- st_join(stations %>% select(station_id), wards) %>% st_drop_geometry() %>%
  left_join(st_ll %>% select(station_id, station_name), by = "station_id")
wards_out <- map(ward_ids, function(w) {
  s <- w_scen %>% filter(ward == w); l <- w_lever %>% filter(ward == w)
  list(ward = w,
       units  = as_named(s, "key", "units"),
       conf   = setNames(as.list(s$conf), s$key),
       levers = as_named(l, "lever", "units"),
       stations = sort(unique(st_in_ward$station_name[st_in_ward$ward == w])),
       print_url = sprintf("wards/ward-%02d.html", w))
})

# =============================================================================
# 5. FEATURED PLACES (edit the text freely; numbers fill in automatically)
# =============================================================================

sid <- function(nm) {
  id <- st_ll$station_id[st_ll$station_name == nm]
  if (!length(id)) warning("Station not found: ", nm)
  id
}
site_sum <- function(ids) {
  miss <- setdiff(ids, attrs$site_id)
  if (length(miss)) warning("Featured site IDs not found (check the text): ", paste(miss, collapse = ", "))
  attrs %>% filter(site_id %in% ids) %>%
    summarise(acres = sum(acres_ws), units = sum(net_units), aor = sum(aor))
}

orange_ids <- unlist(map(c("Ashland (Orange)", "Halsted (Orange)", "Kedzie (Orange)", "Western (Orange)"), sid))
orange     <- attrs %>% filter(station_id %in% orange_ids)
sox_ids    <- c("1733218001", "1733400048", "1733124001")
cta79_ids  <- c("2028416008", "2028416003", "2028416002", "2028416001")
roos_ids   <- c("1721210092", "1721210086")
np_id      <- sid("Norwood Park")
np         <- attrs %>% filter(station_id %in% np_id, in_mm)

featured <- list(
  list(key = "orange", title = "The Orange Line's industrial corridor",
       body = sprintf("Four Orange Line stations, from Western to Halsted, have more underused land than almost anywhere in the city: room for about %s homes. About %s of that sits on land zoned for industry, much of it working businesses. Here, building housing near transit is also a decision about jobs.",
                      approx_r(sum(orange$net_units)),
                      percent(sum(orange$gain[orange$lever == "industrial"]) / sum(orange$net_units), 1)),
       station_ids = orange_ids, site_ids = character()),
  list(key = "sox", title = "Parking lots next to a Red Line station",
       body = sprintf("The Illinois Sports Facilities Authority owns about %s acres around the ballpark at 35th Street, much of it surface parking a short walk from the Sox-35th station. At the station's target density, that land could hold about %s homes.",
                      round(site_sum(sox_ids)$acres), approx_r(site_sum(sox_ids)$units)),
       station_ids = character(), site_ids = sox_ids),
  list(key = "norwood", title = "A Metra stop surrounded by single-family homes",
       body = sprintf("The area around the Norwood Park Metra station is almost all single-family homes. Allowing fourplexes on those lots would add about %s homes over time, one lot at a time. Today's zoning allows about %s of them.",
                      approx_r(sum(np$net_units)), approx_r(sum(np$aor))),
       station_ids = np_id, site_ids = character()),
  list(key = "cta79", title = "Transit land at 79th Street",
       body = sprintf("CTA owns about %s acres near the 79th Street Red Line station, likely including working bus and rail facilities. Building above or beside active transit facilities is harder and costlier, but it has precedent in other cities. Here it could add about %s homes.",
                      round(site_sum(cta79_ids)$acres), approx_r(site_sum(cta79_ids)$units)),
       station_ids = character(), site_ids = cta79_ids),
  list(key = "roosevelt", title = "Approved, but not yet built",
       body = sprintf("Two large lots near Roosevelt station already have planned-development zoning and appeared empty in the City's most recent building footprints. Sites like these don't need a rezoning; they need projects to move forward. Together they could hold about %s homes.",
                      approx_r(site_sum(roos_ids)$units)),
       station_ids = character(), site_ids = roos_ids)
)
featured_site_ids <- unique(unlist(map(featured, "site_ids")))

# =============================================================================
# 6. MAP LAYERS
# =============================================================================

map_sites <- sites %>%
  left_join(attrs %>% select(site_id, aor, conf, ward), by = "site_id") %>%
  filter(!in_mm) %>%
  { bind_rows(slice_max(., net_units, n = n_map_sites, with_ties = FALSE),
              filter(., site_id %in% featured_site_ids)) } %>%
  distinct(site_id, .keep_all = TRUE) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 10) %>%
  transmute(id = site_id,
            owner   = str_to_title(coalesce(owner_main, "Owner not listed")),
            type    = site_type, station = station_name,
            units   = round(net_units), allowed = round(aor),
            zoning  = coalesce(zone_class, ""), status = plain_status(zoning_status),
            conf, campus = active_campus %in% TRUE, tier = lot_tier,
            acres   = round(acres_ws, 1))

tracts_city <- tracts[lengths(st_intersects(tracts, city)) > 0, ] %>%
  filter(!is.na(income))
inc_breaks <- quantile(tracts_city$income, probs = seq(0, 1, 0.2), na.rm = TRUE)
tracts_out <- tracts_city %>%
  mutate(bin = pmin(findInterval(income, inc_breaks, rightmost.closed = TRUE), 5) - 1) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 40) %>%
  transmute(income = round(income, -2), bin)
inc_labels <- sprintf("$%sk to $%sk", round(head(inc_breaks, -1) / 1000), round(tail(inc_breaks, -1) / 1000))

wards_geo <- wards %>% st_simplify(preserveTopology = TRUE, dTolerance = 30)

# =============================================================================
# 7. SUMMARY + WRITE DATA FILES
# =============================================================================

summary_out <- list(
  generated   = format(Sys.Date(), "%B %Y"),
  author      = author_name, repo_url = repo_url, site_url = site_url,
  homes_today = round(sum(st_context$homes_today)),
  scenarios   = map(seq_len(nrow(scen_def)), function(i) {
    k <- scen_def$key[i]; s <- city_scen %>% filter(key == k)
    list(key = k, label = scen_def$label[i], description = scen_def$description[i],
         units = round(s$units), sites = s$sites, conf = s$conf,
         share_est = round(s$est / s$units, 3), share_low = round(s$low / s$units, 3))
  }),
  levers      = map(seq_len(nrow(lever_def)), ~ as.list(lever_def[.x, ])),
  city_levers = as_named(city_lever, "lever", "units"),
  income_labels = inc_labels,
  assumptions = list(
    fourplex_far = fourplex_far, courtyard_far = courtyard_far,
    small_lot_ac = small_lot_ac, large_lot_ac = large_lot_ac,
    station_far  = setNames(as.list(target_far_tbl$target_far), target_far_tbl$typology),
    inner_bonus  = if (exists("inner_bonus")) inner_bonus else 1.25,
    sqft_per_unit = sqft_per_unit, mm_min_units = mm_min_units
  )
)

write_js(list(summary = summary_out, stations = stations_out, wards = wards_out,
              featured = featured), "TOD", file.path(docs, "data", "core.js"))
geo_js(rail,       "TOD_RAIL",   file.path(docs, "data", "rail.js"))
geo_js(map_sites,  "TOD_SITES",  file.path(docs, "data", "sites.js"))
geo_js(tracts_out, "TOD_TRACTS", file.path(docs, "data", "tracts.js"), precision = 4)
geo_js(wards_geo,  "TOD_WARDS",  file.path(docs, "data", "wards.js"), precision = 5)

# Downloads
write_csv(city_scen %>% left_join(scen_def %>% select(key, label), by = "key"),
          file.path(docs, "data", "scenarios.csv"))
write_csv(st_scen %>% left_join(st_ll %>% select(station_id, station_name), by = "station_id"),
          file.path(docs, "data", "stations_by_scenario.csv"))
write_csv(attrs %>% select(-any_of("pin10s")), file.path(docs, "data", "all_sites.csv"))

# =============================================================================
# 8. WARD PRINT PAGES (one letter-size page each)
# =============================================================================

type_cols <- c("Private" = "#8A97A3", "Other public" = "#3C7A5A", "Transit-owned" = "#1C5D99")
rail_il   <- st_transform(rail, crs_il)
sites_w   <- sites %>% left_join(attrs %>% select(site_id, ward), by = "site_id") %>% filter(!in_mm)
near_pts  <- stations %>% left_join(st_scen %>% filter(key == "near") %>% select(station_id, units),
                                    by = "station_id")

ward_map <- function(w, file) {
  wp <- wards %>% filter(ward == w)
  bb <- st_bbox(st_buffer(wp, 1500))
  s  <- sites_w %>% filter(ward == w) %>% slice_max(net_units, n = 400, with_ties = FALSE)
  p <- ggplot() +
    geom_sf(data = wp, fill = "white", color = "#1F2A33", linewidth = 0.6) +
    geom_sf(data = s, aes(fill = site_type), color = NA) +
    geom_sf(data = st_crop(rail_il, bb), aes(color = color), linewidth = 0.8) +
    geom_sf(data = st_crop(near_pts, bb), aes(size = units), shape = 21,
            fill = "#41B6E6", color = "#1F2A33", alpha = 0.85) +
    scale_color_identity() +
    scale_fill_manual(values = type_cols, name = "Site owner") +
    scale_size_area(max_size = 9, guide = "none") +
    coord_sf(xlim = bb[c("xmin", "xmax")], ylim = bb[c("ymin", "ymax")], datum = NA) +
    theme_void(base_size = 11) +
    theme(legend.position = "bottom")
  ggsave(file, p, width = 7, height = 5.2, dpi = 150, bg = "white")
}

ward_css <- "
@page { size: letter; margin: 0.6in; }
body { font: 400 11pt/1.45 'Barlow', system-ui, sans-serif; color:#1F2A33; max-width: 7.3in; margin: 24px auto; }
h1 { font: 700 26pt/1.1 'Barlow Semi Condensed', sans-serif; margin: 0 0 4px; }
.sub { color:#56636E; margin: 0 0 14px; }
img { width:100%; border:1px solid #D3DADF; }
table { border-collapse: collapse; width:100%; margin: 10px 0; }
td { padding: 3px 0; border-bottom: 1px solid #E3E8EB; }
td.n { text-align:right; font-variant-numeric: tabular-nums; }
.lev { display:grid; grid-template-columns: 2.6in 1fr 0.9in; gap: 4px 10px; align-items:center; margin: 8px 0 12px; }
.lev .bar { height: 12px; }
.lev .n { text-align:right; font-variant-numeric: tabular-nums; }
.note { color:#56636E; font-size: 9pt; }
@media print { a { color: inherit; text-decoration: none; } .noprint { display:none; } }
"

for (w in ward_ids) {
  img <- sprintf("ward-%02d.png", w)
  ward_map(w, file.path(docs, "wards", img))
  s <- w_scen %>% filter(ward == w); l <- w_lever %>% filter(ward == w)
  lv <- lever_def %>% left_join(l, by = c("key" = "lever")) %>% mutate(units = coalesce(units, 0))
  lmax <- max(sum(lv$units), 1)
  sts  <- sort(unique(st_in_ward$station_name[st_in_ward$ward == w]))
  
  page <- tagList(
    tags$head(tags$meta(charset = "utf-8"),
              tags$title(sprintf("Ward %s: housing near rail stations", w)),
              tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Barlow:wght@400;600&family=Barlow+Semi+Condensed:wght@700&display=swap"),
              tags$style(HTML(ward_css))),
    tags$body(
      tags$p(class = "noprint", tags$a(href = "../index.html", "Back to the full map")),
      tags$h1(sprintf("Ward %s", w)),
      tags$p(class = "sub", if (length(sts)) paste("Stations in this ward:", paste(sts, collapse = ", "))
             else "Areas of this ward within a half mile of a rail station"),
      tags$img(src = img, alt = sprintf("Map of Ward %s showing rail lines, stations, and the largest sites", w)),
      tags$table(map(seq_len(nrow(scen_def)), function(i) {
        k <- scen_def$key[i]; v <- s$units[s$key == k]
        tags$tr(tags$td(scen_def$label[i]), tags$td(class = "n", approx_r(if (length(v)) v else 0)))
      })),
      tags$p(tags$strong("Which decisions unlock the most homes here")),
      tags$div(class = "lev", map(seq_len(nrow(lv)), function(i) tagList(
        tags$span(lv$label[i]),
        tags$span(class = "bar", style = sprintf("background:%s;width:%.1f%%", lv$color[i], 100 * lv$units[i] / lmax)),
        tags$span(class = "n", approx_r(lv$units[i]))))),
      tags$p(class = "note", sprintf(
        "Capacity, not a forecast: homes that could fit under the stated density targets, rounded. Existing building sizes come from Cook County Assessor records where available and are estimated elsewhere. Prepared by %s, %s. Full map and methods: %s",
        author_name, summary_out$generated, site_url))
    )
  )
  save_html(page, file.path(docs, "wards", sprintf("ward-%02d.html", w)))
}

# =============================================================================
# 9. SOCIAL PREVIEW IMAGE + DATA PAGE + COPY SITE ASSETS
# =============================================================================

prev <- ggplot() +
  geom_sf(data = city, fill = "#F5F7F8", color = "#D3DADF") +
  geom_sf(data = st_crop(rail_il, st_bbox(city)), aes(color = color), linewidth = 0.7) +
  geom_sf(data = near_pts %>% filter(!is.na(units)), aes(size = units), shape = 21,
          fill = "#41B6E6", color = "#1F2A33", alpha = 0.8) +
  scale_color_identity() + scale_size_area(max_size = 10, guide = "none") +
  coord_sf(datum = NA) + theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA))
ggsave(file.path(docs, "preview.png"), prev, width = 8, height = 4.2, dpi = 150)

scen_tbl <- city_scen %>%
  left_join(scen_def %>% select(key, label), by = "key") %>%
  transmute(Scenario = label, `New homes` = round(units), Sites = sites,
            `Share on estimated building data` = est / units)
st_tbl <- st_scen %>%
  left_join(scen_def %>% select(key, label), by = "key") %>%
  left_join(st_ll %>% select(station_id, station_name, modes), by = "station_id") %>%
  group_by(station_name) %>%
  mutate(Station = if (n_distinct(station_id) > 1) paste0(station_name, " (", modes, ")")
         else station_name) %>%
  ungroup() %>%
  select(station_id, Station, label, units) %>%
  pivot_wider(names_from = label, values_from = units, values_fill = 0) %>%
  select(-station_id)
site_tbl <- st_drop_geometry(map_sites) %>%
  transmute(Owner = owner, `Site type` = type, Station = station, `New homes` = units,
            `Allowed today` = allowed, Zoning = zoning, `Zoning status` = status,
            `Building data` = conf, Acres = acres)
dt_o <- list(pageLength = 15, dom = "Bfrtip", buttons = list("csv"), scrollX = TRUE)

data_page <- tagList(
  tags$head(tags$meta(charset = "utf-8"), tags$title("Explore the data"),
            tags$link(rel = "stylesheet", href = "styles.css")),
  tags$main(class = "data-page",
            tags$p(tags$a(href = "index.html", "Back to the map")),
            tags$h1("Explore the data"),
            tags$p(class = "note", "Every number is rounded on the main page; these tables show the model's unrounded estimates. They are capacity estimates, not forecasts."),
            tags$h2("Scenarios"),
            datatable(scen_tbl, rownames = FALSE, options = list(dom = "t")) %>%
              formatRound(c("New homes", "Sites"), 0) %>% formatPercentage("Share on estimated building data", 0),
            tags$h2("Stations"),
            datatable(st_tbl, rownames = FALSE, filter = "top", extensions = "Buttons", options = dt_o) %>%
              formatRound(setdiff(names(st_tbl), "Station"), 0),
            tags$h2("Largest sites"),
            datatable(site_tbl, rownames = FALSE, filter = "top", extensions = "Buttons", options = dt_o),
            tags$p(class = "note", "Downloads: ",
                   tags$a(href = "data/scenarios.csv", "scenarios"), ", ",
                   tags$a(href = "data/stations_by_scenario.csv", "stations"), ", ",
                   tags$a(href = "data/all_sites.csv", "all sites"), ".")
  )
)
save_html(data_page, file.path(docs, "data.html"), libdir = "lib")

# Copy the hand-written page and fill in the sharing metadata
idx <- readLines(file.path(site_src, "index.html"), warn = FALSE)
idx <- idx %>%
  str_replace_all(fixed("{{SITE_URL}}"), site_url) %>%
  str_replace_all(fixed("{{DESCRIPTION}}"), sprintf(
    "About %s new homes could fit on open land near Chicago's L and Metra stations. See what could be built near your station and which decisions would unlock it.",
    approx_r(u("near"))))
writeLines(idx, file.path(docs, "index.html"), useBytes = TRUE)
file.copy(file.path(site_src, c("app.js", "styles.css")), docs, overwrite = TRUE)

message("Site written to ", docs, "/. Preview with: servr::httd('", docs, "') or open index.html directly.")
