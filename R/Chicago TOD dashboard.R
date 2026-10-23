# =============================================================================
# Chicago TOD redevelopment analysis
# Part 6: Static dashboard page (Leaflet + DT), ready for GitHub Pages
# Inputs: out/dashboard_sites.rds, out/scenarios.rds, out/stations.rds
# Output: docs/index.html (+ docs/lib, docs/data). Publish the docs/ folder.
# =============================================================================

library(tidyverse)
library(sf)
library(leaflet)
library(DT)
library(htmltools)
library(scales)

n_map_sites <- 3000      # parcels drawn on the map (largest by net units)
dash_dir    <- "docs"    # GitHub Pages can serve a repo's /docs folder
if (!exists("crs_il")) crs_il <- 3435
low_conf_flag <- 0.25    # station gets a dagger if >25% of units are low-confidence

dir.create(file.path(dash_dir, "data"), recursive = TRUE, showWarnings = FALSE)

# Assumption values for the methods section come from tod_05 / tod_08 settings
needed <- c("fourplex_far", "courtyard_far", "small_lot_ac", "large_lot_ac",
            "mm_min_units", "sqft_per_unit", "target_far_tbl")
missing <- needed[!vapply(needed, exists, logical(1))]
if (length(missing)) stop("Run the settings blocks of tod_05/tod_08 first; missing: ",
                          paste(missing, collapse = ", "))

sites     <- readRDS("out/dashboard_sites.rds")
scenarios <- readRDS("out/scenarios.rds")
stations  <- readRDS("out/stations.rds")
attrs     <- st_drop_geometry(sites)

# =============================================================================
# SUMMARIES
# =============================================================================

scenario_table <- pmap_dfr(scenarios, function(scenario, flag, unit_col) {
  d <- attrs %>% filter(.data[[flag]])
  u <- d[[unit_col]]
  tibble(
    Scenario         = scenario,
    Sites            = nrow(d),
    Stations         = n_distinct(d$station_id),
    `Walkshed acres` = sum(d$acres_ws),
    Units            = sum(u, na.rm = TRUE),
    `Share on estimated SF`      = sum(u[d$sf_estimated %in% TRUE], na.rm = TRUE) / sum(u, na.rm = TRUE),
    `Share on low-confidence SF` = sum(u[d$sf_low_conf %in% TRUE], na.rm = TRUE) / sum(u, na.rm = TRUE)
  )
})

station_long <- pmap_dfr(scenarios, function(scenario, flag, unit_col) {
  attrs %>%
    filter(.data[[flag]]) %>%
    group_by(station_id, station_name, modes, typology) %>%
    summarise(
      units          = sum(.data[[unit_col]], na.rm = TRUE),
      units_est      = sum(.data[[unit_col]][sf_estimated %in% TRUE], na.rm = TRUE),
      units_low_conf = sum(.data[[unit_col]][sf_low_conf %in% TRUE], na.rm = TRUE),
      .groups        = "drop"
    ) %>%
    mutate(scenario = scenario)
}) %>%
  mutate(share_est = units_est / units, share_low = units_low_conf / units)

units_of <- function(s) scenario_table$Units[scenario_table$Scenario == s]
u_max  <- units_of("Maximum build-out")
u_aor  <- units_of("As-of-right (current zoning)")
u_near <- units_of("Near-term opportunity")

# =============================================================================
# RAIL LINES (GTFS shapes, cached)
# =============================================================================

# CTA L (route_type 1) and Metra (route_type 2) lines from each agency's GTFS,
# one simplified line per route, colored with the agency's own route colors
load_rail_routes <- function(cache = "out/rail_routes.rds") {
  if (file.exists(cache)) return(readRDS(cache))
  
  feeds <- c(CTA   = "https://www.transitchicago.com/downloads/sch_data/google_transit.zip",
             Metra = "https://schedules.metrarail.com/gtfs/schedule.zip")
  
  routes <- imap_dfr(feeds, function(url, agency) {
    tmp <- tempfile(fileext = ".zip")
    download.file(url, tmp, mode = "wb", quiet = TRUE)
    rd  <- function(f) read_csv(unz(tmp, f), col_types = cols(.default = "c"))
    
    rts   <- rd("routes.txt") %>% filter(route_type %in% c("1", "2"))
    trips <- rd("trips.txt") %>%
      filter(route_id %in% rts$route_id, !is.na(shape_id)) %>%
      distinct(route_id, shape_id)
    shp   <- rd("shapes.txt") %>% filter(shape_id %in% trips$shape_id)
    
    shp %>%
      mutate(across(c(shape_pt_lat, shape_pt_lon, shape_pt_sequence), as.numeric)) %>%
      arrange(shape_id, shape_pt_sequence) %>%
      st_as_sf(coords = c("shape_pt_lon", "shape_pt_lat"), crs = 4326) %>%
      group_by(shape_id) %>%
      summarise(do_union = FALSE, .groups = "drop") %>%
      st_cast("LINESTRING") %>%
      inner_join(trips, by = "shape_id") %>%
      inner_join(rts %>% select(route_id, any_of(c("route_short_name", "route_long_name",
                                                   "route_color"))),
                 by = "route_id") %>%
      mutate(agency = agency)
  })
  
  out <- routes %>%
    st_transform(crs_il) %>%
    group_by(agency, route_id) %>%
    summarise(
      route_name = first(coalesce(na_if(route_long_name, ""), route_short_name, route_id)),
      color      = first(if_else(is.na(route_color) | route_color == "",
                                 "#5B6770", paste0("#", route_color))),
      .groups = "drop"
    ) %>%
    st_simplify(dTolerance = 30) %>%          # feet
    st_transform(4326) %>%
    mutate(label = case_when(
      agency == "Metra"                  ~ paste("Metra", route_name),
      str_detect(route_name, "Line$")    ~ route_name,          # CTA names already end in "Line"
      TRUE                               ~ paste(route_name, "Line")
    ))
  
  saveRDS(out, cache)
  out
}

rail_routes <- load_rail_routes()

# =============================================================================
# MAP
# =============================================================================

st_xy <- stations %>%
  st_transform(4326) %>%
  mutate(lng = st_coordinates(.)[, 1], lat = st_coordinates(.)[, 2]) %>%
  st_drop_geometry() %>%
  select(station_id, lng, lat)

station_map <- station_long %>%
  inner_join(st_xy, by = "station_id") %>%
  group_by(scenario) %>%
  mutate(radius = 4 + 24 * sqrt(units / max(units))) %>%
  ungroup() %>%
  mutate(
    dagger = if_else(share_low > low_conf_flag, "\u2020", ""),
    popup  = sprintf(
      "<strong>%s</strong><br>%s, %s<br>%s%s new homes<br><span class='pop-note'>%s rest on estimated existing building SF; %s on low-confidence estimates</span>",
      station_name, modes, typology, comma(round(units)), dagger,
      percent(share_est, 1), percent(share_low, 1)
    )
  )

type_pal <- colorFactor(c("#8A97A3", "#3C7A5A", "#1C5D99"),
                        levels = c("Private", "Other public", "Transit-owned"))

map_sites <- sites %>%
  filter(!in_mm) %>%
  slice_max(net_units, n = n_map_sites, with_ties = FALSE) %>%
  st_simplify(preserveTopology = TRUE, dTolerance = 10) %>%   # feet; keeps the file light
  st_transform(4326) %>%
  mutate(
    sf_note = case_when(
      sf_low_conf %in% TRUE ~ paste0(comma(round(existing_sf)), "\u2020 (", sf_confidence, ")"),
      TRUE                  ~ paste0(comma(round(existing_sf)), " (", sf_confidence, ")")
    ),
    popup = sprintf(
      paste0("<strong>%s</strong><br>%s near %s<br>",
             "%s acres in walkshed, %s<br>",
             "Target FAR %s: %s new homes (%s allowed by current zoning)<br>",
             "Zoning: %s, %s<br>",
             "Existing building SF: %s%s"),
      str_to_title(coalesce(owner_main, "Owner not listed")), site_type, station_name,
      number(acres_ws, 0.1), str_to_lower(lot_tier),
      number(target_far, 0.1), comma(round(net_units)), comma(round(units_aor)),
      coalesce(zone_class, "none"), str_to_lower(zoning_status),
      sf_note, if_else(active_campus %in% TRUE, "<br>Active campus", "")
    )
  )

# Esri Light Gray Canvas: keyless, quiet enough for data overlays, and
# loads from file:// as well as from GitHub Pages
m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
  addTiles(
    urlTemplate = "https://server.arcgisonline.com/ArcGIS/rest/services/Canvas/World_Light_Gray_Base/MapServer/tile/{z}/{y}/{x}",
    attribution = "Tiles &copy; Esri &mdash; Esri, HERE, Garmin, &copy; OpenStreetMap contributors",
    options     = tileOptions(maxZoom = 16)
  ) %>%
  setView(lng = -87.68, lat = 41.85, zoom = 11) %>%
  addPolylines(
    data = rail_routes, group = "Rail lines", label = ~label,
    color = ~color, weight = ~if_else(agency == "CTA", 3.5, 2.5),
    opacity = 0.9, dashArray = ~if_else(agency == "Metra", "6 4", NA_character_)
  )

for (s in scenarios$scenario) {
  m <- m %>%
    addCircleMarkers(
      data = filter(station_map, scenario == s), lng = ~lng, lat = ~lat,
      radius = ~radius, group = s, popup = ~popup,
      stroke = TRUE, weight = 1, color = "#1F2A33",
      fillColor = "#41B6E6", fillOpacity = 0.75
    )
}

m <- m %>%
  addPolygons(
    data = map_sites, group = "Largest sites (parcels)", popup = ~popup,
    fillColor = ~type_pal(site_type), fillOpacity = 0.7,
    color = "#1F2A33", weight = 0.4
  ) %>%
  addLegend("bottomright", pal = type_pal, values = map_sites$site_type,
            title = "Site owner", group = "Largest sites (parcels)", opacity = 0.8) %>%
  addLayersControl(
    baseGroups    = scenarios$scenario,
    overlayGroups = c("Rail lines", "Largest sites (parcels)"),
    options       = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup("Largest sites (parcels)")

# =============================================================================
# TABLES
# =============================================================================

dt_opts <- list(pageLength = 15, dom = "Bfrtip", buttons = list("csv"), scrollX = TRUE)

scenario_dt <- datatable(
  scenario_table, rownames = FALSE, extensions = "Buttons",
  options = list(dom = "t", ordering = FALSE)
) %>%
  formatRound(c("Sites", "Stations", "Walkshed acres", "Units"), digits = 0) %>%
  formatPercentage(c("Share on estimated SF", "Share on low-confidence SF"), digits = 0)

station_wide <- station_long %>%
  select(station_id, station_name, modes, typology, scenario, units) %>%
  pivot_wider(names_from = scenario, values_from = units, values_fill = 0) %>%
  left_join(
    station_long %>%
      filter(scenario == "Maximum build-out") %>%
      transmute(station_id, `Low-confidence share (max)` = share_low),
    by = "station_id"
  ) %>%
  select(-station_id) %>%
  rename(Station = station_name, Mode = modes, Typology = typology) %>%
  arrange(desc(`Near-term opportunity`))

station_dt <- datatable(station_wide, rownames = FALSE, extensions = "Buttons",
                        filter = "top", options = dt_opts) %>%
  formatRound(scenarios$scenario, digits = 0) %>%
  formatPercentage("Low-confidence share (max)", digits = 0)

site_table <- map_sites %>%
  st_drop_geometry() %>%
  transmute(
    `Site ID`       = site_id,
    Owner           = str_to_title(coalesce(owner_main, "")),
    `Site type`     = site_type,
    Station         = station_name,
    `Walkshed acres`= acres_ws,
    `Lot tier`      = lot_tier,
    `New homes`     = net_units,
    `Allowed today` = units_aor,
    Zoning          = zone_class,
    `Zoning status` = zoning_status,
    `Existing SF`   = existing_sf,
    `SF source`     = if_else(sf_low_conf %in% TRUE, paste0(sf_confidence, " \u2020"), sf_confidence),
    `Near-term`     = in_near_term,
    `Active campus` = active_campus
  )

site_dt <- datatable(site_table, rownames = FALSE, extensions = "Buttons",
                     filter = "top", options = dt_opts) %>%
  formatRound(c("Walkshed acres"), digits = 1) %>%
  formatRound(c("New homes", "Allowed today", "Existing SF"), digits = 0)

# Downloads
write_csv(scenario_table, file.path(dash_dir, "data", "scenarios.csv"))
write_csv(station_wide,   file.path(dash_dir, "data", "stations_by_scenario.csv"))
write_csv(attrs %>% select(-any_of("pin10s")), file.path(dash_dir, "data", "all_sites.csv"))

# =============================================================================
# PAGE
# =============================================================================

css <- "
:root { --ink:#1F2A33; --muted:#56636E; --paper:#F5F7F8; --rule:#D3DADF;
        --flag-blue:#41B6E6; --flag-red:#E4002B; }
* { box-sizing: border-box; }
body { margin:0; background:var(--paper); color:var(--ink);
       font:400 17px/1.55 'Barlow', system-ui, sans-serif; }
main { max-width:1120px; margin:0 auto; padding:56px 24px 96px; }
h1 { font:700 clamp(2rem, 4.6vw, 3.3rem)/1.08 'Barlow Semi Condensed', 'Barlow', sans-serif;
     max-width:20ch; margin:0 0 18px; letter-spacing:-0.01em; }
h2 { font:600 1.65rem/1.2 'Barlow Semi Condensed', 'Barlow', sans-serif; margin:64px 0 10px; }
p, li { max-width:70ch; }
.lede { font-size:1.2rem; color:var(--muted); margin:0 0 28px; }
.zbar { display:flex; height:30px; margin:8px 0 10px; }
.zbar span { display:block; height:100%; }
.zbar .aor { background:var(--flag-blue); }
.zbar .blocked { background:var(--flag-red); }
.zkey { display:flex; flex-wrap:wrap; gap:8px 28px; color:var(--muted); font-size:.95rem; }
.zkey b { color:var(--ink); font-weight:600; }
.sw { display:inline-block; width:12px; height:12px; margin-right:6px; vertical-align:-1px; }
.map-wrap { border:1px solid var(--rule); background:#fff; margin-top:16px; }
.pop-note { color:#56636E; font-size:12px; }
.note { color:var(--muted); font-size:.95rem; }
.dagger-note { border-left:3px solid var(--flag-red); padding:4px 0 4px 14px; margin:18px 0; }
a { color:#0E5A86; }
a:focus-visible, button:focus-visible, input:focus-visible { outline:3px solid var(--flag-blue); outline-offset:2px; }
table.dataTable { font-size:.92rem; }
@media (max-width: 640px) { main { padding:32px 16px 64px; } body { font-size:16px; } }
"

aor_share <- u_aor / u_max

page <- tagList(
  tags$head(
    tags$meta(charset = "utf-8"),
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$title("Housing capacity near Chicago rail stations"),
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Barlow:wght@400;500;600&family=Barlow+Semi+Condensed:wght@600;700&display=swap"),
    tags$style(HTML(css))
  ),
  tags$main(
    tags$h1(sprintf("Land within a half mile of Chicago's L and Metra stations could hold about %s new homes on open land alone.",
                    comma(round(u_near, -3)))),
    tags$p(class = "lede",
           sprintf("Building out every underused site and turning single-family lots into fourplexes would add about %s. Current zoning allows roughly %s of that.",
                   comma(round(u_max, -3)), percent(aor_share, 1))),
    
    tags$div(class = "zbar", role = "img",
             `aria-label` = sprintf("%s of build-out capacity is allowed by current zoning", percent(aor_share, 1)),
             tags$span(class = "aor",     style = sprintf("width:%.1f%%", 100 * aor_share)),
             tags$span(class = "blocked", style = sprintf("width:%.1f%%", 100 * (1 - aor_share)))),
    tags$div(class = "zkey",
             tags$span(tags$i(class = "sw", style = "background:var(--flag-blue)"),
                       tags$b(comma(round(u_aor, -3))), " homes allowed under current zoning"),
             tags$span(tags$i(class = "sw", style = "background:var(--flag-red)"),
                       tags$b(comma(round(u_max - u_aor, -3))), " need a rezoning, upzoning, or entitlement change")),
    
    tags$h2("Compare scenarios on the map"),
    tags$p("Pick a scenario in the map's layer panel. Circle size shows new homes near each station. ",
           "Turn on the parcel layer to see the largest individual sites, colored by owner."),
    tags$div(class = "map-wrap", m),
    tags$p(class = "dagger-note",
           "\u2020 marks numbers that lean on low-confidence estimates of existing buildings: ",
           "City footprints with the number of stories imputed from similar buildings. ",
           "Existing building size comes from County Assessor records where available; ",
           "everything else is estimated from 2021 building footprints and labeled with its source."),
    
    tags$h2("Scenarios"),
    tags$p(class = "note",
           "Maximum build-out includes everything. Near-term opportunity is vacant land and surface lots on private ",
           "sites and land the City, County, Land Bank, or a transit agency could offer directly. ",
           "As-of-right counts only what current zoning allows on the same sites."),
    scenario_dt,
    
    tags$h2("Stations"),
    tags$p(class = "note", "New homes by station under each scenario. Filter or sort any column; download as CSV."),
    station_dt,
    
    tags$h2("Largest sites"),
    tags$p(class = "note", sprintf("The %s sites with the most capacity, excluding fourplex conversions of single-family lots.",
                                   comma(n_map_sites))),
    site_dt,
    
    tags$h2("How the numbers work"),
    tags$ul(
      tags$li("Walkshed: a half-mile radius around each CTA L and Metra station in Chicago, with each parcel assigned to its nearest station."),
      tags$li(sprintf("Target density: fourplex density (FAR %s) on lots under %s acres, courtyard-building density (FAR %s) up to %s acres, and the station target above that: FAR %s downtown, %s near CTA stations, %s near Metra stations, 25%% higher within a quarter mile.",
                      fourplex_far, small_lot_ac, courtyard_far, large_lot_ac,
                      target_far_tbl$target_far[target_far_tbl$typology == "Downtown"],
                      target_far_tbl$target_far[target_far_tbl$typology == "CTA neighborhood"],
                      target_far_tbl$target_far[target_far_tbl$typology == "Metra neighborhood"])),
      tags$li(sprintf("New homes: target floor area minus existing floor area, at %s square feet per home. Single-family lots count as one home today and at least %s at build-out.",
                      comma(sqft_per_unit), mm_min_units)),
      tags$li("Underused sites: assessed value per acre well below the station's median, or public land. Parks, forest preserves, cemeteries, rail and street right-of-way, and condominiums are excluded."),
      tags$li("As-of-right: base zoning FAR, limited by permitted building types and minimum lot area per unit in residential districts. Planned developments and industrial districts count as zero, so this undercounts entitled sites."),
      tags$li("Sources: Cook County Assessor (values, owners, building characteristics, commercial valuations), City of Chicago (zoning, building footprints, City-owned land), CTA and Metra GTFS.")
    ),
    tags$p(class = "note",
           "Download the data: ",
           tags$a(href = "data/scenarios.csv", "scenarios"), ", ",
           tags$a(href = "data/stations_by_scenario.csv", "stations by scenario"), ", ",
           tags$a(href = "data/all_sites.csv", "all sites"), ".")
  )
)

save_html(page, file = file.path(dash_dir, "index.html"), libdir = "lib")
message("Wrote ", file.path(dash_dir, "index.html"))
