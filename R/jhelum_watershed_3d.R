#!/usr/bin/env Rscript

# ============================================================================
# Scientifically Grounded 3D Visualization of the Jhelum River Watershed
# ============================================================================

# ---------------------------
# 1) Package installation/loading
# ---------------------------
required_packages <- c(
  "sf", "terra", "dplyr", "stringr", "purrr", "jsonlite", "httr2", "rayshader",
  "rayrender", "rgl", "ggplot2", "lwgeom", "units", "tibble", "tidyr", "osmdata",
  "elevatr", "glue"
)
optional_packages <- c("whitebox")

install_if_missing <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    message("Installing missing packages: ", paste(missing, collapse = ", "))
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
}

# ---------------------------
# 2) Configuration
# ---------------------------
config <- list(
  install_missing_packages = FALSE,
  dirs = list(
    data_raw = "data/raw",
    data_cache = "data/cache",
    data_processed = "data/processed",
    output = "output",
    logs = "logs"
  ),
  bbox_wgs84 = c(xmin = 73.2, ymin = 33.2, xmax = 75.6, ymax = 34.9),
  watershed_buffer_m = 10000,
  crs_projected = 32643, # WGS84 / UTM zone 43N (appropriate for Kashmir extent)
  dem_sources = list(
    cop30 = list(
      enabled = TRUE,
      # OpenTopography Global DEM API. API key is optional but recommended.
      url_template = "https://portal.opentopography.org/API/globaldem?demtype=COP30&south={south}&north={north}&west={west}&east={east}&outputFormat=GTiff",
      api_key_env = "OPENTOPOGRAPHY_API_KEY"
    ),
    elevatr = list(enabled = TRUE, z = 10)
  ),
  hydrology_sources = list(
    hydrorivers = "https://data.hydrosheds.org/file/HydroRIVERS/HydroRIVERS_v10_as_shp.zip",
    hydrobasins = "https://data.hydrosheds.org/file/hydrobasins/standard/hybas_as_lev05_v1c.zip",
    hydrolakes = "https://data.hydrosheds.org/file/hydrolakes/HydroLAKES_polys_v10_shp.zip"
  ),
  required_lakes = c("Wular", "Dal", "Manasbal"),
  target_tributaries = c(
    "Lidder", "Sind", "Pohru", "Kishanganga", "Neelum", "Rambiara", "Vishow",
    "Doodhganga", "Sukhnag", "Ferozpora", "Erin", "Madhumati", "Sandran",
    "Brengi", "Aripal", "Romushi"
  ),
  target_places = c(
    "Srinagar", "Anantnag", "Baramulla", "Sopore", "Pulwama", "Shopian",
    "Pahalgam", "Ganderbal", "Bandipora", "Awantipora"
  ),
  render = list(
    width = 2200,
    height = 1600,
    samples = 500,
    sample_method_preferred = "sobol",
    zscale = 16,
    vertical_exaggeration = 2.0,
    fov = 55,
    theta = 35,
    phi = 35,
    zoom = 0.72,
    windowsize = c(1600, 1200)
  ),
  reference_images_intentionally_unused = c("reference_1.png", "reference_2.png"),
  force_rebuild = FALSE
)

if (isTRUE(config$install_missing_packages)) {
  install_if_missing(required_packages)
  install_if_missing(optional_packages)
}

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop("Required package is missing: ", pkg, call. = FALSE)
  }
}

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(jsonlite)
  library(httr2)
  library(rayshader)
  library(rayrender)
  library(rgl)
  library(ggplot2)
  library(lwgeom)
  library(units)
  library(tibble)
  library(tidyr)
  library(osmdata)
  library(elevatr)
  library(glue)
})

message("Package versions:")
print(vapply(c(required_packages, optional_packages), function(p) {
  if (requireNamespace(p, quietly = TRUE)) as.character(utils::packageVersion(p)) else NA_character_
}, character(1)))

# ---------------------------
# 3) Directory creation
# ---------------------------
for (d in config$dirs) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ---------------------------
# Utility helpers
# ---------------------------
find_existing_field <- function(x, candidates) {
  existing <- names(x)
  idx <- match(tolower(candidates), tolower(existing))
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) return(NULL)
  existing[idx[1]]
}

validate_non_empty_sf <- function(x, label) {
  if (!inherits(x, "sf")) stop(label, " is not an sf object", call. = FALSE)
  if (nrow(x) == 0) stop(label, " is empty", call. = FALSE)
  if (all(sf::st_is_empty(x))) stop(label, " has only empty geometries", call. = FALSE)
  x
}

safe_make_valid <- function(x) {
  if (isTRUE(any(!sf::st_is_valid(x)))) {
    x <- lwgeom::st_make_valid(x)
  }
  x
}

safe_download <- function(url, destfile, min_bytes = 2048L, overwrite = FALSE, retries = 3L) {
  if (file.exists(destfile) && !overwrite) {
    sz <- file.info(destfile)$size
    if (!is.na(sz) && sz >= min_bytes) return(destfile)
  }

  dir.create(dirname(destfile), recursive = TRUE, showWarnings = FALSE)
  tmp <- paste0(destfile, ".tmp")

  for (i in seq_len(retries)) {
    ok <- tryCatch({
      utils::download.file(url = url, destfile = tmp, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) {
      message("Download attempt ", i, " failed for ", url, ": ", conditionMessage(e))
      FALSE
    })

    if (!ok) next
    sz <- file.info(tmp)$size
    if (is.na(sz) || sz < min_bytes) {
      message("Downloaded file too small from ", url, " (", sz, " bytes)")
      next
    }

    file.rename(tmp, destfile)
    return(destfile)
  }

  stop("Failed to download valid file after retries: ", url, call. = FALSE)
}

unzip_checked <- function(zip_path, exdir) {
  if (!file.exists(zip_path)) stop("Archive missing: ", zip_path, call. = FALSE)
  files <- utils::unzip(zip_path, list = TRUE)
  if (nrow(files) == 0) stop("Archive has no entries: ", zip_path, call. = FALSE)
  utils::unzip(zip_path, exdir = exdir)
  shp <- list.files(exdir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)
  if (length(shp) == 0) stop("No shapefile found after extracting: ", zip_path, call. = FALSE)
  shp
}

read_first_layer <- function(shp_files) {
  shp <- shp_files[1]
  sf::st_read(shp, quiet = TRUE)
}

bbox_polygon_sf <- function(bbox_wgs84) {
  sf::st_as_sfc(sf::st_bbox(
    c(xmin = bbox_wgs84["xmin"], ymin = bbox_wgs84["ymin"], xmax = bbox_wgs84["xmax"], ymax = bbox_wgs84["ymax"]),
    crs = 4326
  )) |>
    sf::st_sf(name = "study_bbox", geometry = _)
}

# ---------------------------
# 4) Data acquisition
# ---------------------------
bbox_sf <- bbox_polygon_sf(config$bbox_wgs84)

acquire_dem <- function(cfg, bbox_sf_wgs84) {
  dem_dir <- file.path(cfg$dirs$data_raw, "dem")
  dir.create(dem_dir, recursive = TRUE, showWarnings = FALSE)

  # Primary: OpenTopography COP30
  if (isTRUE(cfg$dem_sources$cop30$enabled)) {
    cop30_path <- file.path(dem_dir, "cop30_jhelum.tif")
    if (!file.exists(cop30_path) || isTRUE(cfg$force_rebuild)) {
      bb <- sf::st_bbox(bbox_sf_wgs84)
      base_url <- glue::glue(
        cfg$dem_sources$cop30$url_template,
        south = bb$ymin,
        north = bb$ymax,
        west = bb$xmin,
        east = bb$xmax
      )
      key <- Sys.getenv(cfg$dem_sources$cop30$api_key_env, unset = "")
      dem_url <- if (nzchar(key)) paste0(base_url, "&API_Key=", utils::URLencode(key, reserved = TRUE)) else base_url

      message("Attempting DEM download from OpenTopography COP30")
      tryCatch({
        safe_download(dem_url, cop30_path, min_bytes = 10000L, overwrite = TRUE)
      }, error = function(e) {
        message("COP30 download failed: ", conditionMessage(e))
      })
    }

    if (file.exists(cop30_path)) {
      r <- tryCatch(terra::rast(cop30_path), error = function(e) NULL)
      if (!is.null(r) && terra::ncell(r) > 0) {
        return(r)
      }
      message("COP30 DEM exists but could not be read; trying fallback.")
    }
  }

  # Fallback: elevatr
  if (isTRUE(cfg$dem_sources$elevatr$enabled)) {
    elev_path <- file.path(dem_dir, "elevatr_jhelum.tif")
    if (!file.exists(elev_path) || isTRUE(cfg$force_rebuild)) {
      message("Attempting DEM fallback with elevatr")
      dem_raster <- elevatr::get_elev_raster(
        locations = bbox_sf_wgs84,
        z = cfg$dem_sources$elevatr$z,
        clip = "bbox"
      )
      dem_terra <- terra::rast(dem_raster)
      terra::writeRaster(dem_terra, elev_path, overwrite = TRUE)
    }
    r <- terra::rast(elev_path)
    if (terra::ncell(r) > 0) return(r)
  }

  stop("Failed to acquire DEM from all configured sources.", call. = FALSE)
}

download_hydrology_archives <- function(cfg) {
  hydro_dir <- file.path(cfg$dirs$data_raw, "hydrology")
  dir.create(hydro_dir, recursive = TRUE, showWarnings = FALSE)

  files <- list(
    hydrorivers_zip = file.path(hydro_dir, "hydrorivers.zip"),
    hydrobasins_zip = file.path(hydro_dir, "hydrobasins.zip"),
    hydrolakes_zip = file.path(hydro_dir, "hydrolakes.zip")
  )

  safe_download(cfg$hydrology_sources$hydrorivers, files$hydrorivers_zip, min_bytes = 100000L, overwrite = cfg$force_rebuild)
  safe_download(cfg$hydrology_sources$hydrobasins, files$hydrobasins_zip, min_bytes = 100000L, overwrite = cfg$force_rebuild)
  safe_download(cfg$hydrology_sources$hydrolakes, files$hydrolakes_zip, min_bytes = 100000L, overwrite = cfg$force_rebuild)

  files
}

# ---------------------------
# 5) DEM processing
# ---------------------------
dem_raw <- acquire_dem(config, bbox_sf)
if (isTRUE(all(is.na(terra::values(dem_raw))))) {
  stop("DEM contains only NA values.", call. = FALSE)
}

dem_wgs84 <- terra::crop(dem_raw, terra::ext(config$bbox_wgs84["xmin"], config$bbox_wgs84["xmax"], config$bbox_wgs84["ymin"], config$bbox_wgs84["ymax"]))
if (terra::ncell(dem_wgs84) == 0) stop("DEM crop produced empty raster", call. = FALSE)

dem_wgs84 <- terra::ifel(is.na(dem_wgs84), terra::focal(dem_wgs84, w = 3, fun = "mean", na.policy = "omit", fillvalue = NA), dem_wgs84)

# ---------------------------
# 6) Watershed acquisition
# ---------------------------
archives <- download_hydrology_archives(config)

hydro_extract_dir <- file.path(config$dirs$data_cache, "hydrology_extract")
dir.create(hydro_extract_dir, recursive = TRUE, showWarnings = FALSE)

hydrorivers <- read_first_layer(unzip_checked(archives$hydrorivers_zip, file.path(hydro_extract_dir, "hydrorivers")))
hydrobasins <- read_first_layer(unzip_checked(archives$hydrobasins_zip, file.path(hydro_extract_dir, "hydrobasins")))
hydrolakes <- read_first_layer(unzip_checked(archives$hydrolakes_zip, file.path(hydro_extract_dir, "hydrolakes")))

hydrorivers <- validate_non_empty_sf(hydrorivers, "HydroRIVERS") |> safe_make_valid()
hydrobasins <- validate_non_empty_sf(hydrobasins, "HydroBASINS") |> safe_make_valid()
hydrolakes <- validate_non_empty_sf(hydrolakes, "HydroLAKES") |> safe_make_valid()

hydrorivers <- sf::st_transform(hydrorivers, 4326)
hydrobasins <- sf::st_transform(hydrobasins, 4326)
hydrolakes <- sf::st_transform(hydrolakes, 4326)

# ---------------------------
# 7) River/tributary processing
# ---------------------------
river_name_field <- find_existing_field(hydrorivers, c("RIV_NAME", "NAME", "name", "river_name"))
if (is.null(river_name_field)) {
  stop("Could not find river name field in HydroRIVERS.", call. = FALSE)
}

hydrorivers <- hydrorivers |>
  mutate(.river_name = stringr::str_squish(as.character(.data[[river_name_field]]))) |>
  filter(!is.na(.river_name), .river_name != "")

study_rivers <- sf::st_intersection(hydrorivers, bbox_sf)
study_rivers <- validate_non_empty_sf(study_rivers, "Rivers in study bbox")

jhelum_candidates <- study_rivers |>
  filter(str_detect(str_to_lower(.river_name), "jhelum|vyeth"))

if (nrow(jhelum_candidates) == 0) {
  # OSM fallback for primary river identity if HydroRIVERS naming is missing
  opq_bbox <- as.numeric(sf::st_bbox(bbox_sf))
  osm_rivers <- osmdata::opq(bbox = opq_bbox) |>
    osmdata::add_osm_feature(key = "waterway", value = c("river", "stream")) |>
    osmdata::osmdata_sf()
  osm_lines <- osm_rivers$osm_lines
  if (is.null(osm_lines) || nrow(osm_lines) == 0) {
    stop("Unable to identify Jhelum geometry in HydroRIVERS and OSM fallback returned no waterways.", call. = FALSE)
  }
  osm_lines <- sf::st_transform(osm_lines, 4326)
  nfield <- find_existing_field(osm_lines, c("name", "NAME"))
  if (is.null(nfield)) stop("OSM waterways have no name field.", call. = FALSE)
  jhelum_candidates <- osm_lines |>
    mutate(.river_name = as.character(.data[[nfield]])) |>
    filter(str_detect(str_to_lower(.river_name), "jhelum|vyeth"))
  if (nrow(jhelum_candidates) == 0) stop("Could not resolve Jhelum geometry from real data sources.", call. = FALSE)
  study_rivers <- bind_rows(study_rivers, jhelum_candidates)
}

jhelum_main <- jhelum_candidates |>
  mutate(.len = as.numeric(sf::st_length(geometry))) |>
  arrange(desc(.len)) |>
  slice(1)

# ---------------------------
# 8) Lake acquisition
# ---------------------------
lake_name_field <- find_existing_field(hydrolakes, c("Lake_name", "LAKE_NAME", "NAME", "name"))
if (is.null(lake_name_field)) stop("Could not find lake name field in HydroLAKES.", call. = FALSE)

lakes_in_area <- hydrolakes |>
  mutate(.lake_name = str_squish(as.character(.data[[lake_name_field]]))) |>
  filter(!is.na(.lake_name), .lake_name != "") |>
  st_intersection(bbox_sf)

required_lakes <- config$required_lakes
required_lake_matches <- lakes_in_area |>
  filter(str_detect(str_to_lower(.lake_name), str_c(str_to_lower(required_lakes), collapse = "|")))

if (length(setdiff(str_to_lower(required_lakes), unique(str_to_lower(required_lake_matches$.lake_name)))) > 0) {
  message("HydroLAKES missing one or more required lakes, attempting OSM fallback for water polygons.")
  osm_water <- osmdata::opq(bbox = as.numeric(sf::st_bbox(bbox_sf))) |>
    osmdata::add_osm_feature(key = "natural", value = "water") |>
    osmdata::osmdata_sf()
  osm_polys <- osm_water$osm_polygons
  if (!is.null(osm_polys) && nrow(osm_polys) > 0) {
    nfield <- find_existing_field(osm_polys, c("name", "NAME"))
    if (!is.null(nfield)) {
      osm_required <- sf::st_transform(osm_polys, 4326) |>
        mutate(.lake_name = str_squish(as.character(.data[[nfield]]))) |>
        filter(!is.na(.lake_name), .lake_name != "") |>
        filter(str_detect(str_to_lower(.lake_name), str_c(str_to_lower(required_lakes), collapse = "|")))

      lakes_in_area <- bind_rows(lakes_in_area, osm_required)
    }
  }
}

present_required <- unique(str_to_lower(lakes_in_area$.lake_name))
missing_required <- required_lakes[!(str_to_lower(required_lakes) %in% present_required)]
if (length(missing_required) > 0) {
  stop("Required real lake geometries are missing even after fallback: ", paste(missing_required, collapse = ", "), call. = FALSE)
}

lakes_final <- lakes_in_area |>
  group_by(.lake_name) |>
  summarize(geometry = sf::st_union(geometry), .groups = "drop")

# ---------------------------
# 9) Places
# ---------------------------
fetch_place_osm <- function(place_name, bbox_vals) {
  q <- tryCatch({
    osmdata::opq(bbox = bbox_vals) |>
      osmdata::add_osm_feature(key = "name", value = place_name) |>
      osmdata::add_osm_feature(key = "place", value = c("city", "town", "village")) |>
      osmdata::osmdata_sf()
  }, error = function(e) NULL)

  if (is.null(q)) return(NULL)
  pts <- q$osm_points
  if (is.null(pts) || nrow(pts) == 0) return(NULL)
  nfield <- find_existing_field(pts, c("name", "NAME"))
  if (is.null(nfield)) return(NULL)
  pts |>
    mutate(name = as.character(.data[[nfield]])) |>
    filter(tolower(name) == tolower(place_name)) |>
    select(name, geometry)
}

bbox_vals <- as.numeric(sf::st_bbox(bbox_sf))
places_list <- purrr::map(config$target_places, fetch_place_osm, bbox_vals = bbox_vals)
places <- bind_rows(places_list)

if (nrow(places) == 0) {
  warning("No target places found from OSM within bounding box.")
  places <- sf::st_sf(name = character(), geometry = sf::st_sfc(crs = 4326))
}

# ---------------------------
# 10) CRS harmonization
# ---------------------------
watershed_method <- "HydroBASINS level-05 polygon intersecting principal Jhelum and study extent"

basin_candidates <- hydrobasins |>
  st_intersection(bbox_sf)

if (nrow(basin_candidates) == 0) {
  watershed_method <- "DEM-derived watershed fallback from HydroRIVERS outlet via WhiteboxTools"
  if (!requireNamespace("whitebox", quietly = TRUE)) {
    stop("HydroBASINS unavailable in extent and whitebox package not installed for defensible DEM-derived watershed fallback.", call. = FALSE)
  }

  tmp_dir <- file.path(config$dirs$data_cache, "whitebox")
  dir.create(tmp_dir, recursive = TRUE, showWarnings = FALSE)

  dem_tmp <- file.path(tmp_dir, "dem_wgs84.tif")
  terra::writeRaster(dem_wgs84, dem_tmp, overwrite = TRUE)

  jhelum_ln <- sf::st_cast(sf::st_geometry(jhelum_main), "MULTILINESTRING")
  line_coords <- sf::st_coordinates(jhelum_ln)
  outlet <- sf::st_sfc(sf::st_point(line_coords[nrow(line_coords), c("X", "Y")]), crs = 4326) |> sf::st_sf(id = 1, geometry = _)
  outlet_path <- file.path(tmp_dir, "outlet.gpkg")
  sf::st_write(outlet, outlet_path, delete_dsn = TRUE, quiet = TRUE)

  flowdir <- file.path(tmp_dir, "flowdir.tif")
  ws_raster <- file.path(tmp_dir, "watershed.tif")
  whitebox::wbt_d8_pointer(dem = dem_tmp, output = flowdir)
  whitebox::wbt_watershed(d8_pntr = flowdir, pour_pts = outlet_path, output = ws_raster)

  ws <- terra::rast(ws_raster)
  ws_poly <- terra::as.polygons(ws, dissolve = TRUE)
  watershed <- sf::st_as_sf(ws_poly) |>
    sf::st_transform(4326) |>
    filter(!is.na(lyr.1), lyr.1 > 0)
} else {
  basin_candidates <- safe_make_valid(basin_candidates)
  inter <- suppressWarnings(sf::st_intersection(basin_candidates, sf::st_buffer(jhelum_main, dist = 0.05)))
  if (nrow(inter) == 0) {
    stop("Could not identify watershed candidate intersecting Jhelum from HydroBASINS.", call. = FALSE)
  }

  area_field <- find_existing_field(inter, c("SUB_AREA", "UP_AREA", "AREA_SQKM"))
  if (is.null(area_field)) {
    inter <- inter |> mutate(.a = as.numeric(sf::st_area(geometry)))
    area_field <- ".a"
  }

  watershed <- inter |>
    arrange(desc(.data[[area_field]])) |>
    slice(1) |>
    select(geometry)
}

watershed <- validate_non_empty_sf(watershed, "Watershed") |> safe_make_valid() |> sf::st_union() |> sf::st_as_sf()

# harmonize layers to projected CRS
watershed_proj <- sf::st_transform(watershed, config$crs_projected)
jhelum_proj <- sf::st_transform(jhelum_main, config$crs_projected)
rivers_proj <- sf::st_transform(study_rivers, config$crs_projected)
lakes_proj <- sf::st_transform(lakes_final, config$crs_projected)
places_proj <- sf::st_transform(places, config$crs_projected)

dem_proj <- terra::project(dem_wgs84, paste0("EPSG:", config$crs_projected), method = "bilinear")
ws_vect <- terra::vect(watershed_proj)
ws_buffer <- terra::buffer(ws_vect, width = config$watershed_buffer_m)

dem_crop <- terra::crop(dem_proj, ws_buffer)
dem_mask <- terra::mask(dem_crop, ws_buffer)

if (terra::ncell(dem_mask) == 0) stop("Projected/cropped DEM is empty", call. = FALSE)
if (isTRUE(all(is.na(terra::values(dem_mask))))) stop("Projected/cropped DEM is all NA", call. = FALSE)

# tributaries after watershed selection
river_name_field_proj <- find_existing_field(rivers_proj, c(".river_name", "RIV_NAME", "NAME", "name", "river_name"))
if (is.null(river_name_field_proj)) {
  rivers_proj$.river_name <- ""
  river_name_field_proj <- ".river_name"
}

tributaries_proj <- rivers_proj |>
  filter(!str_detect(str_to_lower(as.character(.data[[river_name_field_proj]])), "jhelum|vyeth")) |>
  mutate(.river_name = str_squish(as.character(.data[[river_name_field_proj]]))) |>
  st_intersection(watershed_proj)

if (nrow(tributaries_proj) > 0) {
  tributaries_proj <- tributaries_proj |>
    filter(str_detect(str_to_lower(.river_name), str_c(str_to_lower(config$target_tributaries), collapse = "|")) |
             as.numeric(st_length(geometry)) > 2500)
}

jhelum_proj <- st_intersection(jhelum_proj, watershed_proj)
lakes_proj <- st_intersection(lakes_proj, watershed_proj)
places_proj <- suppressWarnings(st_intersection(places_proj, watershed_proj))

# ---------------------------
# 11) Hydrological validation
# ---------------------------
validation <- tibble::tibble(
  check = c(
    "watershed_non_empty", "jhelum_non_empty", "tributaries_non_empty",
    "lakes_non_empty", "required_lakes_present", "jhelum_intersects_watershed"
  ),
  passed = c(
    nrow(watershed_proj) > 0,
    nrow(jhelum_proj) > 0,
    nrow(tributaries_proj) > 0,
    nrow(lakes_proj) > 0,
    all(str_to_lower(config$required_lakes) %in% str_to_lower(lakes_proj$.lake_name)),
    nrow(sf::st_intersection(jhelum_proj, watershed_proj)) > 0
  )
)

print(validation)
if (any(!validation$passed)) {
  stop("Hydrological validation failed. See validation table above.", call. = FALSE)
}

# ---------------------------
# 12) Terrain construction
# ---------------------------
dem_agg <- terra::aggregate(dem_mask, fact = 2, fun = "mean", na.rm = TRUE)
dem_agg <- terra::focal(dem_agg, w = 3, fun = "mean", na.policy = "omit")

dem_path <- file.path(config$dirs$data_processed, "jhelum_dem_projected.tif")
terra::writeRaster(dem_agg, dem_path, overwrite = TRUE)

elmat <- rayshader::raster_to_matrix(dem_agg)
if (all(is.na(elmat))) stop("Elevation matrix is all NA", call. = FALSE)

base_texture <- rayshader::height_shade(elmat, texture = "imhof4")
ray_shadow <- rayshader::ray_shade(elmat, zscale = config$render$zscale, lambert = TRUE)
amb_shadow <- rayshader::ambient_shade(elmat, zscale = config$render$zscale)
map_tex <- base_texture |>
  rayshader::add_shadow(ray_shadow, 0.5) |>
  rayshader::add_shadow(amb_shadow, 0.4)

# ---------------------------
# 13) Water rendering (geometry-driven corridors)
# ---------------------------
ext <- terra::ext(dem_agg)
xy_to_mat <- function(x, y, ext_obj, nrows, ncols) {
  col <- ((x - ext_obj[1]) / (ext_obj[2] - ext_obj[1])) * (ncols - 1) + 1
  row <- nrows - (((y - ext_obj[3]) / (ext_obj[4] - ext_obj[3])) * (nrows - 1))
  cbind(col, row)
}

line_to_overlay <- function(lines_sf, color, width, alpha = 1) {
  if (nrow(lines_sf) == 0) return(NULL)
  geoms <- sf::st_geometry(lines_sf)
  coords_list <- lapply(geoms, function(g) {
    cr <- sf::st_coordinates(g)
    xy_to_mat(cr[, "X"], cr[, "Y"], ext, nrow(elmat), ncol(elmat))
  })
  rayshader::generate_line_overlay(
    geometry = coords_list,
    extent = ext,
    heightmap = elmat,
    linewidth = width,
    color = color,
    antialias = TRUE,
    alpha = alpha
  )
}

lake_overlay <- tryCatch({
  rayshader::generate_polygon_overlay(
    geometry = lakes_proj,
    extent = ext,
    heightmap = elmat,
    color = "#1D4E89",
    linecolor = "#163B66",
    palette = NULL,
    data_column_fill = NULL,
    alpha = 0.82
  )
}, error = function(e) {
  warning("Lake polygon overlay failed: ", conditionMessage(e))
  NULL
})

jhelum_overlay <- line_to_overlay(jhelum_proj, color = "#0B3C8A", width = 10, alpha = 1)
trib_overlay <- line_to_overlay(tributaries_proj, color = "#3D7CC9", width = 3, alpha = 0.95)

if (!is.null(jhelum_overlay)) map_tex <- rayshader::add_overlay(map_tex, jhelum_overlay, alphalayer = 1)
if (!is.null(trib_overlay)) map_tex <- rayshader::add_overlay(map_tex, trib_overlay, alphalayer = 1)
if (!is.null(lake_overlay)) map_tex <- rayshader::add_overlay(map_tex, lake_overlay, alphalayer = 1)

# ---------------------------
# 14) Labels (collision-aware/sparse hierarchy)
# ---------------------------
if (nrow(places_proj) > 0) {
  places_proj <- places_proj |>
    mutate(priority = case_when(
      name %in% c("Srinagar") ~ 1L,
      name %in% c("Anantnag", "Baramulla") ~ 2L,
      TRUE ~ 3L
    )) |>
    arrange(priority)

  keep <- rep(TRUE, nrow(places_proj))
  for (i in seq_len(nrow(places_proj))) {
    if (!keep[i]) next
    d <- as.numeric(sf::st_distance(places_proj[i, ], places_proj, by_element = FALSE))
    overlap_idx <- which(d < 4000 & seq_along(d) > i)
    keep[overlap_idx] <- FALSE
  }
  places_proj <- places_proj[keep, ]
}

# ---------------------------
# 15) Lighting/materials
# ---------------------------
zscale_use <- config$render$zscale / config$render$vertical_exaggeration
rgl::open3d(useNULL = TRUE)

# ---------------------------
# 16) Cameras
# ---------------------------
rayshader::plot_3d(
  elmat,
  zscale = zscale_use,
  solid = TRUE,
  shadow = TRUE,
  windowsize = config$render$windowsize,
  phi = config$render$phi,
  theta = config$render$theta,
  zoom = config$render$zoom,
  fov = config$render$fov,
  soliddepth = -max(elmat, na.rm = TRUE) / 3,
  solidcolor = "#d9c7aa",
  background = "white"
)
rayshader::render_camera(theta = config$render$theta, phi = config$render$phi, zoom = config$render$zoom, fov = config$render$fov)

# ---------------------------
# 17) 500+ sample rendering invariant
# ---------------------------
if (!exists("render_highquality", where = asNamespace("rayshader"), mode = "function")) {
  stop("rayshader::render_highquality is not available in this installed rayshader version.", call. = FALSE)
}

if (!is.numeric(config$render$samples) || config$render$samples < 500) {
  stop("Configured sample count must be >= 500.", call. = FALSE)
}

sample_method <- config$render$sample_method_preferred
if (!identical(sample_method, "sobol")) {
  stop("Configured sample_method must default to 'sobol' for this workflow.", call. = FALSE)
}

validate_sample_method <- function(method) {
  # Runtime introspection + microscopic render test to ensure method is accepted.
  tryCatch({
    rayrender::render_scene(
      scene = rayrender::generate_cornell(),
      width = 4,
      height = 4,
      samples = 1,
      sample_method = method,
      clamp_value = 10,
      min_variance = 1e6,
      parallel = FALSE,
      verbose = FALSE
    )
    TRUE
  }, error = function(e) {
    message("Sample method validation failed for '", method, "': ", conditionMessage(e))
    FALSE
  })
}

if (!validate_sample_method(sample_method)) {
  stop("Selected sample_method is not accepted by installed rayrender/rayshader versions. Aborting before final render.", call. = FALSE)
}

hq_formals <- names(formals(rayshader::render_highquality))
hq_args <- list(
  filename = file.path(config$dirs$output, "jhelum_watershed_final.png"),
  samples = config$render$samples,
  light = TRUE,
  clear = TRUE,
  interactive = FALSE,
  preview = FALSE,
  width = config$render$width,
  height = config$render$height
)

if ("sample_method" %in% hq_formals) {
  hq_args$sample_method <- sample_method
} else if (!("..." %in% hq_formals)) {
  stop("Installed rayshader::render_highquality does not support sample_method and does not expose ... for forwarding. Cannot guarantee compatibility.", call. = FALSE)
} else {
  # Forward through ... when supported.
  hq_args$sample_method <- sample_method
}

render_result <- tryCatch({
  do.call(rayshader::render_highquality, hq_args)
  list(ok = TRUE, warning = NULL)
}, warning = function(w) {
  message("render_highquality warning: ", conditionMessage(w))
  list(ok = TRUE, warning = conditionMessage(w))
}, error = function(e) {
  stop("render_highquality failed: ", conditionMessage(e), call. = FALSE)
})

if (!isTRUE(render_result$ok)) {
  stop("High-quality rendering failed unexpectedly.", call. = FALSE)
}
if (config$render$samples < 500) {
  stop("Internal safety check failed: sample count dropped below 500.", call. = FALSE)
}

# ---------------------------
# 18) Export products (vectors + 2D maps + optional extra 3D views)
# ---------------------------
export_gpkg <- file.path(config$dirs$data_processed, "jhelum_watershed_layers.gpkg")
if (file.exists(export_gpkg)) file.remove(export_gpkg)

sf::st_write(watershed_proj, export_gpkg, layer = "watershed", quiet = TRUE)
sf::st_write(jhelum_proj, export_gpkg, layer = "jhelum_main", quiet = TRUE, append = TRUE)
sf::st_write(tributaries_proj, export_gpkg, layer = "tributaries", quiet = TRUE, append = TRUE)
sf::st_write(lakes_proj, export_gpkg, layer = "lakes", quiet = TRUE, append = TRUE)
sf::st_write(places_proj, export_gpkg, layer = "places", quiet = TRUE, append = TRUE)

sf::st_write(watershed_proj, file.path(config$dirs$data_processed, "watershed.geojson"), delete_dsn = TRUE, quiet = TRUE)
sf::st_write(jhelum_proj, file.path(config$dirs$data_processed, "jhelum_main.geojson"), delete_dsn = TRUE, quiet = TRUE)
sf::st_write(tributaries_proj, file.path(config$dirs$data_processed, "tributaries.geojson"), delete_dsn = TRUE, quiet = TRUE)
sf::st_write(lakes_proj, file.path(config$dirs$data_processed, "lakes.geojson"), delete_dsn = TRUE, quiet = TRUE)
sf::st_write(places_proj, file.path(config$dirs$data_processed, "places.geojson"), delete_dsn = TRUE, quiet = TRUE)

# 2D high-resolution scientific map
watershed_2d <- ggplot() +
  geom_sf(data = sf::st_as_sf(terra::as.contour(dem_agg, levels = pretty(terra::global(dem_agg, "range", na.rm = TRUE)[1, ], n = 12))),
          color = "grey70", linewidth = 0.2, alpha = 0.5) +
  geom_sf(data = watershed_proj, fill = NA, color = "black", linewidth = 0.6) +
  geom_sf(data = lakes_proj, fill = "#1D4E89", color = "#163B66", linewidth = 0.3, alpha = 0.8) +
  geom_sf(data = tributaries_proj, color = "#3D7CC9", linewidth = 0.3, alpha = 0.9) +
  geom_sf(data = jhelum_proj, color = "#0B3C8A", linewidth = 1.1) +
  geom_sf(data = places_proj, color = "black", size = 1) +
  geom_sf_text(data = places_proj, aes(label = name), size = 3, nudge_y = 2000, check_overlap = TRUE) +
  labs(
    title = "Jhelum Watershed (Jammu & Kashmir) - Data-grounded 2D reference map",
    subtitle = "HydroRIVERS/HydroBASINS/HydroLAKES + OSM places; projected EPSG:32643",
    caption = "Labels are anchored to real coordinates. For static PNGs, label size cannot resize interactively with zoom."
  ) +
  theme_minimal(base_size = 12)

ggplot2::ggsave(
  filename = file.path(config$dirs$output, "jhelum_watershed_map_2d.png"),
  plot = watershed_2d,
  width = 14,
  height = 10,
  dpi = 300
)

# Optional additional views
rayshader::render_camera(theta = 0, phi = 85, zoom = 0.55, fov = 0)
rayshader::render_snapshot(filename = file.path(config$dirs$output, "jhelum_watershed_birdseye.png"), clear = FALSE)

if (nrow(lakes_proj) > 0 && any(str_detect(str_to_lower(lakes_proj$.lake_name), "dal"))) {
  dal_lake <- lakes_proj |> filter(str_detect(str_to_lower(.lake_name), "dal")) |> slice(1)
  dal_centroid <- sf::st_coordinates(sf::st_centroid(dal_lake))
  rayshader::render_camera(theta = 25, phi = 40, zoom = 0.9, fov = 45)
  rayshader::render_snapshot(filename = file.path(config$dirs$output, "srinagar_dal_close_view.png"), clear = FALSE)
}

# ---------------------------
# 19) Reporting
# ---------------------------
report <- list(
  timestamp_utc = as.character(Sys.time()),
  watershed_method = watershed_method,
  projected_crs_epsg = config$crs_projected,
  dem_output = dem_path,
  outputs = list.files(config$dirs$output, full.names = TRUE),
  processed_vectors = list.files(config$dirs$data_processed, full.names = TRUE),
  render = list(
    samples = config$render$samples,
    sample_method = sample_method,
    width = config$render$width,
    height = config$render$height,
    zscale = config$render$zscale,
    vertical_exaggeration = config$render$vertical_exaggeration
  ),
  note = "This workflow intentionally does not read or use reference PNG images."
)

jsonlite::write_json(report, file.path(config$dirs$logs, "run_report.json"), pretty = TRUE, auto_unbox = TRUE)

message("Run complete.")
message("Primary render: ", file.path(config$dirs$output, "jhelum_watershed_final.png"))
message("2D map: ", file.path(config$dirs$output, "jhelum_watershed_map_2d.png"))
message("Report: ", file.path(config$dirs$logs, "run_report.json"))

