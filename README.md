# jhelum-watershed-3d-model

Scientifically grounded 3D visualization workflow for the **Jhelum River watershed** in Jammu & Kashmir using R (`terra`, `sf`, `rayshader`, `rayrender`, `rgl`).

## What this project does

The executable pipeline at:

- `/home/runner/work/jhelum-watershed-3d-model/jhelum-watershed-3d-model/R/jhelum_watershed_3d.R`

builds a reproducible, data-driven terrain + hydrography model and exports:

- `output/jhelum_watershed_final.png` (main oblique 3D render)
- `output/jhelum_watershed_map_2d.png` (high-resolution 2D scientific map)
- `output/jhelum_watershed_birdseye.png` (optional bird’s-eye snapshot)
- `output/srinagar_dal_close_view.png` (optional close view where data permit)
- `data/processed/jhelum_dem_projected.tif`
- `data/processed/jhelum_watershed_layers.gpkg`
- `data/processed/*.geojson` layers for watershed, main Jhelum, tributaries, lakes, places
- `logs/run_report.json`

## Scientific constraints enforced

- Uses **real spatial datasets** only (no invented coordinates, no decorative random lines).
- Watershed is sourced from real basin data where available; if not, script attempts DEM-based hydrologic fallback and fails clearly if not defensible.
- Required lakes (**Wular, Dal, Manasbal**) are required from real polygons; fallback is attempted from OSM water polygons; script stops if still missing.
- Hydrological validation checks run before rendering.
- All layers are harmonized to **EPSG:32643 (WGS84 / UTM Zone 43N)** for Kashmir-scale analysis.

## Data sources and attribution

Configured at script top level and downloaded with caching + validation:

- **HydroRIVERS** (HydroSHEDS) — river network
- **HydroBASINS** (HydroSHEDS) — watershed boundary candidate
- **HydroLAKES** — lakes polygons
- **OpenTopography COP30** (preferred DEM endpoint) with fallback to `elevatr`
- **OpenStreetMap / Overpass** — fallback hydro features and places

Please review each provider’s license/terms before redistribution:

- HydroSHEDS / HydroRIVERS / HydroBASINS: https://www.hydrosheds.org/
- HydroLAKES: https://www.hydrosheds.org/products/hydrolakes
- OpenStreetMap (ODbL): https://www.openstreetmap.org/copyright
- OpenTopography terms: https://opentopography.org/

## Installation

Use R >= 4.2 recommended.

```r
install.packages(c(
  "sf", "terra", "dplyr", "stringr", "purrr", "jsonlite", "httr2",
  "rayshader", "rayrender", "rgl", "ggplot2", "lwgeom", "units",
  "tibble", "tidyr", "osmdata", "elevatr", "glue"
))
# optional hydrologic fallback support
install.packages("whitebox")
```

System dependencies may be required for `sf`, `terra`, and OpenGL (`rgl`) depending on OS.

## Execution

From repository root:

```bash
Rscript R/jhelum_watershed_3d.R
```

Optional environment variable for COP30 API key:

```bash
export OPENTOPOGRAPHY_API_KEY="..."
```

## Runtime and storage expectations

Approximate (depends on connection/hardware/package versions):

- Initial run: 15–90+ minutes (download + processing + high-quality rendering)
- Cached rerun: usually much faster
- Storage: several hundred MB to multiple GB depending DEM/source coverage
- `samples = 500` high-quality render can be compute-intensive

## Critical rendering invariant

The script enforces:

- `samples >= 500` (explicitly set to 500 by default)
- `sample_method <- "sobol"` default
- runtime compatibility checks for `render_highquality` and sampling method
- hard stop before rendering if compatibility cannot be guaranteed

No silent reduction below 500 samples is allowed.

## Lightweight validation script

A fast no-render check is included:

```bash
Rscript scripts/validate_jhelum_watershed_3d.R
```

It verifies presence of critical invariants and parses the script for syntax.

## Known limitations

- Public endpoints may change schemas/availability; script includes fallback/error pathways but cannot guarantee third-party uptime.
- Name attributes vary by provider; some tributaries/places may be absent if not present in source datasets for the configured extent.
- Static PNG labels cannot truly rescale interactively with zoom in rayshader/rayrender outputs; a high-resolution 2D map is provided for readability.
- Water appearance emphasizes scientific readability while still being geometry-driven by real hydro data.

## Important note on reference PNGs

Two user reference PNG images are **intentionally not read and not used** by the workflow. The model is generated only from real geospatial data sources.
