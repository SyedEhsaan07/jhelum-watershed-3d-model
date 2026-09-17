#!/usr/bin/env Rscript

script_path <- "/home/runner/work/jhelum-watershed-3d-model/jhelum-watershed-3d-model/R/jhelum_watershed_3d.R"

if (!file.exists(script_path)) {
  stop("Missing script: ", script_path, call. = FALSE)
}

script_text <- paste(readLines(script_path, warn = FALSE), collapse = "\n")

required_patterns <- c(
  "render_highquality",
  "samples = config\\$render\\$samples",
  "sample_method <- config\\$render\\$sample_method_preferred",
  "if \(!is.numeric\(config\\$render\\$samples\) \|\| config\\$render\\$samples < 500\)",
  "reference_images_intentionally_unused",
  "HydroRIVERS",
  "HydroBASINS",
  "HydroLAKES"
)

missing <- required_patterns[!vapply(required_patterns, function(p) grepl(p, script_text, perl = TRUE), logical(1))]
if (length(missing) > 0) {
  stop("Critical invariant patterns missing: ", paste(missing, collapse = ", "), call. = FALSE)
}

# Syntax parse check (lightweight, no data download/render)
parse(file = script_path)

cat("Validation passed: script exists, invariants present, syntax parse succeeded.\n")
