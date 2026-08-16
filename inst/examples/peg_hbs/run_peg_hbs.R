# =============================================================================
# run_peg_hbs.R -- harmonize the PEG + HBS example ON the cohortHarmonize package.
# Proves the generic package reproduces the PEG/HBS pipeline. Run line by line.
# =============================================================================

## --- 0. EDIT these paths --------------------------------------------------
PKG_DIR <- "/Users/lukemonnich/Downloads/BWH26/p3_HARMONIZATION/cohortHarmonize"
EX_DIR  <- file.path(PKG_DIR, "inst", "examples", "peg_hbs")
PEG_DIR <- "/Users/lukemonnich/Downloads/BWH26/p2_DATASCHEMA/PEG Materials/data"        # folder of PEG csvs
HBS_CSV <- "/Users/lukemonnich/Downloads/BWH26/p3_HARMONIZATION/output/hbs_fixture_SYNTHETIC.csv"  # or a real export

# Quick check that the four paths above are correct. If any prints FALSE, fix that
# line (put the correct path between the quotes) and re-run this block.
cat("PKG_DIR ok:", dir.exists(PKG_DIR),
    "| PEG_DIR ok:", dir.exists(PEG_DIR),
    "| HBS_CSV ok:", file.exists(HBS_CSV), "\n")
stopifnot(dir.exists(PKG_DIR), dir.exists(PEG_DIR), file.exists(HBS_CSV))

## --- 1. Load the package + register the PEG/HBS rules ---------------------
# During development you don't need to install -- load_all() sources the package:
#   install.packages("devtools")   # once
devtools::load_all(PKG_DIR)                 # OR: library(cohortHarmonize) after install
source(file.path(EX_DIR, "register_peg_hbs_rules.R"))   # adds the ~40 study rules

ds  <- file.path(EX_DIR, "dataschema.csv")
dqm <- file.path(EX_DIR, "dq_metadata.csv")

## --- 2. PEG comes as several CSVs -> read + full-join on PEGID ------------
read_peg_multi <- function(dir) {
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  raws  <- lapply(files, function(f) suppressMessages(readr::read_csv(f, show_col_types = FALSE)))
  Reduce(function(a, b) dplyr::full_join(a, b, by = "PEGID"), raws)
}
peg_raw <- read_peg_multi(PEG_DIR)
cat(sprintf("PEG raw: %d participants x %d columns\n", nrow(peg_raw), ncol(peg_raw)))

peg <- harmonize_source(source_dataframe(peg_raw),
                        file.path(EX_DIR, "peg_source_map.csv"), ds,
                        source_name = "PEG", id_col = "PEGID", collapse = FALSE)

## --- 3. HBS ----------------------------------------------------------------
options(ch.hbs_visit_date_col = "enrollment_date")   # age = age at enrollment

USE_REAL_HBS <- TRUE   # <<< set TRUE to pull the REAL HBS database via the REDCap API

if (USE_REAL_HBS) {
  # Prerequisites (one time): install.packages("REDCapR"); and REDCAP_URI + REDCAP_TOKEN
  # already set in your .Renviron (the same ones you used all summer).
  # source_redcap() pulls ALL fields for ALL records; harmonize_source() then collapses
  # REDCap's longitudinal multi-row export to one row per participant and harmonizes.
  hbs <- harmonize_source(source_redcap(),
                          file.path(EX_DIR, "hbs_source_map.csv"), ds,
                          source_name = "HBS", id_col = "record_id")   # collapse = TRUE by default
} else {
  # Offline: the bundled synthetic fixture (5 fake records).
  hbs <- harmonize_source(source_csv(HBS_CSV),
                          file.path(EX_DIR, "hbs_source_map.csv"), ds,
                          source_name = "HBS", id_col = "record_id", collapse = TRUE)
}

## --- 4. Combine + inspect + quality report -------------------------------
combined <- combine_sources(peg, hbs)
print(combined)                                   # e.g. 495 PEG + N HBS rows x 98 vars
print(coverage_report(combined), n = 100)         # how populated each variable is
print(select_variables(combined, "exp_coffee"))   # pull just one variable
dq_report(combined, dqm)                          # the four-dimension QC report

## --- 5. (Optional) federated mode: harmonize each site, combine images ----
# save_harmonized_image(peg, "peg_site.rds", mode = "rows")
# save_harmonized_image(hbs, "hbs_site.rds", mode = "rows")
# combined2 <- combine_harmonized(c("peg_site.rds", "hbs_site.rds"))
