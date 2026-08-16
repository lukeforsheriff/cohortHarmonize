# =============================================================================
# run_peg_site.R  --  PEG SITE (UCLA).
# Run this on the machine that holds the raw PEG data. It harmonizes PEG LOCALLY
# and writes ONE shareable output file. No raw PEG record is read or transmitted
# off this machine -- only the harmonized output leaves.
# You received this script + 4 config files from the harmonization lead.
# =============================================================================

## --- EDIT these two lines ---------------------------------------------------
PEG_DIR <- "/path/to/PEG/data"        # folder that contains the PEG .csv files
OUT     <- "peg_harmonized.rds"        # the ONE file you will send back

## --- 1. Install the package once, then load the shared "recipe" -------------
# install.packages("remotes")
# remotes::install_github("lukeforsheriff/cohortHarmonize")
library(cohortHarmonize)
source("register_peg_hbs_rules.R")     # shared recode rules  (contains NO data)
ds <- "dataschema.csv"                  # shared harmonization target (NO data)

## --- 2. Read + join your PEG CSVs on PEGID ----------------------------------
read_peg_multi <- function(dir) {
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  raws  <- lapply(files, function(f) suppressMessages(readr::read_csv(f, show_col_types = FALSE)))
  Reduce(function(a, b) dplyr::full_join(a, b, by = "PEGID"), raws)
}
peg_raw <- read_peg_multi(PEG_DIR)
cat(sprintf("PEG raw: %d participants x %d columns\n", nrow(peg_raw), ncol(peg_raw)))

## --- 3. Harmonize PEG onto the shared DataSchema ----------------------------
peg <- harmonize_source(source_dataframe(peg_raw), "peg_source_map.csv", ds,
                        source_name = "PEG", id_col = "PEGID", collapse = FALSE)

## --- 4. (Optional) inspect YOUR harmonized data + quality, locally ----------
print(peg)
print(coverage_report(peg), n = 100)
dq_report(peg, "dq_metadata.csv")

## --- 5. Write the ONE file to send back -------------------------------------
# mode = "rows"    -> harmonized (DataSchema) variables only; NO raw PEG fields.
# mode = "summary" -> per-variable aggregate stats only (use if your data office
#                     does not allow record-level data to leave).
save_harmonized_image(peg, OUT, mode = "rows")
cat("\nDONE. Send this one file back to the harmonization lead:\n  ", normalizePath(OUT), "\n")
