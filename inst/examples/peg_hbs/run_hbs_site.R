# =============================================================================
# run_hbs_site.R  --  HBS SITE (BWH).  (This is YOUR end.)
# Harmonizes HBS locally from the REDCap API, writes ONE shareable output file,
# then combines it with the image PEG sends back.
# =============================================================================

library(cohortHarmonize)
source("register_peg_hbs_rules.R")
ds <- "dataschema.csv"

## --- 1. Harmonize HBS locally (raw REDCap data never leaves BWH) ------------
options(ch.hbs_visit_date_col = "enrollment_date")
hbs <- harmonize_source(source_redcap(), "hbs_source_map.csv", ds,
                        source_name = "HBS", id_col = "record_id")   # collapses longitudinal export
save_harmonized_image(hbs, "hbs_harmonized.rds", mode = "rows")

## --- 2. After PEG sends you "peg_harmonized.rds", combine the two images ----
combined <- combine_harmonized(c("hbs_harmonized.rds", "peg_harmonized.rds"))
print(combined)                                   # HBS + PEG participants, one schema
print(coverage_report(combined), n = 100)
dq_report(combined, "dq_metadata.csv")
# View(combined)   # <- this is how you "see" the combined (incl. federated PEG) data
