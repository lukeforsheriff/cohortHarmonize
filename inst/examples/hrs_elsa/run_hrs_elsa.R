# =============================================================================
# run_hrs_elsa.R -- harmonize two UNRELATED aging cohorts (HRS + ELSA) on the
# cohortHarmonize package. Demonstrates the package generalizing far beyond the
# PEG/HBS Parkinson's work it was built for.
#
# The two synthetic files deliberately use DIFFERENT variable names AND different
# codings (ELSA sex = 0/1, conditions = 1=yes/2=no, self-rated health & education
# reversed) -- exactly the kind of mismatch real cohorts have. The source maps +
# recode rules reconcile them onto one shared DataSchema.
# =============================================================================

## --- 0. Point at the example folder ---------------------------------------
# While developing locally, use the source path:
EX <- "/Users/lukemonnich/Downloads/BWH26/p3_HARMONIZATION/cohortHarmonize/inst/examples/hrs_elsa"
# After you commit/push these files and reinstall from GitHub, you can use instead:
#   EX <- system.file("examples/hrs_elsa", package = "cohortHarmonize")

library(cohortHarmonize)                                  # the installed package
source(file.path(EX, "register_hrs_elsa_rules.R"))        # adds edu_years_to_cat
ds  <- file.path(EX, "dataschema.csv")
dqm <- file.path(EX, "dq_metadata.csv")

## --- 1. Harmonize each cohort ---------------------------------------------
hrs  <- harmonize_source(source_csv(file.path(EX, "hrs_synthetic.csv")),
                         file.path(EX, "hrs_source_map.csv"),  ds,
                         source_name = "HRS",  id_col = "hhidpn")
elsa <- harmonize_source(source_csv(file.path(EX, "elsa_synthetic.csv")),
                         file.path(EX, "elsa_source_map.csv"), ds,
                         source_name = "ELSA", id_col = "idauniq")

## --- 2. Combine + inspect + quality report --------------------------------
combined <- combine_sources(hrs, elsa)
print(combined)                                   # HRS + ELSA rows x 10 shared variables
print(coverage_report(combined), n = 100)
print(select_domain(combined, "Health"))          # pull one domain
dq_report(combined, dqm)                           # note: it flags the seeded bmi = 8 (below hard limit)

## --- 3. Using the REAL data ------------------------------------------------
# 1) Register + download the Harmonized HRS and Harmonized ELSA files (Stata .dta)
#    from https://g2aging.org (free registration). Each comes with a codebook.
# 2) Read them with source_haven() instead of source_csv():
#      hrs  <- harmonize_source(source_haven("H_HRS_c.dta"),
#                               file.path(EX,"hrs_source_map.csv"), ds,
#                               source_name = "HRS",  id_col = "hhidpn")
#      elsa <- harmonize_source(source_haven("H_ELSA_g3.dta"),
#                               file.path(EX,"elsa_source_map.csv"), ds,
#                               source_name = "ELSA", id_col = "idauniq")
# 3) Update `source_columns` in the two maps to the real wave variables (e.g.
#    r13agey for HRS wave 13); the Gateway codebook lists the exact names. The
#    recode rules and DataSchema stay exactly the same.
