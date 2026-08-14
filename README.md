# cohortHarmonize

This is a config-driven and source-agnostic **harmonization + data-quality assessment**
for longitudinal cohort studies. The package translates raw data from file types
**CSV, a REDCap API, SAS/SPSS/Stata files, or a database** into a single harmonized 
dataset that follows a DataSchema the user defines. The package then grades the result 
with the four-dimension data-quality framework proposed by Schmidt et al. (2021). Built 
for multi-cohort work (originally the PEG and HBS Parkinson's cohorts), the package also
includes a **federated mode** so that studies that cannot share raw data due to privacy
reasons can still be harmonized.

## Install

```r
# install.packages("remotes")
remotes::install_github("lukemonnich/cohortHarmonize")
```

Core dependencies are light (`readr`, `dplyr`, `tidyr`, `purrr`, `stringr`,
`tibble`). Optional readers are used only if you need them: `REDCapR` (API),
`haven` (SAS/SPSS/Stata), `DBI` (databases).

## The idea in one picture

```
 raw source ──▶ read_source() ──▶ collapse_longitudinal() ──▶ harmonize_source()
 (csv/API/…)         (adapter)        (1 row per record)      (apply source_map +
                                                               recode registry)
                                                                    │
 multiple sources ─────────────────────────────────────────▶ combine_sources()
                                                                    │
                                                              dq_report()
```

Three things the user must write (once per project), all as plain CSVs:

| File | What it is |
|---|---|
| **dataschema** | the target variables to harmonize *to* (`variable`, `domain`, `type`) |
| **source_map** | per source: which raw columns map to each target, and the `recode` rule |
| **dq_metadata** | admissible values / limits that drive the quality report |

Scaffold them with `new_dataschema()`, `new_source_map()`, `new_dq_metadata()` and
see `vignette("creating-a-dataschema")`.

## Quick start

```r
library(cohortHarmonize)

ds  <- "dataschema.csv"
peg <- harmonize_source(source_csv("peg.csv"),  "peg_map.csv", ds, source_name = "PEG")
hbs <- harmonize_source(source_redcap(),        "hbs_map.csv", ds, source_name = "HBS") # creds from .Renviron

combined <- combine_sources(peg, hbs)

select_domain(combined, "Exposures")          # pull a domain
select_variables(combined, "exp_coffee")      # or one variable
coverage_report(combined)                     # how populated each variable is
dq_report(combined, "dq_metadata.csv")        # the four-dimension QC report
```

## Extending the recode rules

Built-in rules (`list_recodes()`): `keep`, `binary`, `numeric`, `na_codes`, `map`,
`any_of`, `checkbox_any`, `gt0`, `coalesce`, `year_of`, `age_from_dates`,
`unavailable`. The user can add their own study-specific rule:

```r
register_recode("race_checkbox", function(df, cols, param = NULL) {
  # ... roll checkbox columns up to a single coded race ...
})
```

Then reference `race_checkbox` in the `recode` column of a source map.

## Federated (run-locally, combine-outputs) mode

For sites that cannot share raw records, each site harmonizes locally and shares
only the harmonized image (or aggregate summaries):

```r
# At each site (raw data never leaves):
save_harmonized_image(harmonize_source(...), "site_A.rds", mode = "rows")     # or mode = "summary"

# Centrally:
combined <- combine_harmonized(c("site_A.rds", "site_B.rds"))
```

## License

MIT © 2026 Luke Monnich.
