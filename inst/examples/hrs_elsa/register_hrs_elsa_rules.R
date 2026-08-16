# One study-specific recode rule this demo needs beyond the built-ins:
# HRS records education as YEARS; the DataSchema wants a 3-level category.
# (ELSA already provides a category, so it uses the built-in `map` rule instead.)
suppressPackageStartupMessages(library(cohortHarmonize))

register_recode("edu_years_to_cat", function(df, cols, param = NULL) {
  y <- suppressWarnings(as.numeric(df[[cols[1]]]))
  as.integer(ifelse(is.na(y), NA,
              ifelse(y <= 11, 1L,          # 1 = less than secondary
              ifelse(y <= 15, 2L, 3L))))    # 2 = secondary, 3 = tertiary
})

message("Registered edu_years_to_cat. Total rules now: ", length(list_recodes()))
