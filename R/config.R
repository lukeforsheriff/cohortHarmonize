# =============================================================================
# config.R -- scaffolding + validation for the three user-supplied config files:
#   dataschema  : the target variables (what you harmonize TO)
#   source_map  : how one source's raw columns map onto the dataschema
#   dq_metadata : admissible values / limits that drive the QC report
# new_*() write starter templates; validate_source_map() checks a map is sound.
# =============================================================================

#' Scaffold the config files
#'
#' Write a starter template you then fill in. See
#' `vignette("creating-a-dataschema", package = "cohortHarmonize")`.
#'
#' @param path Output CSV path.
#' @param overwrite Overwrite if it exists.
#' @return `path`, invisibly.
#' @name config_templates
NULL

.write_template <- function(path, df, overwrite) {
  if (file.exists(path) && !overwrite) stop(path, " exists; set overwrite = TRUE", call. = FALSE)
  readr::write_csv(df, path); message("wrote template: ", path); invisible(path)
}

#' @rdname config_templates
#' @export
new_dataschema <- function(path = "dataschema.csv", overwrite = FALSE)
  .write_template(path, tibble::tibble(
    variable = c("sex", "age", "example_condition"),
    domain   = c("Demographics", "Demographics", "Health Conditions"),
    type     = c("integer", "integer", "integer"),
    description = c("1=Female, 2=Male, 3=Other", "age in years", "0=no, 1=yes")),
    overwrite)

#' @rdname config_templates
#' @export
new_source_map <- function(path = "source_map.csv", overwrite = FALSE)
  .write_template(path, tibble::tibble(
    target_variable = c("sex", "age", "example_condition"),
    source_columns  = c("gender", "dob;visit_date", "cond_flag;cond_flag_fu"),
    recode          = c("map", "age_from_dates", "any_of"),
    param           = c("1=1;2=2", NA, NA),
    note            = c("map raw gender codes", "DOB then visit date", "yes at any timepoint")),
    overwrite)

#' @rdname config_templates
#' @export
new_dq_metadata <- function(path = "dq_metadata.csv", overwrite = FALSE)
  .write_template(path, tibble::tibble(
    variable = c("sex", "age", "example_condition"),
    domain   = c("Demographics", "Demographics", "Health Conditions"),
    data_type = c("integer", "integer", "integer"),
    admissible_values = c("1;2;3", "", "0;1"),
    hard_limits = c("", "0;120", ""),
    soft_limits = c("", "18;100", "")),
    overwrite)

#' Validate a source map against a dataschema and the recode registry
#'
#' Flags target variables not in the schema, recode rules that are not
#' registered, and empty source columns -- before you run a harmonization.
#'
#' @param source_map,dataschema Data frames or CSV paths.
#' @return A tibble of issues (empty tibble = all good).
#' @export
validate_source_map <- function(source_map, dataschema) {
  map <- .as_df(source_map); ds <- .as_df(dataschema); rules <- list_recodes(); issues <- list()
  add <- function(variable, issue) issues[[length(issues) + 1]] <<- tibble::tibble(variable = variable, issue = issue)
  need <- setdiff(c("target_variable", "source_columns", "recode"), names(map))
  if (length(need)) add(NA, paste("source_map missing columns:", paste(need, collapse = ", ")))
  if (length(need) == 0) {
    for (i in seq_len(nrow(map))) {
      v <- map$target_variable[i]
      if (!v %in% ds$variable) add(v, "target_variable not in dataschema")
      if (!map$recode[i] %in% rules) add(v, sprintf("recode '%s' not registered (see list_recodes())", map$recode[i]))
      if (is.na(map$source_columns[i]) || map$source_columns[i] == "")
        if (!identical(map$recode[i], "unavailable")) add(v, "no source_columns (use recode='unavailable' if intended)")
    }
  }
  if (!length(issues)) { message("source_map OK: ", nrow(map), " mappings, no issues."); return(tibble::tibble()) }
  dplyr::bind_rows(issues)
}
