# =============================================================================
# harmonize.R -- the generic, source-agnostic harmonization engine.
# One function harmonizes ANY source: it loops a source map (target_variable,
# source_columns, recode, param), applies the named recode rule from the
# registry, and returns the full DataSchema shape (unmapped targets = NA).
# =============================================================================

.as_df <- function(x) if (is.character(x) && length(x) == 1 && file.exists(x))
  suppressMessages(readr::read_csv(x, show_col_types = FALSE, progress = FALSE)) else tibble::as_tibble(x)

#' Harmonize one source onto the DataSchema
#'
#' @param source A [sources] object (`source_csv()`, `source_redcap()`, ...), a
#'   raw data.frame, or a path to a CSV.
#' @param source_map Data frame or CSV path mapping this source to the schema.
#'   Required columns: `target_variable`, `source_columns` (";"-separated),
#'   `recode` (a rule from [list_recodes()]). Optional: `param`.
#' @param dataschema Data frame or CSV path listing the target variables.
#'   Required column: `variable`. Optional: `domain`, `type`.
#' @param source_name Label stored in the `source` column (e.g. "PEG").
#' @param id_col Record-id column in the raw data.
#' @param collapse Collapse a longitudinal multi-row export to one row per record
#'   first (see [collapse_longitudinal()]).
#' @return A `ch_harmonized` tibble: `id`, `source`, and every DataSchema variable.
#' @export
harmonize_source <- function(source, source_map, dataschema, source_name = "source",
                             id_col = "record_id", collapse = TRUE) {
  raw <- if (inherits(source, "ch_source")) read_source(source) else .as_df(source)
  if (collapse && id_col %in% names(raw) && anyDuplicated(raw[[id_col]]) > 0)
    raw <- collapse_longitudinal(raw, id_col)
  map <- .as_df(source_map); ds <- .as_df(dataschema)
  stopifnot("source_map needs target_variable/source_columns/recode" =
              all(c("target_variable", "source_columns", "recode") %in% names(map)))
  stopifnot("dataschema needs a 'variable' column" = "variable" %in% names(ds))
  idc <- if (id_col %in% names(raw)) id_col else names(raw)[1]
  n <- nrow(raw)
  out <- tibble::tibble(id = as.character(raw[[idc]]), source = source_name)
  for (v in ds$variable) out[[v]] <- rep(NA_real_, n)          # full schema shape
  for (i in seq_len(nrow(map))) {
    v <- map$target_variable[i]; if (!v %in% names(out)) next
    cols <- if (is.na(map$source_columns[i]) || map$source_columns[i] == "") character(0)
            else strsplit(map$source_columns[i], ";")[[1]] |> trimws()
    param <- if ("param" %in% names(map)) map$param[i] else NA
    fun <- tryCatch(.get_recode(map$recode[i]), error = function(e) NULL)
    if (is.null(fun)) { warning(sprintf("%s: unknown recode '%s' -> NA", v, map$recode[i]), call. = FALSE); next }
    out[[v]] <- tryCatch(fun(raw, cols, param),
      error = function(e) { warning(sprintf("%s (%s): %s", v, map$recode[i], conditionMessage(e)), call. = FALSE); rep(NA_real_, n) })
  }
  structure(out, class = c("ch_harmonized", class(out)), dataschema = ds)
}

#' @export
print.ch_harmonized <- function(x, ...) {
  cat(sprintf("<ch_harmonized> %d records x %d variables | sources: %s\n",
              nrow(x), sum(!names(x) %in% c("id", "source")),
              paste(unique(x$source), collapse = ", ")))
  NextMethod()
}

#' Stack harmonized sources into one long dataset
#' @param ... `ch_harmonized` tibbles (or a list of them).
#' @export
combine_sources <- function(...) {
  frames <- list(...); if (length(frames) == 1 && is.list(frames[[1]]) && !is.data.frame(frames[[1]])) frames <- frames[[1]]
  frames <- frames[vapply(frames, nrow, 0L) > 0]
  if (!length(frames)) stop("no non-empty sources to combine", call. = FALSE)
  out <- dplyr::bind_rows(frames)
  structure(out, class = c("ch_harmonized", class(tibble::as_tibble(out))), dataschema = attr(frames[[1]], "dataschema"))
}

#' Pull a domain or specific variables from a harmonized dataset
#' @param harmonized A `ch_harmonized` object.
#' @param domains,vars Character vectors.
#' @param dataschema Schema with a `domain` column (defaults to the attached one).
#' @name select
NULL

#' @rdname select
#' @export
select_domain <- function(harmonized, domains, dataschema = attr(harmonized, "dataschema")) {
  stopifnot("dataschema needs a 'domain' column" = !is.null(dataschema) && "domain" %in% names(dataschema))
  vars <- dataschema$variable[dataschema$domain %in% domains]
  dplyr::select(harmonized, dplyr::any_of(c("id", "source", vars)))
}
#' @rdname select
#' @export
select_variables <- function(harmonized, vars)
  dplyr::select(harmonized, dplyr::any_of(c("id", "source", vars)))

#' Coverage report: how populated each harmonized variable is
#' @param harmonized A `ch_harmonized` object.
#' @param dataschema Schema (defaults to the attached one).
#' @return A tibble: variable, domain, n_populated, pct_populated, value_range.
#' @export
coverage_report <- function(harmonized, dataschema = attr(harmonized, "dataschema")) {
  vars <- setdiff(names(harmonized), c("id", "source"))
  dom <- if (!is.null(dataschema) && "domain" %in% names(dataschema))
    dataschema$domain[match(vars, dataschema$variable)] else NA
  purrr::map_dfr(seq_along(vars), function(k) {
    v <- vars[k]; x <- harmonized[[v]]; nm <- sum(!is.na(x))
    rng <- if (nm > 0 && is.numeric(x)) sprintf("%s..%s", min(x, na.rm = TRUE), max(x, na.rm = TRUE)) else NA_character_
    tibble::tibble(variable = v, domain = dom[k], n_total = length(x), n_populated = nm,
                   pct_populated = round(100 * nm / length(x), 1), value_range = rng)
  })
}
