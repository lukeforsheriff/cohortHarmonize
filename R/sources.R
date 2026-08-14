# =============================================================================
# sources.R -- pluggable input adapters + the longitudinal-collapse step.
# Each source_*() returns a lightweight `ch_source` description; read_source()
# turns it into a raw tibble. This is the "input optionality" layer: CSV, REDCap
# API, statistical formats (SAS/SPSS/Stata via haven), a database (DBI), or an
# in-memory data.frame -- all feed the SAME downstream harmonization engine.
# =============================================================================

new_ch_source <- function(kind, ...) structure(list(kind = kind, ...), class = "ch_source")

#' Describe a CSV / REDCap / statistical-file / database / data.frame source
#'
#' These constructors only *describe* where the data lives; [read_source()]
#' actually reads it. This keeps the source definition portable (e.g. to pass
#' around a pipeline or run at another site).
#'
#' @param path Path to a `.csv` file (`source_csv`) or a `.sav`/`.dta`/`.sas7bdat`
#'   file (`source_haven`).
#' @param url,token REDCap API URI and token (default from the `REDCAP_URI` /
#'   `REDCAP_TOKEN` environment variables, e.g. your `.Renviron`).
#' @param fields Optional character vector of REDCap fields to pull (default all).
#' @param conn A `DBI` connection (for `source_db`).
#' @param query A SQL query string (for `source_db`).
#' @param table A table name to read (for `source_db`).
#' @param df An in-memory data.frame / tibble.
#' @param ... Passed to the underlying reader.
#' @return A `ch_source` object.
#' @name sources
#' @examples
#' src <- source_csv("participants.csv")
#' \dontrun{ raw <- read_source(src) }
NULL

#' @rdname sources
#' @export
source_csv <- function(path, ...) new_ch_source("csv", path = path, args = list(...))

#' @rdname sources
#' @export
source_redcap <- function(url = Sys.getenv("REDCAP_URI"),
                          token = Sys.getenv("REDCAP_TOKEN"),
                          fields = NULL, ...) {
  if (identical(url, "") || identical(token, ""))
    stop("Set REDCAP_URI and REDCAP_TOKEN (e.g. in .Renviron), or pass url/token.", call. = FALSE)
  if (!grepl("^https://", url)) stop("REDCap url must start with https://", call. = FALSE)
  new_ch_source("redcap", url = url, token = token, fields = fields, args = list(...))
}

#' @rdname sources
#' @export
source_haven <- function(path, ...) new_ch_source("haven", path = path, args = list(...))

#' @rdname sources
#' @export
source_db <- function(conn, query = NULL, table = NULL, ...)
  new_ch_source("db", conn = conn, query = query, table = table, args = list(...))

#' @rdname sources
#' @export
source_dataframe <- function(df) new_ch_source("df", df = tibble::as_tibble(df))

#' @export
print.ch_source <- function(x, ...) {
  loc <- switch(x$kind, csv = x$path, haven = x$path, redcap = x$url,
                db = if (!is.null(x$query)) "query" else x$table, df = "<data.frame>")
  cat(sprintf("<ch_source: %s> %s\n", x$kind, loc)); invisible(x)
}

#' Read a source description into a raw tibble
#'
#' Dispatches on the source kind. REDCap/haven/DBI are only needed for those
#' source types and are Suggested (not required) dependencies.
#' @param src A `ch_source` from one of the `source_*()` constructors.
#' @return A tibble of raw records.
#' @export
read_source <- function(src) {
  stopifnot(inherits(src, "ch_source"))
  raw <- switch(src$kind,
    csv = do.call(readr::read_csv, c(list(src$path, show_col_types = FALSE, progress = FALSE), src$args)),
    df  = src$df,
    redcap = {
      if (!requireNamespace("REDCapR", quietly = TRUE)) stop("install.packages('REDCapR')", call. = FALSE)
      REDCapR::redcap_read(redcap_uri = src$url, token = src$token, fields = src$fields, verbose = FALSE)$data
    },
    haven = {
      if (!requireNamespace("haven", quietly = TRUE)) stop("install.packages('haven')", call. = FALSE)
      ext <- tolower(tools::file_ext(src$path))
      reader <- switch(ext, sav = haven::read_sav, zsav = haven::read_sav,
                       dta = haven::read_dta, sas7bdat = haven::read_sas,
                       stop("Unsupported statistical file: ", ext, call. = FALSE))
      out <- reader(src$path); haven::zap_labels(out)   # drop labelled class -> plain values
    },
    db = {
      if (!requireNamespace("DBI", quietly = TRUE)) stop("install.packages('DBI')", call. = FALSE)
      if (!is.null(src$query)) DBI::dbGetQuery(src$conn, src$query) else DBI::dbReadTable(src$conn, src$table)
    },
    stop("unknown source kind: ", src$kind, call. = FALSE)
  )
  tibble::as_tibble(raw)
}

#' Collapse a longitudinal (multi-row-per-record) export to one row per unit
#'
#' Longitudinal exports (e.g. REDCap events / repeat instruments) split a record
#' across rows, with each field blank on the rows where it does not live. This
#' reunites them: for each field, keep the first non-missing value across a
#' record's rows. Cross-sectional / baseline-priority collapse.
#'
#' @param raw A raw tibble.
#' @param id_col The record-id column (default "record_id"; falls back to col 1).
#' @param meta_cols Longitudinal bookkeeping columns to drop before collapsing.
#' @return One row per `id_col`.
#' @export
collapse_longitudinal <- function(raw, id_col = "record_id",
                                  meta_cols = c("redcap_event_name", "redcap_repeat_instrument",
                                                "redcap_repeat_instance")) {
  if (!id_col %in% names(raw)) id_col <- names(raw)[1]
  meta <- intersect(meta_cols, names(raw))
  data_cols <- setdiff(names(raw), c(id_col, meta))
  first_non_na <- function(x) {
    keep <- !is.na(x); if (is.character(x)) keep <- keep & x != ""
    x <- x[keep]; if (length(x)) x[1] else x[NA_integer_]
  }
  raw |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_col))) |>
    dplyr::summarise(dplyr::across(dplyr::all_of(data_cols), first_non_na), .groups = "drop")
}
