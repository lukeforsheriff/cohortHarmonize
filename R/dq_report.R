# =============================================================================
# dq_report.R -- data-quality report following Schmidt et al. (2021)'s four
# dimensions (integrity, completeness, consistency, accuracy), driven by the
# dq_metadata config. Generic across sources.
# =============================================================================

.dq_lims <- function(s) { if (is.null(s) || is.na(s) || s == "") return(c(NA, NA))
  suppressWarnings(as.numeric(strsplit(s, ";")[[1]]))[1:2] }

#' Data-quality report (Schmidt et al. 2021 framework)
#'
#' Runs metadata-driven indicators across four dimensions and prints a summary:
#' \itemize{
#'   \item \strong{Integrity} -- observed data types vs `data_type`.
#'   \item \strong{Completeness} -- item missingness (overall and by source).
#'   \item \strong{Consistency} -- inadmissible categorical values and hard/soft
#'         limit deviations (inadmissible vs merely improbable).
#'   \item \strong{Accuracy} -- univariate outliers, and location/proportion
#'         differences by the `source` process variable (the harmonization-success
#'         screen; |SMD| >= 0.2 flags a variable to inspect).
#' }
#' @param harmonized A `ch_harmonized` object.
#' @param dq_metadata Data frame / CSV path with columns `variable`,
#'   `data_type`, `admissible_values`, `hard_limits`, `soft_limits` (and optional
#'   `domain`).
#' @param contradictions Optional named list of one-row-per-record logical checks,
#'   e.g. `list("end>=start" = quote(end_age >= start_age))`, evaluated in the
#'   harmonized data; each is reported as a consistency (contradiction) indicator.
#' @return A named list of tidy result tables (invisibly).
#' @export
dq_report <- function(harmonized, dq_metadata, contradictions = NULL) {
  md <- .as_df(dq_metadata); has_src <- "source" %in% names(harmonized)
  vars <- intersect(md$variable, names(harmonized)); n <- nrow(harmonized)
  col <- function(v) suppressWarnings(as.numeric(harmonized[[v]]))

  # -- Integrity: data-type conformance --
  integrity <- purrr::map_dfr(vars, function(v) {
    dt <- md$data_type[md$variable == v][1]; x <- harmonized[[v]]; xn <- suppressWarnings(as.numeric(x))
    nonmiss <- !is.na(x)
    bad <- if (identical(dt, "integer")) sum(nonmiss & (is.na(xn) | xn != round(xn)))
           else if (identical(dt, "float")) sum(nonmiss & is.na(xn)) else 0
    tibble::tibble(dimension = "Integrity", indicator = "Data type mismatch", variable = v,
                   n_mismatch = bad, status = if (bad == 0) "PASS" else "FAIL")
  })

  # -- Completeness: item missingness --
  completeness <- purrr::map_dfr(vars, function(v) {
    row <- tibble::tibble(dimension = "Completeness", indicator = "Missing values", variable = v,
                          pct_missing = round(100 * mean(is.na(harmonized[[v]])), 1))
    if (has_src) for (s in sort(unique(harmonized$source)))
      row[[paste0("pct_missing_", s)]] <- round(100 * mean(is.na(harmonized[[v]][harmonized$source == s])), 1)
    row
  })

  # -- Consistency: inadmissible categorical + hard/soft limits --
  cat_md <- md[!is.na(md$admissible_values) & md$admissible_values != "", ]
  consistency_cat <- purrr::map_dfr(intersect(cat_md$variable, vars), function(v) {
    allowed <- suppressWarnings(as.numeric(strsplit(md$admissible_values[md$variable == v][1], ";")[[1]]))
    x <- col(v); nm <- x[!is.na(x)]; bad <- !(nm %in% allowed)
    tibble::tibble(dimension = "Consistency", indicator = "Inadmissible categorical", variable = v,
                   n_checked = length(nm), n_violations = sum(bad), status = if (sum(bad) == 0) "PASS" else "FAIL")
  })
  lim_md <- md[!is.na(md$hard_limits) & md$hard_limits != "", ]
  consistency_lim <- purrr::map_dfr(intersect(lim_md$variable, vars), function(v) {
    hl <- .dq_lims(md$hard_limits[md$variable == v][1]); sl <- .dq_lims(md$soft_limits[md$variable == v][1])
    x <- col(v); nm <- x[!is.na(x)]
    inadm <- (!is.na(hl[1]) & nm < hl[1]) | (!is.na(hl[2]) & nm > hl[2])
    unc <- !inadm & ((!is.na(sl[1]) & nm < sl[1]) | (!is.na(sl[2]) & nm > sl[2]))
    tibble::tibble(dimension = "Consistency", indicator = "Limit deviations", variable = v,
                   n_inadmissible = sum(inadm), n_uncertain = sum(unc), status = if (sum(inadm) == 0) "PASS" else "FAIL")
  })
  consistency_con <- tibble::tibble()
  if (!is.null(contradictions)) consistency_con <- purrr::map_dfr(names(contradictions), function(nm) {
    viol <- tryCatch(sum(!eval(contradictions[[nm]], harmonized), na.rm = TRUE), error = function(e) NA_integer_)
    tibble::tibble(dimension = "Consistency", indicator = "Contradiction", rule = nm,
                   n_violations = viol, status = if (isTRUE(viol == 0)) "PASS" else "FAIL")
  })

  # -- Accuracy: outliers + by-source location/proportion (harmonization screen) --
  ratio <- md$variable[md$data_type %in% c("float", "integer") &
                         (is.na(md$admissible_values) | md$admissible_values == "")]
  accuracy_out <- purrr::map_dfr(intersect(ratio, vars), function(v) {
    x <- col(v); x <- x[!is.na(x)]; if (length(x) < 5) return(NULL)
    q <- stats::quantile(x, c(.25, .75)); iqr <- q[2] - q[1]
    tibble::tibble(dimension = "Accuracy", indicator = "Univariate outliers", variable = v,
                   n_low = sum(x < q[1] - 1.5 * iqr), n_high = sum(x > q[2] + 1.5 * iqr))
  })
  accuracy_src <- tibble::tibble()
  if (has_src && dplyr::n_distinct(harmonized$source) >= 2) {
    srcs <- sort(unique(harmonized$source))[1:2]
    smd <- function(a, b) { a <- a[!is.na(a)]; b <- b[!is.na(b)]
      if (length(a) < 2 || length(b) < 2) NA_real_ else (mean(a) - mean(b)) / sqrt((stats::var(a) + stats::var(b)) / 2) }
    accuracy_src <- purrr::map_dfr(vars, function(v) {
      xa <- col(v)[harmonized$source == srcs[1]]; xb <- col(v)[harmonized$source == srcs[2]]
      if (all(is.na(xa)) || all(is.na(xb))) return(NULL)
      s <- smd(xa, xb)
      tibble::tibble(dimension = "Accuracy", indicator = "By-source SMD", variable = v,
                     SMD = round(s, 3), flag = if (!is.na(s) && abs(s) >= 0.2) "inspect" else "")
    })
  }

  res <- list(integrity = integrity, completeness = completeness,
              consistency_categorical = consistency_cat, consistency_limits = consistency_lim,
              consistency_contradictions = consistency_con,
              accuracy_outliers = accuracy_out, accuracy_by_source = accuracy_src)
  cat("\n===== DATA QUALITY REPORT (Schmidt et al. 2021 framework) =====\n")
  cat(sprintf("  Integrity   : %d/%d vars type-OK\n", sum(integrity$status == "PASS"), nrow(integrity)))
  cat(sprintf("  Completeness: mean %.1f%% missing across %d vars\n", mean(completeness$pct_missing), nrow(completeness)))
  cat(sprintf("  Consistency : %d inadmissible-categorical, %d limit, %d contradiction failures\n",
              sum(consistency_cat$status == "FAIL"), sum(consistency_lim$status == "FAIL"),
              if (nrow(consistency_con)) sum(consistency_con$status == "FAIL") else 0))
  if (nrow(accuracy_src)) cat(sprintf("  Accuracy    : %d vars flagged |SMD|>=0.2 by source\n", sum(accuracy_src$flag == "inspect")))
  cat("===============================================================\n")
  invisible(res)
}
