# =============================================================================
# federated.R -- run-locally-at-each-site, combine-outputs mode.
# When raw data cannot leave a site (IP / privacy), each site harmonizes its own
# data to the shared DataSchema and saves ONLY the harmonized image; those images
# (or aggregate summaries) are then combined centrally. Raw records never move.
# =============================================================================

#' Save a harmonized image for federated combination
#'
#' @param harmonized A `ch_harmonized` object (already mapped to the shared
#'   DataSchema, so it contains no raw source fields).
#' @param path Output `.rds` path.
#' @param mode `"rows"` saves the harmonized record-level table (shareable if the
#'   target variables are non-identifying); `"summary"` saves only per-variable
#'   aggregate statistics (coverage + value distributions), for when even
#'   harmonized rows cannot leave the site.
#' @param dq A precomputed [dq_report()] to bundle in (optional).
#' @return `path`, invisibly.
#' @export
save_harmonized_image <- function(harmonized, path, mode = c("rows", "summary"), dq = NULL) {
  mode <- match.arg(mode)
  ds <- attr(harmonized, "dataschema")
  payload <- list(mode = mode, source = unique(harmonized$source), n = nrow(harmonized),
                  created = Sys.time(), dataschema = ds, coverage = coverage_report(harmonized, ds), dq = dq)
  if (mode == "rows") payload$data <- harmonized
  else {                                   # summary-only: distributions, never rows
    vars <- setdiff(names(harmonized), c("id", "source"))
    payload$summary <- purrr::map_dfr(vars, function(v) {
      x <- suppressWarnings(as.numeric(harmonized[[v]])); nm <- x[!is.na(x)]
      tibble::tibble(variable = v, n = length(nm),
                     mean = if (length(nm)) mean(nm) else NA, sd = if (length(nm)) stats::sd(nm) else NA,
                     dist = paste(utils::capture.output(print(table(nm))), collapse = " | "))
    })
  }
  saveRDS(payload, path); message("saved ", mode, " image: ", path); invisible(path)
}

#' Combine harmonized images from multiple sites
#'
#' @param paths Character vector of `.rds` paths written by
#'   [save_harmonized_image()] (mode = "rows").
#' @return A combined `ch_harmonized` object.
#' @export
combine_harmonized <- function(paths) {
  imgs <- lapply(paths, readRDS)
  row_imgs <- Filter(function(p) identical(p$mode, "rows") && !is.null(p$data), imgs)
  if (!length(row_imgs))
    stop("no row-level images to combine (were they saved with mode='summary'?)", call. = FALSE)
  combine_sources(lapply(row_imgs, `[[`, "data"))
}
