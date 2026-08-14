#' cohortHarmonize: config-driven harmonization + QC for longitudinal cohorts
#'
#' A source-agnostic toolkit that turns raw cohort data (CSV, REDCap API,
#' SAS/SPSS/Stata, or a database) into one harmonized dataset following a
#' user-defined DataSchema, then grades the result with the Schmidt et al. (2021)
#' data-quality framework. See the vignettes:
#' \itemize{
#'   \item \code{vignette("getting-started", package = "cohortHarmonize")}
#'   \item \code{vignette("creating-a-dataschema", package = "cohortHarmonize")}
#' }
#'
#' The workflow is: describe each source in a \emph{source map} that points its
#' raw columns at DataSchema target variables and names a recode rule; call
#' [harmonize_source()] per source; [combine_sources()] them; then [dq_report()].
#' For sites that cannot share raw data, [save_harmonized_image()] +
#' [combine_harmonized()] support a federated (run-locally, combine-outputs) mode.
#'
#' @keywords internal
"_PACKAGE"
