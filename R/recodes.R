# =============================================================================
# recodes.R -- the recode-rule REGISTRY. The harmonization engine is generic:
# each row of a source map names a `recode` rule; this file supplies the rules.
# Ship generic built-ins; users add study-specific rules with register_recode().
# Every rule is a function(df, cols, param) -> a vector of length nrow(df).
# =============================================================================

.ch_registry <- new.env(parent = emptyenv())

#' Register a custom recode rule
#'
#' Add a study-specific transformation the engine can call by name from a source
#' map (e.g. a checkbox roll-up, a scale threshold, a bespoke derivation).
#'
#' @param name Rule name used in the `recode` column of a source map.
#' @param fn A function `(df, cols, param)` returning a vector of length
#'   `nrow(df)`. `cols` is the character vector of source columns for that target
#'   variable; `param` is an optional string from the source map.
#' @export
register_recode <- function(name, fn) {
  stopifnot(is.character(name), is.function(fn))
  assign(name, fn, envir = .ch_registry); invisible(name)
}

#' List available recode rules
#' @return Character vector of registered rule names.
#' @export
list_recodes <- function() sort(ls(.ch_registry))

.get_recode <- function(name) {
  if (!exists(name, envir = .ch_registry, inherits = FALSE))
    stop(sprintf("recode rule '%s' is not registered (see list_recodes())", name), call. = FALSE)
  get(name, envir = .ch_registry, inherits = FALSE)
}

# ---- small helpers ----------------------------------------------------------
.num  <- function(df, c) if (c %in% names(df)) suppressWarnings(as.numeric(df[[c]])) else rep(NA_real_, nrow(df))
.kv   <- function(param) {                       # "1=1;2=1;7=NA" -> named numeric
  if (is.null(param) || is.na(param) || param == "") return(NULL)
  p <- strsplit(strsplit(param, ";")[[1]], "=")
  k <- vapply(p, `[`, "", 1); v <- vapply(p, `[`, "", 2)
  stats::setNames(suppressWarnings(as.numeric(ifelse(toupper(v) == "NA", NA, v))), trimws(k))
}
.lims <- function(param) { if (is.null(param) || is.na(param) || param == "") return(c(NA, NA))
  suppressWarnings(as.numeric(strsplit(param, ";")[[1]]))[1:2] }
.na_codes <- function(x, codes) { x[x %in% codes] <- NA; x }

# ---- built-in generic rules -------------------------------------------------
.register_builtins <- function() {
  register_recode("keep",   function(df, cols, param = NULL) .num(df, cols[1]))
  register_recode("binary", function(df, cols, param = NULL) { v <- .num(df, cols[1]); v[!v %in% c(0, 1)] <- NA; as.integer(v) })
  register_recode("numeric", function(df, cols, param = NULL) {
    v <- .num(df, cols[1]); lim <- .lims(param)
    if (!is.na(lim[1])) v[v < lim[1]] <- NA; if (!is.na(lim[2])) v[v > lim[2]] <- NA; v })
  register_recode("na_codes", function(df, cols, param = NULL) {
    codes <- suppressWarnings(as.numeric(strsplit(param %||% "", ";")[[1]])); .na_codes(.num(df, cols[1]), codes) })
  register_recode("map", function(df, cols, param = NULL) {           # value -> value
    lut <- .kv(param); v <- as.character(.num(df, cols[1])); out <- lut[v]; unname(out) })
  register_recode("any_of", function(df, cols, param = NULL) {        # 1 if any==1, 0 if all 0, NA if all missing
    cols <- cols[cols %in% names(df)]; if (!length(cols)) return(rep(NA_integer_, nrow(df)))
    m <- sapply(cols, function(c) .num(df, c)); if (is.null(dim(m))) m <- matrix(m, ncol = 1)
    apply(m, 1, function(r) if (all(is.na(r))) NA_integer_ else if (any(r == 1, na.rm = TRUE)) 1L else 0L) })
  register_recode("checkbox_any", function(df, cols, param = NULL) {  # "mark if you use": blank -> 0
    cols <- cols[cols %in% names(df)]; if (!length(cols)) return(rep(NA_integer_, nrow(df)))
    m <- sapply(cols, function(c) { v <- .num(df, c); !is.na(v) & v == 1 }); if (is.null(dim(m))) m <- matrix(m, ncol = 1)
    as.integer(apply(m, 1, any)) })
  register_recode("gt0", function(df, cols, param = NULL) {           # any col > 0 -> 1
    cols <- cols[cols %in% names(df)]; if (!length(cols)) return(rep(NA_integer_, nrow(df)))
    m <- sapply(cols, function(c) .num(df, c)); if (is.null(dim(m))) m <- matrix(m, ncol = 1)
    apply(m, 1, function(r) if (all(is.na(r))) NA_integer_ else if (any(r > 0, na.rm = TRUE)) 1L else 0L) })
  register_recode("coalesce", function(df, cols, param = NULL) {      # first non-NA across cols (baseline priority)
    cols <- cols[cols %in% names(df)]; if (!length(cols)) return(rep(NA_real_, nrow(df)))
    Reduce(function(a, b) dplyr::coalesce(a, b), lapply(cols, function(c) .num(df, c))) })
  register_recode("year_of", function(df, cols, param = NULL) {       # calendar year of a date column
    if (!cols[1] %in% names(df)) return(rep(NA_integer_, nrow(df)))
    as.integer(format(.parse_date(df[[cols[1]]]), "%Y")) })
  register_recode("age_from_dates", function(df, cols, param = NULL) {# completed years; cols = DOB then event date
    if (length(cols) < 2 || !all(cols[1:2] %in% names(df))) return(rep(NA_integer_, nrow(df)))
    d1 <- .parse_date(df[[cols[1]]]); d2 <- .parse_date(df[[cols[2]]])
    y <- as.integer(format(d2, "%Y")) - as.integer(format(d1, "%Y"))
    before_bday <- as.integer(format(d2, "%m%d")) < as.integer(format(d1, "%m%d"))  # birthday not yet reached
    a <- y - as.integer(before_bday); a[is.na(d1) | is.na(d2) | a < 0 | a > 120] <- NA
    as.integer(a) })
  register_recode("unavailable", function(df, cols, param = NULL) rep(NA_real_, nrow(df)))
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# flexible date parser (ISO or MM-DD-YYYY etc.); shared by year_of / age_from_dates
.parse_date <- function(x) {
  x <- trimws(as.character(x)); x[x == "" | tolower(x) %in% c("na", "nan")] <- NA
  fmts <- c("%Y-%m-%d", "%m-%d-%Y", "%m/%d/%Y", "%Y/%m/%d", "%d-%m-%Y")
  best <- as.Date(rep(NA_character_, length(x))); best_n <- -1L
  for (f in fmts) { d <- suppressWarnings(as.Date(x, format = f)); n <- sum(!is.na(d))
    if (n > best_n) { best_n <- n; best <- d } }
  best
}

.onLoad <- function(libname, pkgname) .register_builtins()
