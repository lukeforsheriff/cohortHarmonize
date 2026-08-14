test_that("built-in recode rules are registered", {
  expect_true(all(c("binary", "any_of", "checkbox_any", "gt0", "coalesce",
                    "map", "year_of", "age_from_dates", "unavailable") %in% list_recodes()))
})

test_that("binary keeps only 0/1", {
  df <- data.frame(x = c(0, 1, 2, NA))
  fn <- cohortHarmonize:::.get_recode("binary")
  expect_equal(fn(df, "x", NULL), c(0L, 1L, NA_integer_, NA_integer_))
})

test_that("any_of is 1 if any, 0 if all 0, NA if all missing", {
  df <- data.frame(a = c(1, 0, NA), b = c(0, 0, NA))
  fn <- cohortHarmonize:::.get_recode("any_of")
  expect_equal(fn(df, c("a", "b"), NULL), c(1L, 0L, NA_integer_))
})

test_that("checkbox_any treats blank as 0 (never NA)", {
  df <- data.frame(a = c(1, NA), b = c(NA, NA))
  fn <- cohortHarmonize:::.get_recode("checkbox_any")
  expect_equal(fn(df, c("a", "b"), NULL), c(1L, 0L))
})

test_that("map applies a value->value spec with NA", {
  df <- data.frame(x = c(1, 2, 7))
  fn <- cohortHarmonize:::.get_recode("map")
  expect_equal(unname(fn(df, "x", "1=1;2=1;7=NA")), c(1, 1, NA))
})

test_that("age_from_dates computes floor years", {
  df <- data.frame(dob = "1950-01-01", visit = "2016-07-01")
  fn <- cohortHarmonize:::.get_recode("age_from_dates")
  expect_equal(fn(df, c("dob", "visit"), NULL), 66)
})

test_that("register_recode adds a custom rule", {
  register_recode("double_it", function(df, cols, param = NULL) 2 * suppressWarnings(as.numeric(df[[cols[1]]])))
  expect_true("double_it" %in% list_recodes())
})
