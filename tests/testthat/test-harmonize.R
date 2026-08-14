test_that("harmonize_source builds the full schema shape and combines", {
  ds  <- data.frame(variable = c("sex", "age"), domain = "Demographics", type = "integer")
  raw <- data.frame(record_id = 1:3, gender = c("F", "M", "F"),
                    dob = c("1950-01-01", "1960-06-15", "1970-03-20"),
                    visit = "2016-01-01")
  register_recode("sex_text", function(df, cols, param = NULL) {
    s <- tolower(trimws(as.character(df[[cols[1]]])))
    ifelse(s %in% c("f", "female"), 1L, ifelse(s %in% c("m", "male"), 2L, NA_integer_))
  })
  map <- data.frame(target_variable = c("sex", "age"),
                    source_columns = c("gender", "dob;visit"),
                    recode = c("sex_text", "age_from_dates"), param = NA)

  h <- harmonize_source(source_dataframe(raw), map, ds, source_name = "A")
  expect_s3_class(h, "ch_harmonized")
  expect_true(all(c("id", "source", "sex", "age") %in% names(h)))
  expect_equal(h$sex, c(1L, 2L, 1L))
  expect_equal(h$age, c(66, 55, 45))

  h2 <- harmonize_source(source_dataframe(raw), map, ds, source_name = "B")
  comb <- combine_sources(h, h2)
  expect_equal(nrow(comb), 6)
  expect_setequal(unique(comb$source), c("A", "B"))
})

test_that("validate_source_map flags unknown targets and rules", {
  ds  <- data.frame(variable = "sex", domain = "Demographics", type = "integer")
  bad <- data.frame(target_variable = c("sex", "nope"),
                    source_columns = c("gender", "x"),
                    recode = c("binary", "not_a_rule"))
  iss <- validate_source_map(bad, ds)
  expect_true(any(grepl("not in dataschema", iss$issue)))
  expect_true(any(grepl("not registered", iss$issue)))
})
