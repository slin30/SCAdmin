context("collapse_simple")

# Temp
# library(SCAdmin)
# library(testthat)

# ref data ----------------------------------------------------------------

test_df <- tryCatch(
  {
    dget("../testdata/test_set_unnested.txt")
  }, warning = function(w) {
    suppressMessages(w)
    dget("./tests/testdata/test_set_unnested.txt")
  }, finally = NULL
)


# tests -------------------------------------------------------------------

test_that("a field that is not found throws an error", {
  expect_error(collapse_simple_target(test_df, "idd"), 
               ".* not found in x")
})

test_that("a target field that is not a list throws an error", {
  expect_error(collapse_simple_target(test_df, "id"), 
               ".*Expected an input of class 'list', but input is a character")
})

test_that("a target field that is not a simple list throws an error", {
  expect_error(collapse_simple_target(test_df$definition$container, "rules"), 
               ".*character expected for parsing, but encountered data.frame at x\\[\\[1\\]\\] instead")
})

test_that("a target field with an unexpected structure throws an error", {
  expect_error(collapse_simple_target(test_df$definition, "container"), 
               ".*Mismatch in row count and target column length")
})


