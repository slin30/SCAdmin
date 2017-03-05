context("collapse_shares")

# ref data ----------------------------------------------------------------

test_df <- tryCatch(
  {
    dget("../testdata/test_set_unnested.txt")
  }, warning = function(w) {
    suppressMessages(w)
    dget("./tests/testdata/test_set_unnested.txt")
  }, finally = NULL
)


# list of mock dfs for shares only
good_shares <- list(structure(list(type = c("user", "user", "group"), 
                                   name = c("aa", "bb", "group1")), 
                              .Names = c("type", "name"), 
                              row.names = c(NA, 3L), 
                              class = "data.frame"), 
                    structure(list(type = "user", 
                                   name = "ab"), 
                              .Names = c("type", "name"), 
                              row.names = c(NA, -1L), 
                              class = "data.frame"), 
                    structure(list(type = "user", 
                                   name = "ac"), 
                              .Names = c("type", "name"), 
                              row.names = c(NA, -1L), 
                              class = "data.frame")
)
good_df <- data.frame(
  A = LETTERS[1:3], 
  shares = cbind(unname(good_shares)), 
  id = c("id_1", "id_2", "id_3"),
  name = c("mock_1", "mock_2", "mock_3"),
  stringsAsFactors = FALSE
)


# tests -------------------------------------------------------------------
test_that("'shares' must be a list of data.frames found in the context of a data.frame", {
  expect_error(collapse_shares(good_df[, c("shares")]))
  expect_error(collapse_shares(good_shares), 
               ".*x must be a data.frame")
  expect_error(collapse_shares(data.frame(
    shares = LETTERS[1:3], 
    id = "id", 
    name = "name",
    stringsAsFactors = FALSE)
  ), ".*Expected a list for 'structures', but class is character"
  )
})

test_that("As long as 'shares', 'name', and 'id' are present, collapsing works", {
  expect_is(collapse_shares(good_df), "data.frame")
  expect_equal(
    sum(vapply(good_df$shares, nrow, FUN.VALUE = integer(1))), 
    nrow(collapse_shares(good_df))
  )
  expect_error(collapse_shares(good_df[, c("shares", "id")]), 
               ".*One or more expected names of 'id' and/or 'name' missing in x")
  expect_error(collapse_shares(good_df[, c("shares", "name")]), 
               ".*One or more expected names of 'id' and/or 'name' missing in x")
})

test_that("A return without 'shares' raises an error", {
  expect_error(collapse_shares(test_df[, setdiff(names(test_df), "shares")]), 
               ".*'shares' not found in.*"
  )
})

test_that("A multi-row return with a single shared segment returns a single-row df with 3 fields", {
  out <- collapse_shares(test_df)
  expect_is(out, "data.frame")
  expect_length(nrow(out), 1L)
  expect_length(names(out),3L)
})

# Note that this scenario is unlikely unless user intentionally does something 
# similar to this test
test_that("A return with 'shares' that are all zero-row returns NA_character_", {
  expect_identical(collapse_shares(test_df[1:4, ]), NA_character_)
})
