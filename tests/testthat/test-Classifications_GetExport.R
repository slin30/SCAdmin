context("Classifications_GetExport")


# Test status_returns -----------------------------------------------------

df_pos <- data.frame(
  id = c(1L, 2L),
  type = c("job_id", "file_id"),
  viewable_pages = c(0L, 5L),
  status = c("Completed", "Ready"),
  stringsAsFactors = FALSE
)

df_neg <- data.frame(
  id = c(1L, 2L),
  type = c("job_id", "file_id"),
  viewable_pages = c(0L, 0L),
  status = c("Completed", "Ready"),
  stringsAsFactors = FALSE
)

df_neg_jobid <- data.frame(
  id = c(1L, 2L),
  type = c("job_id", "file_id"),
  viewable_pages = c(1L, 0L),
  status = c("Completed", "Ready"),
  stringsAsFactors = FALSE
)


# tests -------------------------------------------------------------------

test_that("a zero-value viewable pages for type file_id returns an error", {
  expect_error(Classifications_GetExport(df_neg, dontrun = TRUE))
})
test_that("a zero-value viewable pages for type file_id returns an error, 
          even if job_id pages is > 0", {
  expect_error(Classifications_GetExport(df_neg_jobid, dontrun = TRUE))
})

test_that("a basic working status_return works", {
  expect_length(Classifications_GetExport(df_pos, dontrun = TRUE), 
                5)
})
test_that("setting all_pages to FALSE returns single-length call body", {
  expect_length(Classifications_GetExport(df_pos, all_pages = FALSE, dontrun = TRUE), 
                1)
})
test_that("page arg is ignored if all_pages is not explicitly set to FALSE", {
  expect_length(Classifications_GetExport(df_pos, page = 0:100, dontrun = TRUE), 
                5)
})

test_that("page arg of length 1 returns a single-length call", {
  expect_length(Classifications_GetExport(df_pos, all_pages = FALSE, page = 2, dontrun = TRUE), 
                1)
})
test_that("page arg of various lengths return equal-length call bodies with expected indices", {
  pages <- c(1:7L)
  tst <- lapply(pages, function(f) Classifications_GetExport(df_pos, all_pages = FALSE, page = f, dontrun = TRUE))

  reslens <- unlist(lapply(tst, function(f) as.integer(f[[1]][[2]])))
  reflens <- pages
  reflens[reflens > 5L] <- 5L
  expect_equal(reslens, reflens)
})
test_that("page arg of length > 1 returns a call of equal length", {
  pages <- 1:3L
  res <- Classifications_GetExport(df_pos, all_pages = FALSE, page = pages, dontrun = TRUE)
  expect_equal(length(pages), length(res))
})
test_that("page arg max length truncation works", {
  pages <- 6
  res <- Classifications_GetExport(df_pos, all_pages = FALSE, page = pages, dontrun = TRUE)
  expect_equal(5L, as.integer(res[[1]][[2]]))
  
})