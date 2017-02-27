context("shares")

## Temp
library(SCAdmin)
library(jsonlite)
library(testthat)

# Reference data ----------------------------------------------------------

##
basic_sharelist.chr <- list(
  list(type = "user", 
       name = "first_initial.last_name")
)


basic_sharelist.scl <- rapply(basic_sharelist.chr, unbox, how = "list")

##
set.seed(1)
multi_sharelist.chr <- Map(list, type = sample(c("user", "group"), 10, replace = TRUE), 
      name = LETTERS[1:10]
)
names(multi_sharelist.chr) <- NULL

multi_sharelist.scl <- rapply(multi_sharelist.chr, unbox, how = "list")

##

# make_sharelist ----------------------------------------------------------

test_that("most basic case works", {
  expect_equal(make_sharelist("user", "first_initial.last_name"), 
               basic_sharelist.scl)
})

test_that("valid scalar input works", {
  expect_equal(make_sharelist(unbox("user"), unbox("first_initial.last_name")), 
               basic_sharelist.scl)
})

test_that("valid inputs of length 10 return length 10 output", {
  expect_length(make_sharelist(type = sample(c("user", "group"), 10, replace = TRUE), 
                                name = LETTERS[1:10]), 10)
})

test_that("An argument of 'type' of length != 1 or length(name) errors with message", {
  expect_error(make_sharelist(type = c("user", "group"), 
                              name = LETTERS[1:10]), 
               "'type' must be a vector of length 1 or the same length as 'name'")
})

test_that("An invalid value for 'type' errors with message", {
  expect_error(make_sharelist(type = c("user", "name"), 
                              name = LETTERS[1:2]), 
               "Allowed values for 'type' are \\{group,user\\}.*")
})

