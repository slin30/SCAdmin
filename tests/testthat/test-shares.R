context("shares")


# Reference data ----------------------------------------------------------

pos.basic_sharelist <- list(
  list(type = unbox("user"), 
       name = unbox("first_initial.last_name"))
)


# make_sharelist ----------------------------------------------------------

test_that("most basic case works", {
  expect_equal(make_sharelist("user", "first_initial.last_name"), 
               pos.basic_sharelist)
})


test_that("valid scalar input works", {
  expect_equal(make_sharelist(unbox("user"), unbox("first_initial.last_name")), 
               pos.basic_sharelist)
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

