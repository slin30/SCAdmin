context("shares")

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

test_that("a basic sharelist is created properly", {
  expect_equal(make_sharelist("user", "first_initial.last_name"), 
               basic_sharelist.chr)
})

test_that("a sharelist can be created with unboxed chr inputs", {
  expect_equal(make_sharelist(unbox("user"), unbox("first_initial.last_name")), 
               basic_sharelist.chr)
})

test_that("valid inputs of length 10 return length 10 sharelists", {
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


# chk_shares --------------------------------------------------------------

test_that("the output from a single sharelist is correct", {
  expect_true(
    SCAdmin:::chk_shares(make_sharelist("group", "w.zhang"))
  )
})

test_that("the output from a multi-length sharelist is correct", {
  expect_true(
    SCAdmin:::chk_shares(make_sharelist("group", LETTERS))
  )
})

test_that("Scalar chr inputs works with chk_shares", {
  expect_true(SCAdmin:::chk_shares(multi_sharelist.scl)
  )
})

test_that("An nested list input of length 1 shares throws an error with chk_shares", {
  expect_error(SCAdmin:::chk_shares(list(basic_sharelist.chr),
                                    "Unexpected input structure.*")
  )
})
 
test_that("An nested list input of length >1 shares throws an error with chk_shares", {
  expect_error(SCAdmin:::chk_shares(list(multi_sharelist.chr),
                                    "Unexpected input structure.*")
  )
})

test_that("A vector of length >1 for a field fails", {
  expect_error(SCAdmin:::chk_shares(list(list(type = c("group", "user"), name = LETTERS[1:2]))))
})

