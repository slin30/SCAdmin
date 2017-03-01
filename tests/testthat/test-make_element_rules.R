context("make_element_rules")

# Reference data ----------------------------------------------------------

el <- "element_1"
op <- "equals"

ref.dual_el <- list(
  structure(list(element = structure("element_1", 
                                     class = c("scalar", "character")), 
                 operator = structure("equals", class = c("scalar", "character")), 
                 value = structure("A", class = c("scalar", "character"))), 
            .Names = c("element", "operator", "value")), 
  structure(list(element = structure("element_2", 
                                     class = c("scalar", "character")), 
                 operator = structure("equals", class = c("scalar", "character")), 
                 value = structure("A", class = c("scalar", "character"))), 
            .Names = c("element", "operator", "value"))
)

ref.dual_elRule <- list(
  structure(list(element = structure("element_1", 
                                     class = c("scalar", "character")), 
                 operator = structure("equals", class = c("scalar", "character")), 
                 value = structure("A", class = c("scalar", "character"))), 
            .Names = c("element", "operator", "value")), 
  structure(list(element = structure("element_2", 
                                     class = c("scalar", "character")), 
                 operator = structure("equals", class = c("scalar", "character")), 
                 value = structure("B", class = c("scalar", "character"))), 
            .Names = c("element", "operator", "value"))
)

ref.dual_elOp <- list(
  structure(list(element = structure("element_1", 
                                     class = c("scalar", "character")), 
                 operator = structure("equals", class = c("scalar", "character")), 
                 value = structure("A", class = c("scalar", "character"))), 
            .Names = c("element", "operator", "value")), 
  structure(list(element = structure("element_2", 
                                     class = c("scalar", "character")), 
                 operator = structure("contains", class = c("scalar", "character")), 
                 value = structure("A", class = c("scalar", "character"))), 
            .Names = c("element", "operator", "value"))
)


# Tests -------------------------------------------------------------------

test_that("Duplicated rules return a message", {
  expect_message(
    make_element_rules(el, op, rules = c(LETTERS, LETTERS[1:2]))
  )
})

test_that("dedupe arg works", {
  expect_silent(
    make_element_rules(el, op, rules = c(LETTERS, LETTERS[1:2]), dedupe = TRUE)
  )
})

test_that("two elements and one operator and rule works, without classification", {
  expect_equal(
    make_element_rules(c(el, "element_2"), op, LETTERS[1]), 
    ref.dual_el
  )
})

test_that("two elements, one operator, and two rules works, without classification", {
  expect_equal(
    make_element_rules(c(el, "element_2"), op, LETTERS[1:2]), 
    ref.dual_elRule
  )
})

test_that("two elements, two operators, and one rules works, without classification", {
  expect_equal(
    make_element_rules(c(el, "element_2"), c(op, "contains"), LETTERS[1]), 
    ref.dual_elOp
  )
})

test_that("When classification is present, element must be length 1", {
  expect_error(
    make_element_rules(c(el, "element_2"), c(op, "contains"), LETTERS[1], classification = "class")
  )
})

test_that("A vector of rules of length n returns a result of length n", {
  expect_length(
    make_element_rules(el, op, sample(LETTERS, 50, replace = TRUE)), 
    50L
  )
})

test_that("trim happens before unique, when both TRUE", {
  expect_length(
    make_element_rules(el, op, c(" A ", LETTERS[1:10], "A"), 
                       trim = TRUE, dedupe = TRUE), 
    10L
  )
})

test_that("trim works independently of unique", {
  expect_length(
    make_element_rules(el, op, c(" A ", LETTERS[1:10], "B"), 
                       trim = FALSE, dedupe = TRUE), 
    11L
  )
})

make_element_rules(el, op, c(" A ", LETTERS[1:10], "B"), 
                   trim = FALSE, dedupe = TRUE)
