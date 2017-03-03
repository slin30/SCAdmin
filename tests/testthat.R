library(testthat)
library(SCAdmin)

#create if not exists testthat\testdata
if(!dir.exists("./tests/testdata")) {
  dir.create("./tests/testdata")
}

test_check("SCAdmin")
