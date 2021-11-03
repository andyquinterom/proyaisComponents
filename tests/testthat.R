library(testthat)
library(proyaisComponents)

test_dir(
  "tests/modules",
  env = shiny::loadSupport("tests/")
)

#test_check("proyaisComponents")


