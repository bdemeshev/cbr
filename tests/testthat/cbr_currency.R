library(testthat)

# mass days
test_that("cbr_currency() works - mass days", expect_equal({
  ncol(cbr_currency(currency = "R01239", from = "2024-09-01", to = "2024-09-10"))
}, 4))

# not working days
test_that("cbr_currency() works 2 -  not working days", expect_equal({
  cbr_currency(currency = "R01239", from = "2024-01-01", to = "2024-01-02")
}, NA))

# working ind days
test_that("cbr_currency() works  - working ind days", expect_equal({
  nrow(cbr_currency(currency = "R01239", from = "2024-09-03", to = "2024-09-03"))
}, 1))
