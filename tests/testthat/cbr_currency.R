library(testthat)

test_that("cbr_currency() works", expect_equal({
  ncol(cbr_currency(currency = "R01239", from = "2024-09-01", to = "2024-09-10"))
}, {4}))
