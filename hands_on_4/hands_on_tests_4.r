library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg1$coefficients[1], 3)), "56f04106b0d09f24d95010b891573eea")
  })
  print("Success!")
}


test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg2$coefficients[1], 3)), "a4dd2bedaafa1f6b52eb1e6b4820fbde")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg3$coefficients[1], 3)), "f02741b40f9113aeb5a5e87fb9b2ab23")
  })
  print("Success!")
}


test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg4$coefficients[1], 3)), "f02741b40f9113aeb5a5e87fb9b2ab23")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg5$residuals[1], 3)), "bbfd349bb38017dca76e637fb2dc5373")
  })
  print("Success!")
}


test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(census_data$resid[1], 3)), "bbfd349bb38017dca76e637fb2dc5373")
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(WT$coefficients[1], 2)), "0da67aa7287597c3912d21e461eb1e44")
  })
  print("Success!")
}


test_8 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(BPstat,1)), "b5063b59d220cc2a6d8e7a6a56eb9568")
  })
  print("Success!")
}
