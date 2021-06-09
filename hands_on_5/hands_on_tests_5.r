library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg1$coefficients[1], 3)), "c4dd3467a9e6cad923e572e51c276ddb")
  })
  print("Success!")
}


test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg2$coefficients[1], 3)), "77d788e26bd18c76f971771f19e3a951")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg3A$coefficients[1], 3)), "696d20daeb55f69298ba771ec0d235e8")
  })
  print("Success!")
}


test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg3M$coefficients[1], 3)), "c0e268f563b2b6e10c387ac2eb8186d5")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg3F$coefficients[1], 3)), "3288f93aa819cde10f97a9609acb0115")
  })
  print("Success!")
}


test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg4$coefficients[1], 3)), "c0e268f563b2b6e10c387ac2eb8186d5")
  })
  print("Success!")
}