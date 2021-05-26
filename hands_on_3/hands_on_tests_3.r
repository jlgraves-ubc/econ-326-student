library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer1), "0acb97bbfd2fa2133d6057dbdcab2910")
  })
  print("Success!")
}


test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer2), "046b366d6fdb4a722fb0e30c00e2910e")
  })
  print("Success!")
}

test_3 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg_LESS$coefficients[2], 2)), "285b74f54658c34c1084a513375afc29")
  })
  print("Success!")
}


test_4 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg_HS$coefficients[2], 2)), "6daf2ec473e5b3262d4f13aa1efa6faf")
  })
  print("Success!")
}

test_5 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg_SC$coefficients[2], 2)), "f84d62f49df429fab57e40f81fee0393")
  })
  print("Success!")
}


test_6 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg_BACH$coefficients[2], 2)), "df57fae07fdacdfb8f055e9eed7e6fef")
  })
  print("Success!")
}

test_7 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg_GRAD$coefficients[2], 2)), "854270bbc3cf342a422f29a6edecc493")
  })
  print("Success!")
}


test_8 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg2$coefficients[2], 2)), "b561beb775d931d6917773e2becdc20e")
  })
  print("Success!")
}

test_9 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg3$coefficients[2], 2)), "4cebe999d1ac30bf82c8a70c2a225d49")
  })
  print("Success!")
}


test_10 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg5$coefficients[2], 2)), "bb26a49ddc5b8a6b8396b14cbba81a75")
  })
  print("Success!")
}

test_11 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(round(reg6$coefficients[2], 2)), "14b4e5c6d8fe58b02104569b5940b378")
  })
  print("Success!")
}