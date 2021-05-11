library(testthat)
library(digest)

test_1 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer1), "dbc09cba9fe2583fb01d63c70e1555a8")
  })
  print("Success!")
}

test_2 <- function() {
  test_that("Solution is incorrect", {
    expect_equal(digest(answer2), "475bf9280aab63a82af60791302736f6")
  })
  print("Success!")
}

test_3 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(answer3), "1dc68e15f3e33c850d6c874f6d286358")
    })
    print("Success!")
}

test_4 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(answer4), "b09db48ab3bbf872355973edd9312f10")
    })
    print("Success!")
}

test_5 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(answer5), "c7591e6f63d3e7b35f09caf9b4461917")
    })
    print("Success!")
}

test_6 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(educ_graph), "d5c3d526ee72926c4cf7a7e9f254d654")
    })
    print("Success!")
}

test_7 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(answer7), "3cb161b4ac852c9b599cf727e5d0c9e1")
    })
    print("Success!")
}

test_8 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(educ_graph2), "e0d175be987496828168c55f3b6b89fa")
    })
    print("Success!")
}

test_9 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(ths), "1e3e2e32e58738d7f466291e96b4c004")
    })
    print("Success!")
}

test_10 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(tbach), "5d6e280eed2fa09dd089d394917d0524")
    })
    print("Success!")
}

test_11 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(retHSI), "638517ebe809bcc28e3d35cf1ae17e41")
    })
    print("Success!")
}

test_12 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(retHSNI), "3247ed1a20124752e214c7d014be614c")
    })
    print("Success!")
}

test_13 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(retBachI), "8198278c5857f929281b1d648a6075e5")
    })
    print("Success!")
}

test_14 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(retBachNI), "f4f4cc759de5a213c24091cd80854839")
    })
    print("Success!")
}

test_15 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(tmen), "5823028a2f6a26cfe63e6e8da2c561d5")
    })
    print("Success!")
}

test_16 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(hist_men), "9ac2eeafc87a41bcb18d1689ef41e004")
    })
    print("Success!")
}

test_17 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(tmenhs), "fd20f3f5b8e3ec4ce40f5aacb0a89a1b")
    })
    print("Success!")
}

test_18 <- function()  {
    test_that("Solution is incorrect",  {
        expect_equal(digest(tmenbach), "055077f17ff8a345b0cacff599e5bb73")
    })
    print("Success!")
}