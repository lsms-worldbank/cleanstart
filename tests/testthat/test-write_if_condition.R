testthat::test_that("Writes empty string if `condition_expression` NA", {
    testthat::expect_equal(
        write_if_condition(NA),
        ""
    )
})

testthat::test_that("Writes empty string if `condition_expression` empty", {
    testthat::expect_equal(
        write_if_condition(""),
        ""
    )
})

testthat::test_that("Writes condition if  not-NA/empty", {
    testthat::expect_equal(
        write_if_condition("var1 > 1 & !mi(var2)"),
        "if (var1 > 1 & !mi(var2))"
    )
})
