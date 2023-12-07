df_in <- data.frame(
    varname = c(
        "a",
        "b",
        "c"
    ),
    condition_expression = c(
        "consent == 1",
        "consent == 1",
        "consent == 1"
    ),
    validation_expression_1 = c(
        "var2a > 1",
        "var2b > 1",
        NA
    ),
    validation_message_1 = c(
        "First validation for a",
        "First validation for b",
        NA
    ),
    # 1 = warning, 0 = error
    severity_1 = c(
        0,
        0,
        NA
    ),
    validation_expression_2 = c(
        "inlist(a, 1, 2)",
        NA,
        NA
    ),
    validation_message_2 = c(
        "Second validation for a",
        NA,
        NA
    ),
    # 1 = warning, 0 = error
    severity_2 = c(
        0,
        NA,
        NA
    )
)

testthat::test_that("Writes single validation", {
    testthat::expect_equal(
        df_in %>%
            dplyr::filter(varname == "b") %>%
            write_validation_txt(),
        glue::glue(
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            '* Validate responses',
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            '',
            '/*',
            '* VALIDATION 1 : First validation for b',
            'validate var2b > 1 if (consent == 1), ///',
            '    msg("First validation for b")',
            '*/',
            .sep = "\n"
        )
    )
})

testthat::test_that("Writes multiple validations", {
    testthat::expect_equal(
        df_in %>%
            dplyr::filter(varname == "a") %>%
            write_validation_txt(),
        glue::glue(
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            '* Validate responses',
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            '',
            '/*',
            '* VALIDATION 1 : First validation for a',
            'validate var2a > 1 if (consent == 1), ///',
            '    msg("First validation for a")',
            '*/',
            '',
            '/*',
            '* VALIDATION 2 : Second validation for a',
            'validate inlist(a, 1, 2) if (consent == 1), ///',
            '    msg("Second validation for a")',
            '*/',
            '',
            .sep = "\n"
        )
    )

})

testthat::test_that("Writes empty string if no validations", {
    testthat::expect_equal(
        df_in %>%
            dplyr::filter(varname == "c") %>%
            write_validation_txt(),
        ""
    )
})
