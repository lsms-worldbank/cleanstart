non_numeric <- list(
    varname = "a",
    type = "Single-select, linked",
    if_condition = "consent == 1",
    show_vars = "hhid ea_id"
)

numeric_integer <- list(
    varname = "b",
    type = "Numeric, integer",
    if_condition = "consent == 1",
    show_vars = "hhid"
)

numeric_w_decimal <- list(
    varname = "c",
    type = "Numeric, real (with 2 decimal places)",
    if_condition = "consent == 1",
    show_vars = "ea_id"
)



testthat::test_that("Writes empty string if not numeric", {
    testthat::expect_equal(
        write_extreme_val_txt(
            varname = non_numeric$varname,
            type = non_numeric$type,
            if_condition = non_numeric$if_condition,
            show_vars = non_numeric$show_vars
        ),
        ""
    )
})

testthat::test_that("Writes templated string if integer", {
    testthat::expect_equal(
        write_extreme_val_txt(
            varname = numeric_integer$varname,
            type = numeric_integer$type,
            if_condition = numeric_integer$if_condition,
            show_vars = numeric_integer$show_vars
        ),
        glue::glue(
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            '* Identify extreme values',
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            '',
            'list_extreme_vals b consent == 1, show_vars(hhid)',
            .sep = "\n"
        )
    )
})

testthat::test_that("Writes templated string if real with decimals", {
    testthat::expect_equal(
        write_extreme_val_txt(
            varname = numeric_w_decimal$varname,
            type = numeric_w_decimal$type,
            if_condition = numeric_w_decimal$if_condition,
            show_vars = numeric_w_decimal$show_vars
        ),
        glue::glue(
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            '* Identify extreme values',
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            '',
            'list_extreme_vals c consent == 1, show_vars(ea_id)',
            .sep = "\n"
        )
    )
})
