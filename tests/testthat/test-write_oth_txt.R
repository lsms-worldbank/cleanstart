oth_right  <- list(
    varname = "var1_oth",
    type = "Text",
    oth_var_pattern = "_oth$",
    if_condition = "if (consent == 1)"
)

oth_not_type <- list(
    varname = "var1_oth",
    type = "Numeric",
    oth_var_pattern = "_oth$",
    if_condition = "if (consent == 1)"
)

oth_not_var <- list(
    varname = "var1",
    type = "Text",
    oth_var_pattern = "_oth$",
    if_condition = "if (consent == 1)"
)

testthat::test_that("Write filled template if follows pattern", {
    testthat::expect_equal(
        write_oth_val_txt(
            varname = oth_right$varname,
            type = oth_right$type,
            oth_var_pattern = oth_right$oth_var_pattern,
            if_condition = oth_right$if_condition
        ),
        glue::glue(
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            "* Look for opportunities to recode 'other'",
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',
            '',
            '* count',
            'qui: count if (consent == 1)',
            'local n_oth = r(N)',
            "di as result 'Variable -var1_oth- has `n_oth' other values'",
            "",
            '* identify opportunities to recode',
            'list interview__id var1_oth if (consent == 1)',
            .sep = "\n"
        )
    )
})

testthat::test_that("Write empty character if wrong type", {
    testthat::expect_equal(
        write_oth_val_txt(
            varname = oth_not_type$varname,
            type = oth_not_type$type,
            oth_var_pattern = oth_not_type$oth_var_pattern,
            if_condition = oth_not_type$if_condition
        ),
        ""
    )
})


testthat::test_that("Write empty character if doesn't follows pattern", {
    testthat::expect_equal(
        write_oth_val_txt(
            varname = oth_not_var$varname,
            type = oth_not_var$type,
            oth_var_pattern = oth_not_var$oth_var_pattern,
            if_condition = oth_not_var$if_condition
        ),
        ""
    )
})
