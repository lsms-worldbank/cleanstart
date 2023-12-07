df_in <- data.frame(
    varname = c(
        "a",
        "b",
        "c"
    ),
    type = c(
        "SingleQuestion",
        "SingleQuestion",
        "TextQuestion"
    ),
    type_detailed = c(
        "Single-select",
        "Single-select",
        "Text"
    ),
    label = c(
        "Variable a",
        "Variable b",
        "Variable c"
    ),
    show_vars = c(
        "hhid ea_id",
        "hhid ea_id",
        "hhid ea_id"
    ),
    oth_var_pattern = c(
        "_oth$",
        "_oth$",
        "_oth$"
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
    severity_2 = c(
        0,
        NA,
        NA
    ),
    answer_text_1 = c(
        "Yes",
        "Oui",
        NA
    ),
    answer_value_1 = c(
        1,
        1,
        NA
    ),
    answer_text_2 = c(
        "No",
        "Non",
        NA
    ),
    answer_value_2 = c(
        2,
        2,
        NA
    ),
    answer_text_3 = c(
        "Maybe",
        NA,
        NA
    ),
    answer_value_3 = c(
        3,
        NA,
        NA
    )
)

testthat::test_that("Writes block that only checks for missing", {
    testthat::expect_equal(
        write_var_block(
            qnr_df = df_in,
            var = "c"
        ),
        glue::glue(
            "* ----------------------------------------------------------------------------",
            "* VARIABLE : c ",
            "* LABEL : Variable c",
            "* TYPE : Text",
            "* ANSWERS: N/A",
            "* ----------------------------------------------------------------------------",
            "",
            "* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ",
            "* Look for missing values",
            "* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ",
            "",
            "check_if_miss c if (consent == 1), show_vars(hhid ea_id)",
            "",
            "",
            "",
            "",
            "",
            .sep = "\n"
        )
    )
})

testthat::test_that("Writes block with multiple validations", {
    testthat::expect_equal(
        write_var_block(
            qnr_df = df_in,
            var = "a"
        ),
        glue::glue(
            "* ----------------------------------------------------------------------------",
            "* VARIABLE : a ",
            "* LABEL : Variable a",
            "* TYPE : Single-select",
            "* ANSWERS : ",
            "*\t1\tYes",
            "*\t2\tNo",
            "*\t3\tMaybe",
            "* ----------------------------------------------------------------------------",
            "",
            "* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ",
            "* Look for missing values",
            "* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ",
            "",
            "check_if_miss a if (consent == 1), show_vars(hhid ea_id)",
            "",
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
            "",
            "",
            "",
            .sep = "\n"
        )
    )
})
