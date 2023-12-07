test_qnr_df <- data.frame(
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
    question_text = c(
        'Variable <font color = "blue">a</font>',
        "Boo",
        "Boo"
    ),
    variable_label = c(
        NA,
        "Variable b",
        "Variable c"
    ),
    linked_to_roster_id = c(
        NA,
        NA,
        NA
    ),
    linked_to_question_id = c(
        NA,
        NA,
        NA
    ),
    is_integer = c(
        NA,
        NA,
        NA
    ),
    num_decimal_places = c(
        NA,
        NA,
        NA
    ),
    mask = c(
        NA,
        NA,
        FALSE
    ),
    yes_no_view = c(
        NA,
        NA,
        NA
    ),
    is_timestamp = c(
        NA,
        NA,
        NA
    ),
    condition_expression = c(
        "consent.InList(1, 2)",
        "consent.InList(1, 2)",
        "consent.InList(1, 2)"
    ),
    validation_expression_1 = c(
        "self > 1",
        "self > 1",
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
        "a.InList(1, 2)",
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


mod_qnr_df <- test_qnr_df %>%
    prepare_metadata() %>%
    prepare_for_vars(
        vars = c("a", "b"),
        oth_var_pattern = "_oth$",
        show_vars = "hhid ea_id"
    )

testthat::test_that("Test that write block works", {

    testthat::expect_equal(
        write_var_block(
            qnr_df = mod_qnr_df,
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
            "check_if_miss a if (inlist(consent, 1, 2)), show_vars(hhid ea_id)",
            "",
            "* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ",
            "* Validate responses",
            "* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ",
            "",
            "/*",
            "* VALIDATION 1 : First validation for a",
            "validate a > 1 if (inlist(consent, 1, 2)), ///",
            '    msg("First validation for a")',
            "*/",
            "",
            "/*",
            "* VALIDATION 2 : Second validation for a",
            "validate inlist(a, 1, 2) if (inlist(consent, 1, 2)), ///",
            '    msg("Second validation for a")',
            "*/",
            "",
            "",
            "",
            .sep = "\n"
        )
    )
})
