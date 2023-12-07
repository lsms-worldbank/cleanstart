df_in <- data.frame(
    varname = c(
        "a",
        "b",
        "c"
    ),
    condition_expression = c(
        "inlist(@rowcode, 1, 2)",
        "a > 1",
        "a > 1"
    ),
    label = c(
        "How old is %rostertitle%?",
        "Foo bar",
        "Hello world"
    )
)

df_expected <- data.frame(
    varname = c(
        "a",
        "b"
    ),
    condition_expression = c(
        "inlist(members__id, 1, 2)",
        "a > 1"
    ),
    label = c(
        "How old is [NAME]?",
        "Foo bar"
    ),
    show_vars = rep("boo", 2)
)

testthat::test_that("Filters to desired variables", {
    testthat::expect_equal(
        df_out <- df_in %>%
            prepare_for_vars(
                vars = c("a", "b"),
                rc_expr_target = "@rowcode",
                rc_expr_replacement = "members__id",
                rt_lbl_target = "%rostertitle%",
                rt_lbl_replacement = "[NAME]",
                oth_var_pattern = "_oth",
                show_vars = "boo"
            ) %>%
            dplyr::pull(varname),
        df_expected$varname
    )

})

testthat::test_that("Replaces rowcode in condition", {
    testthat::expect_equal(
        df_out <- df_in %>%
            prepare_for_vars(
                vars = c("a", "b"),
                rc_expr_target = "@rowcode",
                rc_expr_replacement = "members__id",
                rt_lbl_target = "%rostertitle%",
                rt_lbl_replacement = "[NAME]",
                oth_var_pattern = "_oth",
                show_vars = "boo"
            ) %>%
            dplyr::pull(condition_expression),
        df_expected$condition_expression
    )
})

testthat::test_that("Replaces rostertitle in `label`", {
    testthat::expect_equal(
        df_out <- df_in %>%
            prepare_for_vars(
                vars = c("a", "b"),
                rc_expr_target = "@rowcode",
                rc_expr_replacement = "members__id",
                rt_lbl_target = "%rostertitle%",
                rt_lbl_replacement = "[NAME]",
                oth_var_pattern = "_oth",
                show_vars = "boo"
            ) %>%
            dplyr::pull(label),
        df_expected$label
    )
})


testthat::test_that("Create `show_vars` column with specified content", {
    testthat::expect_equal(
        df_out <- df_in %>%
            prepare_for_vars(
                vars = c("a", "b"),
                rc_expr_target = "@rowcode",
                rc_expr_replacement = "members__id",
                rt_lbl_target = "%rostertitle%",
                rt_lbl_replacement = "[NAME]",
                oth_var_pattern = "_oth",
                show_vars = "boo"
            ) %>%
            dplyr::pull(show_vars),
        df_expected$show_vars
    )
})
