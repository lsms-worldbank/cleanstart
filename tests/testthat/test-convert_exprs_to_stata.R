df_suso_code <- data.frame(
    varname = c(
        "a", "b", "c"
    ),
    condition_expression = c(
        "self > 1",
        "@rowcode.InList(1, 2)",
        "!IsAnswered(var1) && var2.ContainsAny(1, 2, 3)"
    )
)

exprs_expected <- c(
    "a > 1",
    "inlist(@rowcode, 1, 2)",
    "mi(var1) & (var2__1 == 1 | var2__2 == 1 | var2__3 == 1)"
)

testthat::test_that("Converts code from SuSo to Stata", {
    testthat::expect_equal(
        df_converted <- df_suso_code %>%
            dplyr::mutate(
                dplyr::across(
                    .cols = dplyr::matches("_expression"),
                    .fns = ~ convert_exprs_to_stata(
                        suso_expr = .x,
                        varname = varname
                    )
                )
            ) %>%
            dplyr::pull(condition_expression),
        exprs_expected
    )
})
