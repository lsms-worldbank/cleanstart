df_in <- data.frame(
    varname = c(
        "a",
        "b"
    ),
    answer_text_1 = c(
        "First", 
        NA
    ),
    answer_text_2 = c(
        "Second", 
        NA
    ),
    answer_text_3 = c(
        "Third", 
        NA
    ),
    answer_value_1 = c(
        1, 
        NA
    ),
    answer_value_2 = c(
        2, 
        NA
    ),
    answer_value_3 = c(
        3, 
        NA
    )
)

answer_options_filled <- glue::glue(
    "* ANSWERS : ",
    "*\t1\tFirst",
    "*\t2\tSecond",
    "*\t3\tThird",
    .sep = "\n"
)

answer_options_empty <- "* ANSWERS: N/A"

testthat::test_that("Writes answer options", {
    testthat::expect_equal(
        df_in %>%
            dplyr::filter(varname == "a") %>%
            write_answer_options(),
        answer_options_filled
    )
})

testthat::test_that("Writes empty block with comment", {
    testthat::expect_equal(
        df_in %>%
            dplyr::filter(varname == "b") %>%
            write_answer_options(),
        answer_options_empty
    )
})
