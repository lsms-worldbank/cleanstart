testthat::test_that("Creates label from question from question text", {

    qnr_df_in <- data.frame(
        varname = c("a", "b", "c"),
        question_text = c("question text 1", "question text 2", "question text 3"),
        variable_label = c(NA, "", "label")
    )

    testthat::expect_equal(
        labels_out <- qnr_df_in %>%
            construct_label() %>%
            dplyr::pull(label),
        c("question text 1", "question text 2", "label")
    )

})
