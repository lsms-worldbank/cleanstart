qnr_df_to_sanitize <- data.frame(
    varname = c("a", "b", "c"),
    label = c(
        'In the <font color="blue">7 days</font>, what did you eat?',
        'How much did you consume of <font color = "blue">%rostertitle%?</font><br><br>QUANTITY',
        'This is <big><i>important</i></big>'
    ),
    validation_message_1 = c(
        'In the <font color="blue">7 days</font>, what did you eat?',
        'How much did you consume of <font color = "blue">%rostertitle%?</font><br><br>QUANTITY',
        'This is <big><i>important</i></big>'
    ),
    validation_message_2 = c(
        NA,
        'How much did you consume of <font color = "blue">%rostertitle%?</font><br><br>UNIT',
        'This is <big><i>big</i></big>'
    )
)

testthat::test_that("Removes HTML tags from label", {
    testthat::expect_equal(
        santized_label <- qnr_df_to_sanitize %>%
            sanitize_text() %>%
            dplyr::pull(label),
        c(
            'In the 7 days, what did you eat?',
            'How much did you consume of %rostertitle%?QUANTITY',
            'This is important'
        )
    )

})

testthat::test_that("Removes HTML tags from all validation messages", {
    testthat::expect_equal(
        sanitized_messages <- qnr_df_to_sanitize %>%
            sanitize_text() %>%
            dplyr::select(dplyr::starts_with("validation_message")),
        data.frame(
            validation_message_1 = c(
                'In the 7 days, what did you eat?',
                'How much did you consume of %rostertitle%?QUANTITY',
                'This is important'
            ),
            validation_message_2 = c(
                NA,
                'How much did you consume of %rostertitle%?UNIT',
                'This is big'
            )
        )
    )
})
