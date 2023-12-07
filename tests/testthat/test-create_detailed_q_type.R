roster_id <- uuid::UUIDgenerate(n = 1)

df_types_input <- data.frame(
    varname = c(
        "single_select",
        "single_select_linked",
        "numeric_integer",
        "numeric_2decimals",
        "text",
        "text_w_pattern",
        "multi_select_yn",
        "multi_select_checkbox",
        "date_timestamp",
        "date_calendar",
        "picture",
        "gps",
        "list",
        "variable"
    ),
    type = c(
        "SingleQuestion",
        "SingleQuestion",
        "NumericQuestion",
        "NumericQuestion",
        "TextQuestion",
        "TextQuestion",
        "MultyOptionsQuestion", 
        "MultyOptionsQuestion",
        "DateTimeQuestion",
        "DateTimeQuestion",
        "MultimediaQuestion",
        "GpsCoordinateQuestion",
        "TextListQuestion",
        "Variable"
    ),
    mask = c(
        NA,
        NA,
        NA,
        NA,
        NA,
        "##",
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA
    ),
    is_integer = c(
        NA,
        NA,
        TRUE,
        FALSE,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA
    ),
    num_decimal_places = c(
        NA,
        NA,
        NA,
        2,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA
    ),
    linked_to_roster_id = c(
        NA,
        roster_id,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA
    ),
    linked_to_question_id = c(
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA
    ),
    yes_no_view = c(
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        TRUE,
        FALSE,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA
    ),
    is_timestamp = c(
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        NA,
        TRUE,
        FALSE,
        NA,
        NA,
        NA,
        NA
    )
)

df_types_expected <- df_types_input
df_types_expected$type_detailed <- c(
        "Single-select",
        "Single-select, linked",
        "Numeric, integer",
        "Numeric, real (with 2 decimal places)",
        "Text", 
        "Text, with pattern",
        "Multi-select, yes/no",
        "Multi-select, checkbox",
        "Date, timestamp",
        "Date, calendar",
        "Picture",
        "GPS",
        "List",
        "Variable"
    )

testthat::test_that("Detailed variable types correctly identified", {
    testthat::expect_equal(
        df_out <- create_detailed_q_type(qnr_df = df_types_input),
        df_types_expected
    )
})
