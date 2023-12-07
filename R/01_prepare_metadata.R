#' Construct variable label
#' 
#' If variable label present in Designer, use it. Otherwise, use question text.
#' 
#' @param qnr_df Data frame. Questionnaire metadata.
#' @param question_text_var Character. Name of question text variable in `qnr_df`.
#' @param variable_label_var Character. Name of variable label in `qnr_df`.
#' 
#' @return Data frame. Questionnaire metadata with `label` column.
#' 
#' @importFrom rlang sym
#' @importFrom dplyr `%>%` if_else
construct_label <- function(
    qnr_df,
    question_text_var = "question_text",
    variable_label_var = "variable_label"
) {

    question_text <- rlang::sym(question_text_var)
    variable_label <- rlang::sym(variable_label_var)

    df <- qnr_df %>%
        dplyr::mutate(
            label = dplyr::if_else(
                condition = !is.na(!!variable_label) & (!!variable_label != ""),
                true = !!variable_label,
                false = !!question_text,
                missing = !!question_text
            )
        )

    return(df)

}

#' Sanitize variable label
#' 
#' Remove HTML tags
#' 
#' @param qnr_df Data frame. Questionnaire metadata.
#' 
#' @return Data frame. Questionnaire metadata with sanitized `label` column.
#' 
#' @importFrom rlang sym
#' @importFrom dplyr `%>%` mutate
#' @importFrom stringr str_replace_all
sanitize_text <- function(
    qnr_df
) {

    html_tag_regex <- "<.*?>"

    df <- qnr_df %>%
        # remove HTML tags
        dplyr::mutate(
            # from variable label
            label = stringr::str_replace_all(
                string = .data$label,
                pattern = html_tag_regex,
                replacement = ""
            ),
            # from validation messages
            dplyr::across(
                .cols = dplyr::matches("validation_message_"),
                .fns = ~ stringr::str_replace_all(
                    string = .x,
                    pattern = html_tag_regex,
                    replacement = ""
                )
            )
            # ,
            # TODO: move this into another function that's
            # run for the selected variables only
            # replace pipe with user-specified string
            # label = stringr::str_replace_all(
            #     string = label,
            #     pattern = "%.+?%",
            #     replacement = pipe_replacement
            # )
        )

    return(df)

}

#' Create detailed question type
#' 
#' @param qnr_df Data frame. Questionnaire metadata.
#' @param type_var Character. Name of column with data type from JSON.
#' @param mask_var Character. Name of column indicating whether text is masked.
#' @param is_integer_var Character. Name of column whether is integer.
#' @param linked_to_roster_id_var Name of column with linked roster ID.
#' @param linked_to_question_id_var Name of column with linked question.
#' @param yes_no_view_var Name of column with yes/no mode indicator.
#' @param is_timestamp_var Name of column saying whether date is timestamp.
#' 
#' @return Data frame. Questionnaire metadata with `type_detailed` column.
#' 
#' @importFrom dplyr mutate case_when
#' @importFrom rlang `.data`
#' @importFrom glue glue
create_detailed_q_type <- function(
    qnr_df,
    type_var = "type",
    mask_var = "mask",
    is_integer_var = "is_integer",
    linked_to_roster_id_var = "linked_to_roster_id",
    linked_to_question_id_var = "linked_to_question_id",
    yes_no_view_var = "yes_no_view",
    is_timestamp_var = "is_timestamp"
) {

    df <- dplyr::mutate(qnr_df,
        type_detailed = dplyr::case_when(
            # single-select
            .data[[type_var]] == "SingleQuestion" & 
                (!is.na(.data[[linked_to_roster_id_var]]) | !is.na(.data[[linked_to_question_id_var]])) 
                ~ "Single-select, linked",
            .data[[type_var]] == "SingleQuestion" ~ "Single-select",
            # numeric
            .data[[type_var]] == "NumericQuestion" & 
                .data[[is_integer_var]] == TRUE ~ "Numeric, integer",
            .data[[type_var]] == "NumericQuestion" & 
                .data[[is_integer_var]] == FALSE 
                ~ glue::glue("Numeric, real (with {num_decimal_places} decimal places)"), 
            # text
            .data[[type_var]] == "TextQuestion" & !is.na(.data[[mask_var]]) ~ "Text, with pattern",
            .data[[type_var]] == "TextQuestion" & is.na(.data[[mask_var]]) ~ "Text",
            # multi-select
            .data[[type_var]] == "MultyOptionsQuestion" & 
                .data[[yes_no_view_var]] == TRUE ~ "Multi-select, yes/no",
            .data[[type_var]] == "MultyOptionsQuestion" & 
                .data[[yes_no_view_var]] == FALSE ~ "Multi-select, checkbox",
            # date/time
            .data[[type_var]] == "DateTimeQuestion" & 
                .data[[is_timestamp_var]] == TRUE ~ "Date, timestamp",
            .data[[type_var]] == "DateTimeQuestion" & 
                .data[[is_timestamp_var]] == FALSE ~ "Date, calendar",
            # misc
            .data[[type_var]] == "MultimediaQuestion" ~ "Picture",
            .data[[type_var]] == "GpsCoordinateQuestion" ~ "GPS",
            .data[[type_var]] == "TextListQuestion" ~ "List",
            # for all other .data[[type_vars, inherit values from JSON metadata
            .data[[type_var]] == "Variable" ~ .data[[type_var]],
            TRUE ~ NA_character_
        ))

    return(df)

}

#' Convert expressions in target column from SuSo C# to Stata
#' 
#' @details Apply series of functions to translate C# to equivalent Stata
#' 
#' @param suso_expr Character vector. SuSo enablement or validation condition.
#' @param varname Character vector. Variable name to replace `self`
#' 
#' @return Data frame. Questionnaire metadata with translated expressions.
#' 
#' @importFrom dplyr `%>%`
#' @importFrom suso2stata replace_self replace_Contains_yn replace_ContainsAny 
#' replace_ContainsAll replace_InRange replace_InList replace_IsAnswered
#' replace_and replace_or replace_null replace_bool
convert_exprs_to_stata <- function(
    suso_expr,
    varname
) {

    col <- suso_expr %>%
        suso2stata::replace_self(varname = varname) %>%
        suso2stata::replace_Contains_yn() %>%
        suso2stata::replace_ContainsAny() %>%
        suso2stata::replace_ContainsAll() %>%
        suso2stata::replace_Contains() %>%
        suso2stata::replace_InRange() %>%
        suso2stata::replace_InList() %>%
        suso2stata::replace_IsAnswered() %>%
        suso2stata::replace_and() %>%
        suso2stata::replace_or() %>%
        suso2stata::replace_null() %>%
        suso2stata::replace_bool()

    return(col)

}

#' Prepare metadata
#' 
#' @param qnr_df Data frame. Questionnaire metadata.
#' 
#' @return Data frame. Modified questinnaire metadata file.
#' 
#' @importFrom dplyr `%>%` mutate across
prepare_metadata <- function(
    qnr_df
) {

    df <- qnr_df %>%
        # construct label
        construct_label() %>%
        # sanitize label
        sanitize_text() %>%
        # create more granular type
        create_detailed_q_type() %>%
        # convert SuSo expressions to Stata
        dplyr::mutate(
            dplyr::across(
                .cols = c(
                    .data$condition_expression, 
                    dplyr::matches("validation_expression_")
                ),
                .fns = ~ convert_exprs_to_stata(
                    suso_expr = .x,
                    varname = varname
                )
            )
        )

    return(df)

}
