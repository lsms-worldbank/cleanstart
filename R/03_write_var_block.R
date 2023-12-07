#' Write code block for a variable
#' 
#' @param qnr_df Data frame. Modified questionnaire metadata for 
#' a group of variables.
#' @param var Character. Name of a variable whose code block to write.
#' 
#' @return Character. Code block for a variable
#' 
#' @importFrom dplyr filter pull
#' @importFrom glue glue
write_var_block <- function(
    qnr_df,
    var
) {

    # filter to attributes for variable
    var_df <- dplyr::filter(qnr_df, .data$varname == var)

    # pull attributes from df into variables
    varname     <- dplyr::pull(.data = var_df, var = varname)
    label       <- dplyr::pull(.data = var_df, var = label)
    type_detailed   <- dplyr::pull(.data = var_df, var = type_detailed)
    condition   <- dplyr::pull(.data = var_df, var = condition_expression)
    show_vars   <- dplyr::pull(.data = var_df, var = show_vars)
    oth_var_pattern <- dplyr::pull(.data = var_df, var = oth_var_pattern)

    # compose options
    options_txt <- write_answer_options(var_df = var_df)

    # compose if condition
    if_condition_txt <- write_if_condition(condition_expression = condition)

    # compose validation text
    validation_txt <- write_validation_txt(var_df = var_df)

    # compose extreme value text
    extreme_val_txt <- write_extreme_val_txt(
        type = type_detailed,
        varname = varname,
        if_condition = if_condition_txt,
        show_vars = show_vars
    )

    # compose "other" value text
    oth_val_txt <- write_oth_val_txt(
        varname = varname,
        type = type_detailed,
        oth_var_pattern = oth_var_pattern,
        if_condition = if_condition_txt
    )

    # compose block
    var_block_txt <- glue::glue(
        "* ----------------------------------------------------------------------------",
        "* VARIABLE : {varname} ",
        "* LABEL : {label}",
        "* TYPE : {type_detailed}",
        "{options_txt}",
        "* ----------------------------------------------------------------------------",
        "",
        "* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ",
        "* Look for missing values",
        "* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ",
        "",
        "check_if_miss {varname} {if_condition_txt}, show_vars({show_vars})",
        "",
        "{validation_txt}",
        "",
        "{extreme_val_txt}",
        "",
        .sep = "\n"
    )

    return(var_block_txt)

}

#' Write if-condition code component
#' 
#' @param condition_expression Character. Stata condition.
#' 
#' @return Character string. Either filled template or empty string, on
#' whether `condition_expression` is NA/empty or not.
#' 
#' @importFrom glue glue
write_if_condition <- function(
    condition_expression
) {

    if (!is.na(condition_expression) & condition_expression != "") {

        if_condition_txt <- glue::glue("if ({condition_expression})")

    } else {

        if_condition_txt <- glue::glue("")

    }

    return(if_condition_txt)

}

#' Write answer options in template
#' 
#' @param var_df Data frame. Contains single row for variable of interest.
#' 
#' @return Character. Answer option block. That looks like this:
#' ANSWERS:
#' 1    Answer 1
#' 2    Answer 2
#' N    Answer N
#' 
#' @importFrom dplyr mutate across starts_with pull select rename relocate
#' @importFrom tidyr pivot_longer everything
#' @importFrom glue glue_data glue_collapse glue
write_answer_options <- function(
    var_df
) {

    # check whether has any answer options
    has_answer_options <- var_df |>
        dplyr::mutate(
            has_answers = rowSums(
                dplyr::across(
                    .cols = dplyr::starts_with("answer_text_"), 
                    .fns = ~ !is.na(.x)
                )
            ) > 0
        ) |>
        dplyr::pull(.data$has_answers)

    if (has_answer_options == 1) {

        codes_df <-  var_df |>
            # select columns that capture answer text and values
            dplyr::select(
                dplyr::starts_with("answer_text"), 
                dplyr::starts_with("answer_value")
            ) |>
            # pivot so that option text and values occupy their own columns
            tidyr::pivot_longer(
                cols = tidyr::everything(),
                # create 1 column per each value found in pattern, named what is found in expression
                names_to = c(".value", "index"),
                # variable names: `answer_text_{n}` and `answer_value_{n}`
                names_pattern = "answer_([a-z]+)_([0-9]+)",
               # drop rows that contain only NAs--that is, 
               # empty answer_text and answer_value columns 
                values_drop_na = TRUE
            ) |>
            dplyr::rename(label = text) |>
            dplyr::relocate(label, .before = label) |>
            dplyr::select(value, label)
    }

    if (has_answer_options == 1) {

        codes_only <- codes_df |>
            glue::glue_data("*\t{value}\t{label}") |>
            glue::glue_collapse(sep = "\n")

        codes_glued <- glue::glue("* ANSWERS : \n{codes_only}")

    } else {

        codes_glued <- glue::glue("")

    }

    codes <- dplyr::if_else(
        condition = has_answer_options == 0,
        true = glue::glue("* ANSWERS: N/A"),
        false = glue::glue("{codes_glued}")
    )

    return(codes)

}

#' Write validation code block
#' 
#' @param var_df Data frame. Questionnaire metadata filtered to single 
#' variable of interest.
#' 
#' @return Character. String with templated validaiton condition. 
#' Either empty or filled.
#' 
#' @importFrom dplyr `%>%` select mutate if_else starts_with left_join pull
#' @importFrom glue glue glue_collapse
#' @importFrom tidyr pivot_longer
write_validation_txt <- function(
    var_df
) {

    conditions <- var_df %>%
        # select columns
        dplyr::select(varname, condition_expression) %>%
        dplyr::mutate(
            if_condition = dplyr::if_else(
                condition = (
                    is.na(.data$condition_expression) | 
                    .data$condition_expression == ""
                ),
                true = "",
                false = as.character(glue::glue("if ({condition_expression})"))
            )            
        ) %>%
        dplyr::select(varname, if_condition)
    
    validations <- var_df %>%
        # select variable name and components of validation
        dplyr::select(
            varname, 
            dplyr::starts_with("validation_"), dplyr::starts_with("severity_")
        ) %>%
        # pivot into long format while dropping any NA obs
        tidyr::pivot_longer(
            cols = c(dplyr::starts_with("validation_"), dplyr::starts_with("severity_")),
            names_pattern = "([a-z_]+)_([0-9]+)",
            names_to = c(".value", "id"),
            values_drop_na = TRUE, 
        ) %>%
        # add if_condition
        dplyr::left_join(conditions, by = "varname")

    if (nrow(validations) > 0) {

        validation_txt <- validations %>%
            dplyr::mutate(txt = glue::glue(
                '/*',
                '* VALIDATION {id} : {validation_message}',
                'validate {validation_expression} {if_condition}, ///',
                '    msg("{validation_message}")',
                '*/',
                .sep = "\n"
                )
            ) %>%
            dplyr::select(txt) %>%
            dplyr::pull(.data$txt) %>%
            glue::glue_collapse(sep = "\n\n")

        validation_txt <- glue::glue(
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            '* Validate responses',
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            '',
            '{validation_txt}',
            .sep = "\n"
        )

    } else if (nrow(validations) == 0) {

        validation_txt = glue::glue("")

    }

    return(validation_txt)

}

#' Write extreme values code block
#' 
#' @param type Character. Variable type string created by 
#' `create_detailed_q_type()`.
#' @param varname Character. Variable name.
#' @param if_condition Character. Stata if condition.
#' @param show_vars Character. Variables to show for context.
#' 
#' @return Character string. Either filled template or empty string, on
#' variable type in `type`.
#' 
#' @importFrom stringr str_detect
#' @importFrom glue glue
write_extreme_val_txt <- function(
    type,
    varname,
    if_condition,
    show_vars
) {

    if (stringr::str_detect(type, "Numeric")) {

        extreme_val_txt <- glue::glue(
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            '* Identify extreme values',
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            '',
            'list_extreme_vals {varname} {if_condition}, show_vars({show_vars})',
            .sep = "\n"
        )

    } else {

        extreme_val_txt <- glue::glue("")

    }

    return(extreme_val_txt)

}

#' Write other values code block
#' 
#' @param varname Character. Variable name.
#' @param type Character. Detailed variable type.
#' @param oth_var_pattern Character. (Regex) expression to identify 
#' "other (specify)" variables.
#' @param if_condition Character. Stata if condition.
#' 
#' @return Character string. Either filled template or empty string, on
#' whether variable name matches the pattern.
#' 
#' @importFrom stringr str_detect
#' @importFrom glue glue
write_oth_val_txt <- function(
    varname,
    type,
    oth_var_pattern,
    if_condition
) {

    is_text_or_string <- stringr::str_detect(type, "Text|List")
    is_oth_var <- stringr::str_detect(varname, oth_var_pattern)

    if (is_text_or_string & is_oth_var) {

        oth_val_txt <- glue::glue(
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',
            "* Look for opportunities to recode 'other'",
            '* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -',
            '',
            '* count',
            'qui: count {if_condition}',
            'local n_oth = r(N)',
            "di as result 'Variable -{varname}- has `n_oth' other values'",
            "",
            '* identify opportunities to recode',
            'list interview__id {varname} {if_condition}',
            .sep = "\n"
        )

    } else {

        oth_val_txt <- glue::glue("")

    }

    return(oth_val_txt)

}
