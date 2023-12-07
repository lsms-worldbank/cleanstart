#' Prepare the data for variables of interest
#' 
#' @param qnr_df Data frame. Variable metadata.
#' @param vars Character vector. Names of variables.
#' @param rc_expr_target Character. `rowcode` variable in expressoins (e.g., `"@rowcode"`).
#' @param rc_expr_replacement Character. Replacement for rowcode.
#' @param rt_lbl_target Character. Pipe to replace in variable label.
#' @param rt_lbl_replacement Character. Replacement for roster title.
#' @param oth_var_pattern Character. Regex for identifying 
#' "other (specify)" variables
#' @param show_vars Character. Variables to show as context.
#' 
#' @return Data frame. Questionnaire metadata prepared for grou.
#' 
#' @importFrom dplyr `%>%` filter mutate across matches
#' @importFrom suso2stata replace_rowcode_with
#' @importFrom stringr str_replace_all
prepare_for_vars <- function(
    qnr_df,
    vars,
    rc_expr_target = "@rowcode",
    rc_expr_replacement = "some__id",
    rt_lbl_target = "%rostertitle%",
    rt_lbl_replacement = "[ITEM]",
    oth_var_pattern,
    show_vars = "interview__id"
) {

    df <- qnr_df %>%
        # subset to desired variables
        dplyr::filter(.data$varname %in% vars) %>%
        # prepare
        dplyr::mutate(
            # replace rowcode in expressions
            dplyr::across(
                .cols = c(
                    condition_expression,
                    dplyr::matches("validation_expression")
                ),
                .fns = ~ suso2stata::replace_rowcode_with(
                    suso_expr = .x,
                    rowcode = rc_expr_target,
                    replacement = rc_expr_replacement
                )
            ),
            # replace rowtitle in label
            label = stringr::str_replace_all(
                string = .data$label,
                pattern = stringr::fixed(rt_lbl_target),
                replacement = rt_lbl_replacement
            ),
            # add regex to identify "other (specify)" questions
            oth_var_pattern = oth_var_pattern,
            # add variables to show for context with validations
            show_vars = show_vars
        )

    return(df)
        
}
