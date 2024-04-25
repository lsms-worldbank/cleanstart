#' Container for Shiny usage
#' 
#' @field app_dir Character, atomic. Path to Shiny app's user data directory
#' @field prepared_qnr_df Data frame. Questionnaire metadata processed to
#' construct a sanitized variable label, create detailed question types,
#' and convert enablement and validation expressions to Stata.
#' @field vars_in_qnr Character vector. Compilation of questions and variables
#' found in questionnaire JSON file. Follows format: "{varname} : {label}"
#' @field q_var_start Character. Variable name for start of selection range.
#' @field q_var_end Character. Variable name for end of selection range.
#' @field rc_expr_target Character. Rowcode expressions to find in expressions.
#' @field rc_expr_replacement. Character. Rowcode replacement value.
#' @field rt_lbl_target. Character. Row title expression to find in label.
#' @field rt_lbl_replacement Character. Row title replacement value.
#' @field oth_var_pattern Character. Regex to identify "other (specify)" vars.
#' @field show_vars Character. Space-delimited list of variables to show as
#' context for `check_if_miss` and `validate`.
#' @field vars_for_file Character vector. Names of variables selected for 
#' cleaning file to be created.
#' @field df_for_file Data frame. Metadata for creating Stata file.
r6 = R6::R6Class(
  classname = "r6",
  public = list(

    # ==========================================================================
    # Fields
    # ==========================================================================

    app_dir = NULL,
    prepared_qnr_df = NULL,
    vars_in_qnr = NULL,
    q_var_start = NULL,
    q_var_end = NULL,
    rc_expr_target = NULL,
    rc_expr_replacement = NULL,
    rt_lbl_target = NULL,
    rt_lbl_replacement = NULL,
    oth_var_pattern = NULL,
    show_vars = NULL,
    vars_for_file = NULL,
    df_for_file = NULL,

    # ==========================================================================
    # Methods
    # ==========================================================================

    #' Read past R6 values from disk
    #' 
    #' @description
    #' Perform the following tasks:
    #' 
    #' - Read RDS on disk
    #' - Populate R6 fields with values from RDS
    #' 
    #' @param path Character. Path to the RDS file containing R6 values.
    read = function(path = fs::path(self[["app_dir"]], "saved_r6.rds")) {

      # read setup file from disk
      input_file <- readRDS(path)

      # collect names of fields in setup file
      fields <- names(input_file)

      # populate the R6 object with the corresponding setup file value
      # data frame fields need to be extracted from a list
      # "scalar" fields can be extracted directly
      for (field in fields) {
        field_type <- typeof(input_file[[field]])
        if (field_type == "list") {
          self[[field]] <- input_file[[field]][[1]]
        } else {
          self[[field]] <- input_file[[field]]
        }
      }

    },

    #' Write R6 to disk as RDS file
    #' 
    #' @description
    #' Write all R6 fields to a single RDS file, from which they can be 
    #' "restored" with the `read()` method above
    #' 
    #' @param Character. Path where RDS files should be written
    #' 
    #' @noRd
    write = function(path = fs::path(self[["app_dir"]], "saved_r6.rds")) {

      # data frame fields
      df_fields <- c(
        "prepared_qnr_df", "vars_in_qnr", "vars_for_file", "df_for_file"
      )

      # "scalar" fields
      # introspect to obtain vector fields and methods
      fields <- names(self)

      # remove system components and methods
      fields <- fields[
        ! fields %in% c(
          # system components
          ".__enclos_env__", "clone",
          # methods
          "write", "update", "read",
          # omitting data frames
          df_fields
        )
      ]

      # put fields in data frame
      # create empty 1-row data frame
      df <- tibble::tibble(.rows = 1)

      # iteratively populate it with the value of all non-df fields
      for (field in fields) {
          df[[field]] <- self[[field]]
      }

      # put data frames in data frame
      for (df_field in df_fields) {
        df[[df_field]] <- list(self[[df_field]])
      }

      # write data frame to disk
      saveRDS(
        object = df,
        file = path
      )

    }

  )

)
