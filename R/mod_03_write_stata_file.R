#' 03_write_stata_file UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList h3 textInput actionButton downloadButton
#' @importFrom bslib accordion_panel accordion
#' @importFrom fontawesome fa
mod_03_write_stata_file_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::accordion_panel(
      title = "Create Stata .do file",
      icon = fontawesome::fa(name = "laptop-code"),
      bslib::accordion(
        id = ns("file_panels"),
        open = "File",
        bslib::accordion_panel(
          title = "Settings (optional)",
          icon = fontawesome::fa(name = "gear"),
          shiny::h3("Row code expression"),
          shiny::textInput(
            inputId = ns("rc_expr_target"),
            label = "Used in SuSo",
            value = "@rowcode"
          ),
          shiny::textInput(
            inputId = ns("rc_expr_replacement"),
            label = "Used in Stata",
            placeholder = "Some roster ID variable like hhmembers__id"
          ),
          shiny::h3("Text substitution"),
          shiny::textInput(
            inputId = ns("rt_lbl_target"),
            label = "Used in SuSo",
            value = "%rostertitle%"
          ),
          shiny::textInput(
            inputId = ns("rt_lbl_replacement"),
            label = "Used in Stata",
            placeholder = "Some placeholder like [NAME] or [ITEM]"
          ),
          shiny::h3('"Other" variables'),
          shiny::textInput(
            inputId = ns("oth_var_pattern"),
            label = 'Pattern to identify "other (specify)" variables',
            value = "_oth"
          ),
          shiny::h3("Contextual variables"),
          shiny::textInput(
            inputId = ns("show_vars"),
            label = paste0(
              "Variables to show for context ", 
              "(e.g., when checking for missing, validating, etc.)"
            ),
            placeholder = "interview__id"
          ),
          shiny::actionButton(
            inputId = ns("save_file_params"),
            label = "Save",
            icon = shiny::icon("floppy-disk")
          )
        ),
        bslib::accordion_panel(
          title = "File",
          icon = fontawesome::fa(name = "file-code"),
          shiny::h2("File"),
          shiny::textInput(
            inputId = ns("file_name"),
            label = "File name"
          ),
          shiny::actionButton(
            inputId  = ns("create_file"),
            label = "Create",
            icon = shiny::icon(name = "wand-magic-sparkles")
          ),
          shiny::downloadButton(
            outputId = ns("download_file"),
            label = "Download",
            icon = shiny::icon(name = "download")
          )
        )

      )
    )
 
  )
}
    
#' 03_write_stata_file Server Functions
#'
#' @noRd 
#' 
#' @importFrom shinyvalidate InputValidator sv_required
#' @importFrom bslib accordion_panel_close accordion_panel_open
#' @importFrom gargoyle trigger
#' @importFrom shiny observeEvent downloadHandler
#' @importFrom dplyr `%>%` if_else
#' @importFrom purrr map_chr
#' @importFrom stringr str_detect
mod_03_write_stata_file_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # define validation rules
    param_iv <- shinyvalidate::InputValidator$new()
    param_iv$add_rule("rc_expr_target", shinyvalidate::sv_required())
    param_iv$add_rule("rc_expr_replacement", shinyvalidate::sv_required())
    param_iv$add_rule("rt_lbl_target", shinyvalidate::sv_required())
    param_iv$add_rule("rt_lbl_replacement", shinyvalidate::sv_required())
    param_iv$add_rule("oth_var_pattern", shinyvalidate::sv_required())
    param_iv$add_rule("show_vars", shinyvalidate::sv_required())

    shiny::observeEvent(input$save_file_params, {

      # run input validation rules
      param_iv$enable()

      # if input is valid, use input for logic
      if (param_iv$is_valid()) {

        # capture user inputs in R6 and write to disk
        r6$rc_expr_target <- input$rc_expr_target
        r6$rc_expr_replacement <- input$rc_expr_replacement
        r6$rt_lbl_target <- input$rt_lbl_target
        r6$rt_lbl_replacement <- input$rt_lbl_replacement
        r6$oth_var_pattern <- input$oth_var_pattern
        r6$show_vars <- input$show_vars
        r6$write()

        # manage panels
        bslib::accordion_panel_close(
          id = "file_panels",
          values = "Settings (optional)"
        )
        bslib::accordion_panel_open(id = "file_panels", values = "File")

        # send signal that user-provided parameters provided
        gargoyle::trigger("use_settings")

      }

    })

    # define validation rules
    file_iv <- shinyvalidate::InputValidator$new()
    file_iv$add_rule("file_name", shinyvalidate::sv_required())

    shiny::observeEvent(input$create_file, {

      # run input validation
      file_iv$enable()

      # if input is valid, use input for logic
      if (file_iv$is_valid()) {

        # further prepare metadata for selected variables
        # using a different approach depending on user inputs
        # if provided parameters in settings accordion pannel, use them
        # otherwise, use harmless defaults
        # NOTE: session$userData[["trigger_name"]] is where gargoyle puts info
        if (session$userData[["use_settings"]]() >= 1) {

          df_for_file <- r6$prepared_qnr_df %>%
            prepare_for_vars(
              vars = r6$vars_for_file,
              rc_expr_target = input$rc_expr_target,
              rc_expr_replacement = input$rc_expr_replacement,
              rt_lbl_target = input$rt_lbl_target,
              rt_lbl_replacement = input$rt_lbl_replacement,
              oth_var_pattern = input$oth_var_pattern,
              show_vars = input$show_vars
            )

        } else {

          df_for_file <- r6$prepared_qnr_df %>%
            prepare_for_vars(
              vars = r6$vars_for_file,
              rc_expr_target = "@rowcode",
              rc_expr_replacement = "placeholder_var__id",
              rt_lbl_target = "@rostertitle",
              rt_lbl_replacement = "[NAME]",
              oth_var_pattern = "_oth",
              show_vars = "intervew__id"
            )

        }

        # construct Stata script by writing a block for each selected variable
        stata_file_content <- purrr::map_chr(
          .x = r6$vars_for_file,
          .f = ~ write_var_block(
            qnr_df = df_for_file,
            var = .x
          )
        )

        # define how file name is set
        add_do_extension <- function(file_name) {
          dplyr::if_else(
            condition = stringr::str_detect(
              string = file_name,
              pattern = "\\.do$"
            ),
            true = file_name,
            false = paste0(file_name, ".do")
          )
        }

        # write Stata .do file to disk in app directory
        output$download_file <- shiny::downloadHandler(
          filename = function() {
            add_do_extension(input$file_name)
          },
          content = function(file) {
            writeLines(
              text = stata_file_content,
              con = file
            )
          }
        )

      }

    })

  })
}
    
## To be copied in the UI
# mod_03_write_stata_file_ui("03_write_stata_file_1")
    
## To be copied in the server
# mod_03_write_stata_file_server("03_write_stata_file_1")
