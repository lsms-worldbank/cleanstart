#' 02_select_vars UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList selectizeInput actionButton
#' @importFrom bslib accordion_panel
#' @importFrom fontawesome fa
mod_02_select_vars_ui <- function(id){
  ns <- NS(id)
  tagList(
 
    bslib::accordion_panel(
      title = "Select questions",
      icon = fontawesome::fa(name = "clipboard-question"),
      # shiny::textInput(
      shiny::selectizeInput(
        inputId = ns("q_start"),
        label = "From",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        width = "400px",
        options = list(create = FALSE)
      ),
      shiny::selectizeInput(
        inputId = ns("q_end"),
        label = "To",
        choices = NULL,
        selected = NULL,
        multiple = FALSE,
        width = "400px",
        options = list(create = FALSE)
      ),
      shiny::actionButton(
        inputId = ns("save_questions"),
        label = "Save"
      )
    )

  )
}
    
#' 02_select_vars Server Functions
#'
#' @noRd 
#' 
#' @importFrom gargoyle on trigger
#' @importFrom shiny updateSelectizeInput
#' @importFrom stringr str_extract
#' @importFrom dplyr `%>%` filter between row_number pull
mod_02_select_vars_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    gargoyle::on("move_to_questions", {

      # pass the variable labels to the UI
      shiny::updateSelectizeInput(
        inputId = "q_start",
        choices = r6$vars_in_qnr,
        server = TRUE
      )

      shiny::updateSelectizeInput(
        inputId = "q_end",
        choices = r6$vars_in_qnr,
        server = TRUE
      )

    })

    shiny::observeEvent(input$save_questions, {

      # recover the variable names from the selections
      var_pattern <- ".+?(?= : )"
      q_var_start <- stringr::str_extract(input$q_start, var_pattern)
      q_var_end <- stringr::str_extract(input$q_end, var_pattern)

      # create an array of variable names from selected start to end
      # get index of selected variables
      loc_q_var_start <- which(r6$prepared_qnr_df$varname == q_var_start)
      loc_q_var_end <- which(r6$prepared_qnr_df$varname == q_var_end)
      # select variables between start and end variable
      vars_for_file <- r6$prepared_qnr_df %>%
        dplyr::filter(
          dplyr::between(
            x = dplyr::row_number(),
            left = loc_q_var_start,
            right = loc_q_var_end
          )
        ) %>%
        dplyr::pull(.data$varname)

      # write variables and selection range both to R6 and to disk
      r6$q_var_start <- q_var_start
      r6$q_var_end <- q_var_end
      r6$vars_for_file <- vars_for_file
      r6$write()

      # signal to move to next UI prompt
      gargoyle::trigger("move_to_stata")

    })


  })
}
    
## To be copied in the UI
# mod_02_select_vars_ui("02_select_vars_1")
    
## To be copied in the server
# mod_02_select_vars_server("02_select_vars_1")
