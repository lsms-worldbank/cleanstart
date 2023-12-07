#' 01_get_qnr_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList fileInput actionButton
#' @importFrom bslib accordion_panel
#' @importFrom fontawesome fa
mod_01_get_qnr_data_ui <- function(id){
  ns <- NS(id)
  tagList(

    bslib::accordion_panel(
      title = "Load SuSo questionnaire",
      icon = fontawesome::fa("clipboard"),
      shiny::fileInput(
        inputId = ns("qnr_path"),
        label = "Path to the SuSo questionnaire file",
        buttonLabel = "Browse",
        accept = ".json"
      ),
      shiny::actionButton(
        inputId = ns("save_qnr_path"),
        label = "Save"
      )
    )

  )
}
    
#' 01_get_qnr_data Server Functions
#' 
#' @importFrom shinyvalidate InputValidator
#' @importFrom fs path file_exists file_copy
#' @importFrom tools md5sum
#' @importFrom susometa parse_questionnaire
#' @importFrom dplyr `%>%` filter select mutate pull
#' @importFrom gargoyle trigger
#' 
#' @noRd 
mod_01_get_qnr_data_server <- function(id, r6){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # define input validation rules
    qnr_iv <- shinyvalidate::InputValidator$new()
    qnr_iv$add_rule(
      "qnr_path", 
      shinyvalidate::sv_required(message = "No questionnaire file selected")
    )

    shiny::observeEvent(input$save_qnr_path, {

      # run input validation rules
      qnr_iv$enable()

      # if input is valid, use input for logic
      if (qnr_iv$is_valid()) {

        # specify file paths of comparison and reference JSON files
        comparison_file <- fs::path(r6$app_dir, "comparison_document.json")
        reference_file <- fs::path(r6$app_dir, "reference_document.json")

        # if comparison JSON file aleady present, rename it to reference
        if (fs::file_exists(comparison_file)) {
          fs::file_copy(
            path = comparison_file,
            new_path = reference_file,
            overwrite = TRUE
          )
        }

        # copy user's file to app's local storage as new comparison file
        fs::file_copy(
          path = input$qnr_path$datapath,
          new_path = comparison_file,
          overwrite = TRUE
        )

        # compute md5sum for each file
        reference_md5sum <- ifelse(
          test = fs::file_exists(reference_file),
          yes = tools::md5sum(reference_file),
          no = NA
        )
        comparison_md5sum <- tools::md5sum(comparison_file)

        # if md5sum of files is different, process new file
        if (is.na(reference_md5sum) | comparison_md5sum != reference_md5sum) {

          # ingest questionnaire metadata
          qnr_df <- susometa::parse_questionnaire(comparison_file)

          # prepare metadata, through following steps:
          # - subset to questions and variables
          # - construct label
          # - sanitize label
          # - create granular question types
          # - convert SuSo expressions to Stata
          prepared_qnr_df <- qnr_df %>%
            dplyr::filter(!.data$type %in% c("Group", "StaticText")) %>%
            prepare_metadata()

          # construct display label for variable selection
          vars_in_qnr <- prepared_qnr_df %>%
            dplyr::select(.data$varname, .data$label) %>%
            dplyr::mutate(display_lbl = glue::glue("{varname} : {label}")) %>%
            dplyr::pull(.data$display_lbl)

          # write captured data to R6 and to local storage
          r6$prepared_qnr_df <- prepared_qnr_df
          r6$vars_in_qnr <- vars_in_qnr
          r6$write()

        }

        # signal to move to the next UI prompt
        gargoyle::trigger("move_to_questions")

      }

    })

  })
}
    
## To be copied in the UI
# mod_01_get_qnr_data_ui("01_get_qnr_data_1")
    
## To be copied in the server
# mod_01_get_qnr_data_server("01_get_qnr_data_1")
