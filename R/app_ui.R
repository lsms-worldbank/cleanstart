#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib page_fluid bs_theme accordion
#' 
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # load dependencies
    # shinyFeedback::useShinyFeedback(),
    # Your application UI logic
    bslib::page_fluid(
      theme = bslib::bs_theme(version = 5),
      h1("cleanstart"),
      bslib::accordion(
        id = "my_panel",
        # Questionnaire file
        mod_01_get_qnr_data_ui("01_get_qnr_data_1"),
        # Questions to include
        mod_02_select_vars_ui("02_select_vars_1"),
        # Create Stata file
        mod_03_write_stata_file_ui("03_write_stata_file_1")
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "cleanstart"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
