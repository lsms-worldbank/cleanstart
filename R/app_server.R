#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' 
#' @import shiny
#' @importFrom fs file_exists path
#' @importFrom gargoyle init
#' @importFrom bslib accordion_panel_open accordion_panel_close
#' 
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  # initialize R6 object

  # create application directory
  app_dir <- create_user_app_dir()

  # initialize R6 object
  r6 <- r6$new()
  r6$app_dir <- app_dir
  
  # handle R6 object as a function of (past) app state
  # case 1: app never used => ; past R6 not found as RDS in local storage
  # write new R6 to local storage
  if (!fs::file_exists(fs::path(app_dir, "saved_r6.rds"))) {
    r6$write()
  # case 2: app used => RDS exists in app's user data folder
  # restore R6 from past session by reading its values from RDS to R6
  } else {
    r6$read()
  }

  # intialize gargoyle triggers
  gargoyle::init("move_to_questions")
  gargoyle::init("move_to_stata")
  gargoyle::init("use_settings")

  # load module logic
  mod_01_get_qnr_data_server("01_get_qnr_data_1", r6 = r6)
  mod_02_select_vars_server("02_select_vars_1", r6 = r6)
  mod_03_write_stata_file_server("03_write_stata_file_1", r6 = r6)

  # move from Questionnaire to Questions
  gargoyle::on("move_to_questions", {
    bslib::accordion_panel_open(id = "my_panel", value = "Select questions")
    bslib::accordion_panel_close(id = "my_panel", value = "Load SuSo questionnaire")
  })

  # move from Questions to Create
  gargoyle::on("move_to_stata", {  
    bslib::accordion_panel_open(id = "my_panel", value = "Create Stata .do file")
    bslib::accordion_panel_close(id = "my_panel", value = "Select questions")
  })

}
