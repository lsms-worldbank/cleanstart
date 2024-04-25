#' Construct user app data directory path
#'
#' @description
#' Consists of three pieces of information:
#'
#' - System location
#' - App name
#' - App version
#'
#' @importFrom rappdirs user_data_dir
#' @importFrom desc desc_get
#'
#' @return Character. Path to user's app data directory
construct_user_data_path <- function() {

  app_description_path <- app_sys("DESCRIPTION")

  dir <- rappdirs::user_data_dir(
    appname = desc::desc_get(
      "Package",
      file = app_description_path
    ),
    version = desc::desc_get(
      "Version",
      file = app_description_path
    )
  )

  return(dir)

}

#' Create user application directory for persistent storage
#' 
#' @return Character. Path where app directory was created/exists
#' 
#' @importFrom fs dir_exists dir_create
#' @importFrom glue glue
create_user_app_dir <- function() {

  # create user app data directory
  # determine where it should be
  app_dir <- construct_user_data_path()

  # create directory if it doesn't exist
  if (!fs::dir_exists(app_dir)) {
    fs::dir_create(app_dir)
  }

  # confirm that user app data directory exists
  app_dir_exists <- fs::dir_exists(app_dir)
  if (app_dir_exists == TRUE) {
    cat(glue::glue("App directory created at {app_dir}"))
    return(app_dir)
  } else {
    stop("App data directory could not be created")
  }

}
