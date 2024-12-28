#' Run a shiny app
#' @param name A search string
#' @return A shiny app
#' @export

run_app <- function(name) {
  # locate all the shiny app examples that exist
  valid_examples <- list.files(system.file("apps", package = "edgar"))

  valid_msg <-
    paste0(
      "Valid examples are: '",
      paste(valid_examples, collapse = "', '"),
      "'"
    )

  # if an invalid example is given, throw an error
  if (missing(name) || !nzchar(name) || !name %in% valid_examples) {
    stop(
      "Please run `run_app()` with a valid name as an argument.\n",
      valid_msg,
      call. = FALSE
    )
  }

  # find and launch the app
  app_dir <- system.file("apps", name, package = "edgar")
  shiny::runApp(app_dir, display.mode = "normal", quiet = TRUE)
}
