#' Run a shiny app
#' @param name A search string
#' @return A shiny app
#' @export

run_app <- function(name) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("apps", package = "shinyapps"))

  validExamplesMsg <-
    paste0(
      "Valid examples are: '",
      paste(validExamples, collapse = "', '"),
      "'")

  # if an invalid example is given, throw an error
  if (missing(name) || !nzchar(name) ||
      !name %in% validExamples) {
    stop(
      'Please run `run_app()` with a valid name as an argument.\n',
      validExamplesMsg,
      call. = FALSE)
  }

  # find and launch the app
  appDir <- system.file("apps", name, package = "shinyapps")
  shiny::runApp(appDir, display.mode = "normal", quiet = TRUE)
}
