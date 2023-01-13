#' Print a graph
#' @param name A search string
#' @return Plot
#' @export

plotgraph <- function(name) {
  # locate all the shiny app examples that exist
  validExamples <- list.files(system.file("graphs", package = "Graphs"))

  validExamplesMsg <-
    paste0(
      "Valid examples are:\n",
      paste(validExamples, collapse = "\n"))

  # if an invalid example is given, throw an error
  if (missing(name) || !nzchar(name) ||
      !name %in% validExamples) {
    stop(
      'Please run `plotgraph()` with a valid argument.\n',
      validExamplesMsg,
      call. = TRUE)
  }

  # find and launch the app
  #appDir <- system.file("apps", name, package = "ShinyApps")
  #shiny::runApp(appDir, display.mode = "normal", quiet = TRUE)
  directory <- system.file("graphs/", package = "Graphs")
  #file_R <- ".R"

  file_final <- paste(directory, name, sep = "/")


  source(file_final)
  showplot()

}

utils::globalVariables(c("showplot"))

