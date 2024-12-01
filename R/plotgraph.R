#' Plot a graph.
#' @param name A character string.
#' @return A plot.
#' @export

plot_graph <- function(name) {

  # locate the valid examples
  validExamples <- list.files(system.file("graphs", package = "edgar"))

  #create a message for the user
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

  #where the graphs are stored
  directory <- system.file("graphs/", package = "edgar")

  #append the file extension
  #file_R <- ".R"
  file_final <- paste(directory, name, sep = "/")

  #source the file
  source(file_final)
  showplot()

}

utils::globalVariables(c("showplot"))

