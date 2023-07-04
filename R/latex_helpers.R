#' Make a cventry entry
#'
#' @param center Param.
#' @param lefttop Param.
#' @param righttop Param.
#' @param rightbottom Param.
#' @param leftbuttom Param.
#'
#' @return A ggplot2 object.
#' @export
#'
#' @examples
#' cventry(center = "Hallo", lefttop = "world")
#'
cventry <- function(center = " ",
                    lefttop = " ",
                    righttop = " ",
                    rightbottom = " ",
                    leftbuttom = " ") {


  x <-paste0("\\cventry{", center,"}{", lefttop,
             "}{", righttop, "}{", rightbottom,"}{",leftbuttom,"}", "\n")
  cat(x)
}


#' Make a briefitem
#'
#' @param what Param.
#' @param when Param.
#' @param where Param.
#'
#' @return A string
#' @export
#'
#' @examples
#' briefitem(what = "Hallo", when = "world", where = "here")


briefitem <- function(what = "", when = "", where = "") {
  x <- paste0("\\briefitem{", what, "}{", when, "}{",where, "}", "\n")
  x
}



#' Make a briefsection
#'
#' @param what Param.
#' @param when Param.
#' @param where Param.
#'
#' @return A string
#' @export
#'
#' @examples
#' briefsection(what = "Hallo", when = "world", where = "here")

briefsection <- function(what = " ", when = " ", where = " ") {
  #x <- paste0("\\briefitem{", what, "}{", when, "}{",where, "}")
  x <- briefitem(what = what, when = when, where = where)
  x <- stringr::str_flatten(x)

  overall <- paste0("\\briefsection{", "\n",
                    x,
                    "}")

  cat(overall)
}
