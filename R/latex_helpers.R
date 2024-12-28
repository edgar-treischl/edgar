#' Make a Latex cventry entry
#'
#' @param center Param center.
#' @param lefttop Param lefttop.
#' @param righttop Param righttop.
#' @param rightbottom Param rightbottom.
#' @param leftbuttom Param leftbuttom.
#'
#' @return A chracter vector.
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
  x <- paste0(
    "\\cventry{", center, "}{", lefttop,
    "}{", righttop, "}{", rightbottom, "}{", leftbuttom, "}", "\n"
  )
  cat(x)
}


#' Make a Latex briefitem
#'
#' @param what Param what.
#' @param when Param when.
#' @param where Param where.
#'
#' @return A chracter vector.
#' @export
#'
#' @examples
#' brief_item(what = "Hallo", when = "world", where = "here")
brief_item <- function(what = "", when = "", where = "") {
  x <- paste0("\\briefitem{", what, "}{", when, "}{", where, "}", "\n")
  x
}



#' Make a Latex briefsection
#'
#' @param what Param what.
#' @param when Param when.
#' @param where Param where.
#'
#' @return A chracter vector.
#' @export
#'
#' @examples
#' brief_section(what = "Hallo", when = "world", where = "here")
brief_section <- function(what = " ", when = " ", where = " ") {
  x <- brief_item(what = what, when = when, where = where)
  x <- stringr::str_flatten(x)

  overall <- paste0(
    "\\briefsection{", "\n",
    x,
    "}"
  )

  cat(overall)
}
