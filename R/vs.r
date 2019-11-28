#' Search for columns containing string
#'
#' \code{vs} returns a vector of all column names that contain the string
#'   \code{chars}
#'
#' This function is meant to help you find the column name of interest when
#'   there are too many columns to visually search for the one you want
#'
#' @param ds object (i.e. data.frame) whose column names will be searched
#' @param chars the character string to search for in the column names of
#'   \code{ds}
#' @param ignore_case if \code{TRUE} then the case of \code{chars} and the
#'   column names of \code{ds} are ignored, otherwise, it will only match
#'   if the case matches
#'
#' @examples
#' data = data.frame(apple = 1:4,
#'                   orange = 5:8,
#'                   orangutan = 2:5,
#'                   applause = 3:6)
#'
#' vs("oran", data)
#' @export vs
vs <- function(chars="", ds, ignore_case = TRUE) {
  return(colnames(ds)[grepl(chars,colnames(ds), ignore.case = ignore_case)])
}

