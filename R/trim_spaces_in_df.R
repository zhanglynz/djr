#' My Trim White Space
#'
#' Get rid of spaces: no sapce at the begining and ending; leave only one space
#' between characters
#' @param x a vector
#'
#' @return a vector---the input itself or after trimming
#' @export
#'
#' @examples
#' x <- ' long   space  character  '
#' y <- my_trim_ws(x)
my_trim_ws <- function(x) {
  if(!is.character(x)) return(x)
  y <- base::trimws(x, which = 'both')
  z <- base::gsub("\\s+", " ", y)
  return(z)
}

#' Trim Spaces in Data Frame
#'
#' @param a_df a data frame
#'
#' @return a data frame
#' @export
#'
#' @examples
#' df <- data.frame(x = 1:4,
#'                  y = c('a b ', ' cd e ', 'fgh i ', 'ok'),
#'                  z = rep(TRUE, 4))
#' new_df <- trim_spaces_in_df(df)
trim_spaces_in_df <- function(a_df) {
  as.data.frame(lapply(a_df, my_trim_ws))
}
