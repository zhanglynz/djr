#' Length of Unique Values
#'
#' @param x a vector
#'
#' @return an integer
#' @export
#'
len_uniq <- function(x) {
  length(unique(x))
}

#' Modified Sum, Min, Max functions (written by Matthew Hendtlass)
#'
#' Applies operation with NAs removed; if the input is either not numeric or all
#' \code{NA}, \code{modi_sum, modi_min, modi_max} return \code{NA_real_} and
#' \code{min_length, max_length} return \code{0}.
#' @param f an R function
#' @param default default value
#' @param x a vector
#'
#' @return A numeric vector of length 1
modify <- function(f, default = NA_real_) {
  function(x)
    if (!is.numeric(x) || all(is.na(x))) default
  else f(x, na.rm = TRUE)
}
#' @rdname modify
modi_sum <- modify(sum)
#' @rdname modify
modi_min <- modify(min)
#' @rdname modify
modi_max <- modify(max)
#' @rdname modify
min_length <- modify(min, 0)
#' @rdname modify
max_length <- modify(max, 0)
