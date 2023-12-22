#' Check Invalid UTF-8
#'
#' @param x a character vector
#'
#' @return a logical vector
#' @importFrom stringi stri_enc_toutf8
#' @export
#'
#' @examples
#' x <- c("Elm Grove\xa0, 505 Southampton Street East, Hastings", "123")
#' check_invalid_utf8(x)
check_invalid_utf8 <- function(x){
  !is.na(x) & is.na(iconv(x, "UTF-8", "UTF-8"))
}

#' Convert to UTF-8
#'
#' @param x a character vector
#'
#' @return a character vector
#' @export
#'
#' @examples
#' x <- c("Elm Grove\xa0, 505 Southampton Street East, Hastings", "123")
#' convert_to_utf8(x)
convert_to_utf8 <- function(x)
{stringi::stri_enc_toutf8(x, validate = TRUE)
}
