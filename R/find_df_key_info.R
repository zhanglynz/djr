#' Find Data Frame Key Information
#'
#' This is to find key information of a data frame for understanding/checking
#' the data frame
#' @param a_df a data frame
#'
#' @return a data frame
#' @export
#'
#' @examples
#' temp_df <-
#'      data.frame(a = 1:10,
#'                 b = NA,
#'                 e = c(letters[1:8], NA, NA),
#'                 stringsAsFactors = FALSE)
#' x <- find_df_key_info(temp_df)
find_df_key_info <- function(a_df)
{uq_v <- lapply(a_df, unique)
 a_sample <- lapply(uq_v, function(x) {
   if(length(x) <= 10) return(x)
   base::sample(x, 10)
 })
 re_df <-
   data.frame(var_name = names(a_df),
              var_type = vapply(a_df, typeof, character(1)),
              var_class = vapply(a_df, class, character(1)),
              no_of_rows = dim(a_df)[1],
              no_of_unique_rows = vapply(a_df, len_uniq, numeric(1)),
              no_of_NAs = vapply(a_df, function(x) sum(is.na(x)), numeric(1)),
              the_min = vapply(a_df, modi_min, numeric(1)),
              the_max = vapply(a_df, modi_max, numeric(1)),
              width_min =
                vapply(a_df, function(x) min_length(stringr::str_length(x)),
                              numeric(1)),
              width_max =
                vapply(a_df, function(x) max_length(stringr::str_length(x)),
                              numeric(1)),
              sample_values = base::I(a_sample),
              stringsAsFactors = FALSE)
 row.names(re_df) <- NULL
 return(re_df)
}
