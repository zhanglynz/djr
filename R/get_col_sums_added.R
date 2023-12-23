#' Get Column Sums Added In Dataframe
#'
#' This is to get column sums added in dataframe as its last row
#' @param a_df a data frame
#' @param first_col_is_label logical variable, default value TRUE
#'
#' @return a data frame, where the last row has column sums if the column is
#' summable
#' @export
#'
#' @examples
#' my_df <- data.frame(id = letters[1:5],
#' x = c(1:3, 4.1, NA),
#' y = LETTERS[1:5],
#' z = c(NA, NA, 1:3),
#' w = rep(NA, 5),
#' u = rep("", 5)
#' )
#' new_df <- get_col_sums_added(a_df = my_df)
get_col_sums_added <- function(a_df, first_col_is_label = TRUE)
{col_sums <- base::unlist(lapply(a_df, modi_sum))
 wk_df <- base::rbind(a_df, col_sums)
 if(first_col_is_label) {
   wk_df[, 1] <- as.character(wk_df[, 1])
   wk_df[nrow(wk_df), 1] <- "Total"
 }
 return(wk_df)
}
