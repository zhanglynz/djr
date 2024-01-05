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
              nbr_of_rows = dim(a_df)[1],
              nbr_of_unique_rows = vapply(a_df, len_uniq, numeric(1)),
              nbr_of_NAs = vapply(a_df, function(x) sum(is.na(x)), numeric(1)),
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


#' Find Key Info of a Table in Database
#'
#' @param db_con database connection
#' @param tbl_name table's name
#'
#' @return a dataframe
#' @export
#'
db_tbl_key_info <- function(db_con, tbl_name)
{# query <- sprintf("select * from %s", tbl_name)
 # df <- DBI::dbGetQuery(db_con, query)
 df <- DBI::dbReadTable(db_con, tbl_name)
 DBI::dbDisconnect(db_con)
 info_df <- find_df_key_info(df)
 return(info_df)
}


#' Find Names of Tables in Database
#'
#' @param db_con database connection
#'
#' @return a vector
#' @export
#'
db_tbl_names <- function(db_con)
{the_names <- DBI::dbListTables(db_con)
 DBI::dbDisconnect(db_con)
 return(the_names)
}
