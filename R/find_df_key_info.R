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
#' @param big_tbl_threshold a number, default is 1e6
#' @param db_system, default is 'duckdb' can also be 'MS-SQL'
#' @return a dataframe
#' @export
#'
db_tbl_key_info <- function(db_con,
                            tbl_name,
                            big_tbl_threshold = 1e6,
                            db_system = 'duckdb')
{# column names and number of columns
 all_vars <- DBI::dbListFields(conn = db_con, name = tbl_name)
 col_nbr <- length(all_vars)

 # number of rows
 query <- sprintf("select count(1) as n from %s", tbl_name)
 a_df <- DBI::dbGetQuery(db_con, query)
 row_nbr <- a_df$n

 v <- col_nbr * row_nbr

 if(v < big_tbl_threshold) {
   df <- DBI::dbReadTable(db_con, tbl_name)
   DBI::dbDisconnect(db_con)
   info_df <- find_df_key_info(df)
   return(info_df)
 }

 sampled_rows <- min(10000, row_nbr)
 cat(sprintf("\n IMPORTANT: A big table, having %d rows and %d columns;
            the returned info is based on sampled %s rows.\n",
            row_nbr,
            col_nbr,
            sampled_rows))

 db_system <- match.arg(db_system, c("duckdb", "MS-SQL"))
 if(db_system == 'duckdb') {
   query <- sprintf("SELECT * FROM %s
                    ORDER BY RANDOM() LIMIT %d", tbl_name, sampled_rows)
   } else if(db_system == 'MS-SQL') {
   query <- sprintf("SELECT top %d * FROM %s
                    ORDER BY NEWID()", sampled_rows, tbl_name)
   }

 df <- DBI::dbGetQuery(db_con, query)
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
