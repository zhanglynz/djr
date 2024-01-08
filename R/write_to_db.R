#' Write a File to Duckdb
#'
#' @param data the data, normally a dataframe
#' @param the_dir the directory where the database locates
#' @param table_name name of table to be created
#' @param overwrite_indi logical, default is FALSE
#'
#' @return
#' @export
#'
write_a_file_to_duckdb <- function(data, the_dir, table_name,
                                   overwrite_indi = FALSE)
{proj_name <- base::basename(base::getwd())
 the_dir <- file.path(the_dir, proj_name)
 db_con <- DBI::dbConnect(duckdb::duckdb(), dbdir = the_dir)
 DBI::dbWriteTable(db_con, table_name, data,
                   overwrite = overwrite_indi, row.names = FALSE)
 DBI::dbDisconnect(db_con, shutdown = TRUE)
}

