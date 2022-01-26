#' connect to DWH
#'
#' connect to datawarehouse (DWH) using ODBC
#'
#' @param dsn DSN string
#' @param user user name
#' @param pwd password of user
#' @param pwd_crypt is password encryption used?
#' @param ... Further arguments to be passed to DBI::dbConnect()
#' @return connection
#' @examples
#' \dontrun{
#' con <- dwh_connect(dsn = "DWH1", user = "u12345")
#' }
#' @export

dwh_connect <- function(dsn, user = NA, pwd = NA, pwd_crypt = FALSE, ...)  {

  .Defunct(msg = paste(
    "All dwh_ functions are no longer included in {explore}.",
    "You find the source code at https://github.com/rolkra/dwh"))

}

#' disconnect from DWH
#'
#' disconnect from datawarehouse (DWH) using a ODBC connection
#'
#' @param connection channel (ODBC connection)
#' @param ... Further arguments to be passed to DBI::dbDisconnect()
#' @examples
#' \dontrun{
#' dwh_disconnect(con)
#' }
#' @export

dwh_disconnect <- function(connection, ...)  {

  .Defunct(msg = paste(
    "All dwh_ functions are no longer included in {explore}.",
    "You find the source code at https://github.com/rolkra/dwh"))

}

#' read a table from DWH
#'
#' read a table from DWH using a ODBC connection
#'
#' @param connection DWH connection
#' @param table table name (character string)
#' @param names_lower convert field names to lower (default = TRUE)
#' @param ... Further arguments to be passed to DBI::dbGetQuery()
#' @return dataframe containing table data
#' @examples
#' \dontrun{
#' dwh_read_table(con, "database.table_test")
#' }
#' @export

dwh_read_table <- function(connection, table, names_lower = TRUE, ...)  {

  .Defunct(msg = paste(
    "All dwh_ functions are no longer included in {explore}.",
    "You find the source code at https://github.com/rolkra/dwh"))

}

#' read data from DWH
#'
#' read data from DWH using a ODBC connection
#'
#' @param connection DWH connection
#' @param sql sql (character string)
#' @param names_lower convert field names to lower (default = TRUE)
#' @param ... Further arguments to be passed to DBI::dbGetQuery()
#' @return dataframe containing table data
#' @examples
#' \dontrun{
#' dwh_read_data(con, "select * from database.table_test")
#' }
#' @export

dwh_read_data <- function(connection, sql, names_lower = TRUE, ...)  {

  .Defunct(msg = paste(
    "All dwh_ functions are no longer included in {explore}.",
    "You find the source code at https://github.com/rolkra/dwh"))

}

#' write data to a DWH table
#'
#' write data fast to a DWH table using a ODBC connection
#' Function uses packages DBI/odbc to write data faster than RODBC
#' Connects, writes data and disconnects
#'
#' @param data dataframe
#' @param dsn DSN string
#' @param table table name (character string)
#' @param overwrite Overwrite table if already exist
#' @param append Append data to table
#' @param ... Further arguments to be passed to DBI::dbConnect()
#' @return status
#' @examples
#' \dontrun{
#' dwh_fastload(data, "DWH", "database.table_test")
#' }
#' @export

dwh_fastload <- function(data, dsn, table, overwrite = FALSE, append = FALSE, ...)  {

  .Defunct(msg = paste(
    "All dwh_ functions are no longer included in {explore}.",
    "You find the source code at https://github.com/rolkra/dwh"))

} # dwh_fastload
