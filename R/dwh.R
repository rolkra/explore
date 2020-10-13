#============================================================================
#  function: dwh_connect
#============================================================================
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

  if (is.na(user))  {
    # use single sign on
    channel <- DBI::dbConnect(odbc::odbc(), dsn, ...)

  } else {
    # use user & passwort
    channel <- DBI::dbConnect(odbc::odbc(), dsn,
                              user = user,
                              password = if (pwd_crypt == TRUE) decrypt(pwd) else pwd,
                              ...
    )
  } # if
  return(channel)
}

#============================================================================
#  function: dwh_disconnect
#============================================================================
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
  DBI::dbDisconnect(connection, ...)
}

#============================================================================
#  function: dwh_read_table
#============================================================================
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

  # define sql
  sql <- paste0("select * from ", table)

  # read data from dwh
  data <- DBI::dbGetQuery(connection, sql, ...)

  # convert names to lower case
  if (names_lower) names(data) <- tolower(names(data))

  return(data)
}

#============================================================================
#  function: dwh_read_data
#============================================================================
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

  # read data from dwh
  data <- DBI::dbGetQuery(connection, sql, ...)

  # convert names to lower case
  if (names_lower) names(data) <- tolower(names(data))

  return(data)
}

#============================================================================
#  function: dwh_fastload
#============================================================================
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

  # check table (must be 'database.table')
  # split string at '.'
  table_split <- strsplit(table, split="[.]")
  database_name <- table_split[[1]][1]
  table_name <- table_split[[1]][2]

  # valid database_name and table_name?
  if ( is.na(database_name) | is.na(table_name) )   {
    stop("table must be in the format 'database.table'")
  }
  stopifnot (nchar(database_name) > 0, nchar(table_name) > 0)

  # connect
  con <- DBI::dbConnect(odbc::odbc(), dsn=dsn, database=database_name, ...)

  # write data
  DBI::dbWriteTable(con, name=table_name, value=data, overwrite=overwrite, append=append)

  # disconnect
  DBI::dbDisconnect(con)

} # dwh_fastload
