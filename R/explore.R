################################################################################
# explore by Roland Krasser
#
# Version 0.4.1
#   prepare for CRAN
#   fix na parameter for target_explore_cat
#   add data_dict_md function
#   improve documentation
#   use tempdir() in examples
#   switch from RODBC to DBI/odbc
#   drop dwh_write_table
#   define intermediates_dir as output_dir in render
#   add clean = TRUE in render (already was the default value)
#
# dwh_connect, dwh_disconnect,
# dwh_read_table, dwh_read_data, dwh_fastload
# clean_var
# describe, describe_all, describe_cat, describe_num
# explore, explore_all, explore_density, explore_shiny, explore_cor
# explain_tree, explain_logreg
# get_type, guess_cat_num, replace_na_with, format_num, format_target
# get_nrow, plot_text
# explore_cat, explore_num
# target_explore_cat, target_explore_num
################################################################################

#============================================================================
#  Function: Encrypt (Passwort)
#============================================================================
#' encrypt text
#'
#' @param text A text (character)
#' @param codeletters A string of letters that are used for encryption
#' @param shift Number of elements shifted
#' @return Encrypted text
#' @examples
#' encrypt("hello world")
#' @export

encrypt<-function (text, codeletters=c(toupper(letters),letters,0:9), shift=18)  {
  old=paste(codeletters,collapse="")
  new=paste(c(codeletters[(shift+1):nchar(old)],codeletters[1:shift]),collapse="")
  return (chartr(old,new,text))
}

#============================================================================
#  Function: Decrypt (Passwort)
#============================================================================
#' decrypt text
#'
#' @param text A text (character)
#' @param codeletters A string of letters that are used for decryption
#' @param shift Number of elements shifted
#' @return Decrypted text
#' @examples
#' decrypt("zw336 E693v")
#' @export

decrypt<-function (text, codeletters=c(toupper(letters),letters,0:9), shift=18)  {
  old=paste(codeletters,collapse="")
  new=paste(c(codeletters[(shift+1):nchar(old)],codeletters[1:shift]),collapse="")
  return (chartr(new,old,text))
}

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
#' @param ... Further arguments to be passed to DBI::dbConnect()
#' @return status
#' @examples
#' \dontrun{
#' dwh_fastload(data, "DWH", "database.table_test")
#' }
#' @export

dwh_fastload <- function(data, dsn, table, ...)  {

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
  DBI::dbWriteTable(con, name=table_name, value=data)

  # disconnect
  DBI::dbDisconnect(con)

} # dwh_fastload

#============================================================================
#  clean_var
#============================================================================
#' Clean variable
#'
#' Clean variable (replace NA values, set min_val and max_val)
#'
#' @param data A dataset
#' @param var Name of variable
#' @param na Value that replaces NA
#' @param min_val All values < min_val are converted to min_val (var numeric or character)
#' @param max_val All values > max_val are converted to max_val (var numeric or character)
#' @param name New name of variable (as string)
#' @return Dataset
#' @import rlang
#' @import dplyr
#' @examples
#' clean_var(iris, Sepal.Width, max_val = 3.5, name = "sepal_width")
#' @export

clean_var <- function(data, var, na = NA, min_val = NA, max_val = NA, name = NA)  {

  # check if var is missing
  if (missing(var)){
    warning("no variable defined, call function with variable that you want to clean!")
    return(data)
  }

  # tidy evaluation
  var_quo <- enquo(var)
  var_txt <- quo_name(var_quo)[[1]]

  # check if var exists
  if (sum(colnames(data) == var_txt) == 0){
    warning("can't find variable " ,var_txt, " in data, check variable name!")
    return(data)
  }

  # replace NA
  if (!is.na(na))  {
    na_pos <- is.na(data[[var_txt]])
    data[na_pos, var_txt] <- na
  }

  # set min value
  if (!is.na(min_val) & !is.factor(data[[var_txt]]))  {
    col <- data[ ,var_txt]
    col[col < min_val] <- min_val
    data[ ,var_txt] <- col
  }

  # set max value
  if (!is.na(max_val) & !is.factor(data[[var_txt]]))  {
    col <- data[ ,var_txt]
    col[col > max_val] <- max_val
    data[ ,var_txt] <- col
  }

  # rename variable
  if (!is.na(name))  {
    var_names <- colnames(data)
    if (name %in% var_names & name != var_txt)  {
      warning("variable ", name, " already exists in data. Did not rename, select other name!")
    } else {
    colnames(data)[colnames(data) == var_txt] <- name
    }
  }

  # return data
  data
} # clean_var

#============================================================================
#  plot_text
#============================================================================
#' Plot a text
#'
#' Plots a text (base plot) and let you choose text-size and color
#'
#' @param text Text as string
#' @param size Text-size
#' @param color Text-color
#' @return Plot
#' @importFrom graphics plot text
#' @examples
#' plot_text("hello", size = 2, color = "red")
#' @export

plot_text <- function(text="hello world", size=1.6, color="black")  {
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, text, cex = size, col = color)
}

#============================================================================
#  format_num
#============================================================================
#' Format number
#'
#' Formats a big number as k (1000) or M (100000)
#'
#' @param number A number (integer or real)
#' @param digits Number of digits
#' @return Formated number as text
#' @examples
#' format_num(5500, digits = 2)
#' @export

format_num <- function(number = 0, digits = 1)   {

  if (abs(number) >= 1000000) {
    result = paste0(format(round(number / 1000000, digits), digits = 15), "M")
  } else if (abs(number) >= 1000) {
    result = paste0(format(round(number / 1000, digits), digits = 15), "k")
  } else {
    result = paste0(format(round(number, digits), digits = 15))
  }

  #result = format(round(number, digits), big.mark = ".", decimal.mark = ",", digits = 15)
}

#============================================================================
#  format_target
#============================================================================
#' Format target
#'
#' Formats a target as a 0/1 attribute
#'
#' @param target Attribute as vector
#' @return Formated target
#' @examples
#' iris$is_virginica <- ifelse(iris$Species == "virginica", "yes", "no")
#' iris$target <- format_target(iris$is_virginica)
#' table(iris$target)
#' @export

format_target <- function(target)   {

  if (is.character(target))  {
    target <- as.factor(target)
  }

  if (is.factor(target))  {
    result <- ifelse(as.integer(target) == 1, 0, 1)
  } else {
    result <- target
  }
} # format_target

#============================================================================
#  Function: target_explore_cat
#============================================================================
#' Explore categorial variable + target
#'
#' Create a plot to explore relation between categorial variable and a binary target
#'
#' @param data A dataset
#' @param var_cat Name of categorial variable
#' @param var_target Name of target variable (0/1 or FALSE/TRUE)
#' @param min_val All values < min_val are converted to min_val
#' @param max_val All values > max_val are converted to max_val
#' @param flip Should plot be flipped? (change of x and y)
#' @param num2char If TRUE, numeric values in variable are converted into character
#' @param title Title of plot
#' @param auto_scale Not used, just for compatibility
#' @param na Value to replace NA
#' @param max_cat Maximum numbers of categories to be plotted
#' @param legend_position Position of legend ("right"|"bottom"|"non")
#' @return Plot object
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2

target_explore_cat <- function(data, var_cat, var_target = "target_ind", min_val = NA, max_val = NA, flip = TRUE, num2char = TRUE, title = NA, auto_scale = TRUE, na = NA, max_cat = 30, legend_position = "bottom") {

  # definitions for CRAN package check
  target <- NULL
  n_target <- NULL
  n_pct <- NULL
  weight <- NULL
  target_pct <- NULL
  num <- NULL

  # rename variables, to use it (lazy evaluation)
  data_bar <- data %>%
    rename_(target = var_target) %>%
    rename_(cat = var_cat)

  # replace na value
  if (!is.na(na))  {
    data_bar <- data_bar %>% replace_na_with("cat", na)
  }

  # format target as 0/1
  data_bar$target <- format_target(data_bar$target)

  # trim min, max (for non factors)
  if(!is.factor(data_bar$cat))  {
    data_bar <- data_bar %>%
      mutate(cat = ifelse(!is.na(max_val) & cat > max_val, max_val, cat)) %>%
      mutate(cat = ifelse(!is.na(min_val) & cat < min_val, min_val, cat))
  }

  # calculate percentage
  data_bar <- data_bar %>%
    group_by(cat) %>%
    summarise(n = n(), n_target = sum(target)) %>%
    ungroup() %>%
    mutate(n_pct = n / sum(n)*100) %>%
    mutate(target_pct = n_target/n*100)

  # calculate mean
  target_mean <- sum(data_bar$n_target) / sum(data_bar$n) * 100

  # define categories
  data_bar <- data_bar %>%
    mutate(weight = ifelse (n_pct <= 5, "00-05%",ifelse (n_pct <= 20, "06-20%", ifelse(n_pct <= 40, "21-40%", "41+%")))) %>%
    select(cat, n, n_pct, weight, n_target, target_pct)

  # convert to character
  if(num2char)  {
    data_bar <- data_bar %>% mutate(cat = as.character(cat))
  }

  # define colors
  bar_col <- c("#ECEFF1", "#CFD8DC", "#B0BEC5", "#90A4AE")
  names(bar_col) <- c("00-05%", "06-20%", "21-40%", "41+%")


  # limit number of categories
  if(nrow(data_bar) > max_cat)  {
    data_bar <- head(data_bar, max_cat)
  }

  # create plot
  plot_bar <- ggplot(data = data_bar) +
    geom_bar(aes(x=cat, y=target_pct, fill=weight), stat="identity") +
    theme_light() +
    #    theme(plot.margin=unit(c(0.5,0.5,0,1),"cm")) +   # o,r,u,l
    ggtitle(ifelse(is.na(title), var_cat, title)) +
    labs(x = "", y = "% target") +
    scale_fill_manual(name = "observations", values = bar_col) +
    #theme(legend.position="bottom") +
    theme(legend.position = legend_position) +
    geom_hline(yintercept = target_mean,
               color = "#7f7f7f", alpha = 0.5,
               linetype = "dashed", size = 1) +
    geom_text(aes(x=cat, y=target_pct, label = round(target_pct,1)),
              hjust = ifelse(flip,"top","center"),
              vjust = ifelse(flip,"center","top"),
              size = 3.5, color = "#525252")

  # flip plot?
  if(flip) plot_bar <- plot_bar + coord_flip()

  # save result
  # result <- list(data_bar, plot_bar)
  # names(result) <- c("data","plot")

  return(plot_bar)

} # target_explore_cat

#============================================================================
#  Function: replace_na_with
#============================================================================
#' Replace NA
#'
#' Replace NA values of an attribute in a dataframe
#'
#' @param data A dataframe
#' @param var_name Name of variable where NAs are replaced
#' @param with Value instead of NA
#' @return Updated dataframe
#' @examples
#' data <- data.frame(nr = c(1,2,3,NA,NA))
#' replace_na_with(data, "nr", 0)
#' @export

replace_na_with <- function(data, var_name, with)  {
  message(paste("replace NA in variable", var_name, "with", with))

  na_pos <- is.na(data[[var_name]])
  data[na_pos, var_name] <- with

  return(data)
}

#============================================================================
#  Function: target_explore_num
#============================================================================
#' Explore categorial variable + target
#'
#' Create a plot to explore relation between numerical variable and a binary target
#'
#' @param data A dataset
#' @param var_num Name of numerical variable
#' @param var_target Name of target variable (0/1 or FALSE/TRUE)
#' @param min_val All values < min_val are converted to min_val
#' @param max_val All values > max_val are converted to max_val
#' @param flip Should plot be flipped? (change of x and y)
#' @param title Title of plot
#' @param auto_scale Use 0.02 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param na Value to replace NA
#' @param legend_position Position of legend ("right"|"bottom"|"non")
#' @return Plot object
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2

target_explore_num <- function(data, var_num, var_target = "target_ind", min_val = NA, max_val = NA, flip = TRUE, title = NA, auto_scale = TRUE, na = NA, legend_position = "bottom") {

  # define variables for CRAN package check
  target <- NULL
  num <- NULL

  # rename variables, to use it (lazy evaluation)
  data_bar <- data %>%
    rename_(target = var_target) %>%
    rename_(num = var_num)

  if (!is.na(na)) {
    data_bar <- data_bar %>% replace_na_with("num", na)
  } else  {
    data_bar <- data_bar %>% filter(!is.na(num))  # needed, because otherwise problems with y-scale
  }

  # autoscale (if min_val and max_val is not used)
  if (auto_scale == TRUE & is.na(min_val) & is.na(max_val))  {
    r <- quantile(data_bar[["num"]], c(0.02, 0.98), na.rm = TRUE)
    min_val = r[1]
    max_val = r[2]
  }

  # if min_val is undefined, calcualte it
  if (is.na(min_val)) {
    min_val = min(data_bar[["num"]])
  }

  # if max_val is undefined, calcualte it
  if (is.na(max_val)) {
    max_val = max(data_bar[["num"]])
  }

  # trim min, max
  data_bar <- data_bar %>%
    mutate(num = ifelse(!is.na(max_val) & num > max_val, max_val, num)) %>%
    mutate(num = ifelse(!is.na(min_val) & num < min_val, min_val, num))

  # cut only when more then 1 different value in data
  if (min_val != max_val)  {
    data_bar <- data_bar %>% mutate(explore_cat  = cut(num, 10))
  } else {
    data_bar <- data_bar %>% mutate(explore_cat = min_val)
  }

  cat_labels <- data_bar %>%
    group_by(explore_cat) %>%
    summarize(cat_label = max(num), n = n())

  data_bar <- data_bar %>%
    inner_join(y = cat_labels, by = "explore_cat")

  #result <- data_bar

  result <- target_explore_cat(data_bar,
                               "cat_label",
                               "target",
                               flip = FALSE,
                               num2char = FALSE,
                               legend_position = legend_position,
                               title = ifelse(is.na(title),
                                              paste0(var_num),
                                              title)
  )

  return(result)

} # target_explore_num

#============================================================================
#  explore_cat
#============================================================================
#' Explore categorial variable
#'
#' Create a plot to explore categorial variable
#'
#' @param data A dataset
#' @param var_cat Name of numerical variable
#' @param flip Should plot be flipped? (change of x and y)
#' @param percent Plot values as percentage (instead of absolute numbers)
#' @param color Color of plot
#' @param auto_scale Use 0.02 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param max_cat Maximum number of categories to be plotted
#' @return Plot object (bar chart)
#' @importFrom magrittr "%>%"
#' @importFrom utils head
#' @import dplyr
#' @import ggplot2

explore_cat <- function(data, var_cat, flip = TRUE, percent = TRUE, color = "#cccccc", auto_scale = TRUE, max_cat = 30)  {

  # define variables for CRAN-package check
  cat <- NULL
  na_ind <- NULL

  # rename variables, to use it (lazy evaluation)
  data_bar <- data %>%
    rename_(cat = var_cat)

  na_check <- data_bar %>%
    mutate(na_ind = ifelse(is.na(cat),1,0)) %>%
    summarize(na_cnt = sum(na_ind), na_pct = sum(na_ind)/n())
  na_cnt <- na_check[1,1]
  na_pct <- na_check[1,2]

  # plot as percentact or absolut numbers?
  if (percent)  {

    n_pct <- NULL   # for CRAN package build

    data_pct <- data_bar %>%
      count(cat) %>%
      mutate(n_pct = n / sum(n) * 100)

    # limit number of categories
    if(nrow(data_pct) > max_cat)  {
      data_pct <- head(data_pct, max_cat)
    }

    plot_bar <- data_pct %>%
      ggplot(aes(x = cat, y = n_pct)) +
      geom_col(fill = color) +
      ggtitle(paste0(var_cat, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)")) +
      labs(x = "", y = "%") +
      theme_light() +
      geom_text(aes(x=cat, y=n_pct, label = round(n_pct,1)),
                hjust = ifelse(flip,"top","center"),
                vjust = ifelse(flip,"center","top"),
                size = 3.5, color = "#525252")

  } else {

    plot_bar <- data_bar %>% ggplot(aes(x = cat)) + geom_bar(fill = color) +
      ggtitle(paste0(var_cat, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)")) +
      labs(x = "", y = "") +
      theme_light()

  } # if

  # flip plot?
  if(flip) plot_bar <- plot_bar + coord_flip()

  plot_bar
}

#============================================================================
#  explore_num
#============================================================================
#' Explore numerical variable
#'
#' Create a plot to explore numerical variable
#'
#' @param data A dataset
#' @param var_num Name of numerical variable
#' @param min_val All values < min_val are converted to min_val
#' @param max_val All values > max_val are converted to max_val
#' @param flip Should plot be flipped? (change of x and y)
#' @param color Color of plot
#' @param bins Number of bins used for histogram
#' @param auto_scale Use 0.02 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @return Plot object (histogram)
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2

explore_num <- function(data, var_num, min_val = NA, max_val = NA, flip = FALSE, color = "#cccccc", bins = 15, auto_scale = TRUE)  {

  # define variables for CRAN-package check
  num <- NULL
  na_ind <- NULL

  # rename variables, to use it (lazy evaluation)
  data_bar <- data %>%
    rename_(num = var_num)

  # autoscale (if mni_val and max_val not used)
  if (auto_scale == TRUE & is.na(min_val) & is.na(max_val))  {
    r <- quantile(data_bar[["num"]], c(0.02, 0.98), na.rm = TRUE)
    min_val = r[1]
    max_val = r[2]
  }

  # trim min, max
  if (!is.na(min_val)) data_bar <- data_bar %>% filter(num >= min_val)
  if (!is.na(max_val)) data_bar <- data_bar %>% filter(num <= max_val)

  # count NA
  na_check <- data_bar %>%
    mutate(na_ind = ifelse(is.na(num),1,0)) %>%
    summarize(na_cnt = sum(na_ind), na_pct = sum(na_ind)/n())
  na_cnt <- na_check[1,1]
  na_pct <- na_check[1,2]

  plot_bar <- data_bar %>% ggplot(aes(x = num)) + geom_histogram(fill = color, bins = bins) +
    ggtitle(paste0(var_num, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)")) +
    labs(x = "", y = "") +
    theme_light()

  # flip plot?
  if(flip) plot_bar <- plot_bar + coord_flip()

  plot_bar
}

#============================================================================
#  explore_density
#============================================================================
#' Explore density of variable
#'
#' Create a density plot to explore numerical variable
#'
#' @param data A dataset
#' @param var Variable
#' @param target Target variable (0/1 or FALSE/TRUE)
#' @param min_val All values < min_val are converted to min_val
#' @param max_val All values > max_val are converted to max_val
#' @param color Color of plot
#' @param auto_scale Use 0.02 and 0.98 percent quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param ... Further arguments
#' @return Plot object (density plot)
#' @importFrom magrittr "%>%"
#' @import rlang
#' @import dplyr
#' @import ggplot2
#' @examples
#' explore_density(iris, "Sepal.Length")
#' iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
#' explore_density(iris, Sepal.Length, target = is_virginica)
#' @export

explore_density <- function(data, var, target, min_val = NA, max_val = NA, color = "grey", auto_scale = TRUE, ...)   {

  # parameter var
  if(!missing(var))  {
    var_quo <- enquo(var)
    var_txt <- quo_name(var_quo)[[1]]
  } else {
    var_txt = NA
  }

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    target_txt = NA
  }

  # define variables for CRAN-package check
  var_ <- NULL
  na_ind <- NULL
  target_ <- NULL

  # rename variables, to use it (lazy evaluation)
  data <- data %>%
    rename(var_ = !!var_quo)

  if (!is.na(target_txt))  {
    data <- data %>%
      rename(target_ = !!target_quo)
  }

  # autoscale (if mni_val and max_val not used)
  if (auto_scale == TRUE & is.na(min_val) & is.na(max_val))  {
    r <- quantile(data[["var_"]], c(0.02, 0.98), na.rm = TRUE)
    min_val = r[1]
    max_val = r[2]
  }

  # trim min, max
  if (!is.na(min_val)) data <- data %>% filter(var_ >= min_val)
  if (!is.na(max_val)) data <- data %>% filter(var_ <= max_val)

  # count NA
  na_check <- data %>%
    mutate(na_ind = ifelse(is.na(var_),1,0)) %>%
    summarize(na_cnt = sum(na_ind), na_pct = sum(na_ind)/n())
  na_cnt <- na_check[1,1]
  na_pct <- na_check[1,2]

  if (is.na(target_txt))  {

    # plot denisity var, no target
    data %>%
      ggplot(aes(var_)) +
      geom_density(fill = color, alpha = 0.7) +
      ggtitle(paste0(var_txt, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)")) +
      labs(x = "", y = "") +
      theme_light()
  } else {
    data %>%
      ggplot(aes(var_, fill = factor(target_, levels = c(0,1), ordered = TRUE))) +
      geom_density(alpha = 0.7) +
#     ggtitle(paste0(var_txt, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)")) +
      ggtitle(paste0("propensity by", var_txt)) +
      labs(x = "", y = "") +
      scale_fill_manual(values = c("#CFD8DC","#90A4AE"), name = "target") +
      theme_light()
  } # if

} # explore_density

#============================================================================
#  format_type
#============================================================================
#' Format type description
#'
#' Format type description of varable to 3 letters (int|dou|log|chr|dat)
#'
#' @param type Type description ("integer", "double", "logical", character", "date")
#' @return Formated type description (int|dou|log|chr|dat)
#' @examples
#' format_type(typeof(iris$Species))
#' @export

format_type <- function(type) {
  if (type == "numeric")  {
    return("num")
  } else if (type == "integer")  {
    return("int")
  } else if (type == "double")  {
    return("dou")
  } else if (type == "logical")  {
    return("log")
  } else if (type == "character")  {
    return("chr")
  } else if (type == "date")  {
    return("dat")
  }

  return("oth")
} # format_type

#============================================================================
#  get_type
#============================================================================
#' Return type of variable
#'
#' Return value of typeof, except if variable contains <hide>, then return "other"
#'
#' @param var A vector (dataframe column)
#' @return Value of typeof or "other"
#' @examples
#' get_type(iris$Species)
#' @export

get_type <- function(var)  {

  var_class <- class(var)[1]

  if (is.factor(var))  {
    return("factor")
  }

  if (var_class %in% c("numeric", "integer", "logical"))  {
    return(typeof(var))
  }

  if (var_class == "character")  {
    if (sum(var == "<hide>", na.rm = TRUE) > 0)  {
      return("other")
    } else {
        return("character")
    }
  }

  if (var_class == "Date")  {
    return("date")
  }

  return("other")

} # get_type


#============================================================================
#  guess_cat_num
#============================================================================
#' Return if variable is categorial or nomerical
#'
#' Guess if variable is categorial or numerical based on name, type and values of variable
#'
#' @param var A vector (dataframe column)
#' @return "cat" (categorial), "num" (numerical) or "oth" (other)
#' @examples
#' guess_cat_num(iris$Species)
#' @export

guess_cat_num <- function(var)  {
  # if var is missing, return "?"
  if (missing(var))  {
    warning("no variable to guess")
    return("?")
  }
  # all factors are cat
  if (is.factor(var)) {
    return("cat")
  }
  # for unsupported classes return "oth"
  if (class(var)[1] %in% c("numeric", "integer", "character", "logical"))  {
    var_class <- class(var)
  } else {
    return("oth")
  }
  ## intelligent guessing if num or cat (based on postfix of variable names)
  var_type <- typeof(var)
  # num with limited number of unique values is cat
  var_unique <- length(unique(var))
  # return result
  if (var_type %in% c("integer", "double")) {
    if (var_unique <= 8)  {
      return("cat")
    } else {
      return("num")
    }
  } else  {
    return("cat")
  }
} # guess_cat_num


#============================================================================
#  get_nrow
#============================================================================
#' Get number of rows for a grid plot
#'
#' @param varnames List of variables to be plotted
#' @param exclude Number of variables that will be excluded from plot
#' @param ncol Number of columns (default = 2)
#' @return Number of rows
#' @examples
#' get_nrow(names(iris), ncol = 2)
#' @export

get_nrow <- function(varnames, exclude = 0, ncol = 2)  {
  n <- length(varnames) - exclude
  result <- ceiling(n / ncol)
  result
}

#============================================================================
#  describe_num (out = text | list)
#============================================================================
#' Describe numerical variable
#'
#' @param data A dataset
#' @param var Variable or variable name
#' @param out Output format ("text"|"list")
#' @param margin Left margin for text output (number of spaces)
#' @return Description as text or list
#' @import rlang
#' @importFrom stats median quantile
#' @examples
#' describe_num(iris, Sepal.Length)
#' @export

describe_num <- function(data, var, out = "text", margin = 0) {

  # error if no data
  if (missing(data)) {
    stop("provide data to describe")
  }

  # parameter var
  if(!missing(var))  {
    var_quo <- enquo(var)
    var_txt <- quo_name(var_quo)[[1]]
  } else {
    var_txt = NA
  }

  # error if var is a factor
  if (is.factor(data[[var_txt]]))  {
    stop("use describe_cat for a factor")
  }

  var_name = var_txt
  var_type = get_type(data[[var_name]])

  var_obs = length(data[[var_name]])
  var_na = sum(is.na(data[[var_name]]))
  var_na_pct = var_na / var_obs * 100

  var_unique = length(unique(data[[var_name]]))
  var_unique_pct = var_unique / var_obs * 100

  var_min = min(data[[var_name]], na.rm = TRUE)
  var_median = median(data[[var_name]], na.rm = TRUE)
  var_mean = mean(data[[var_name]], na.rm = TRUE)
  var_max = max(data[[var_name]], na.rm = TRUE)
  var_quantile = quantile(data[[var_name]], c(0.05, 0.25, 0.75, 0.95), na.rm = TRUE)

  result_num <- list(name = var_name,
                     type = var_type,
                     #guess = var_guess,
                     na = var_na,
                     na_pct = var_na_pct,
                     unique = var_unique,
                     unique_pct = var_unique_pct,
                     min = var_min,
                     quantile = var_quantile,
                     max = var_max,
                     median = var_median,
                     mean = var_mean)

  if (out == "text")  {
    spc <- paste(rep(" ", margin), collapse = "")
    cat(paste0(spc, "variable ="), var_name, "\n")
    #cat("type     =", paste0(var_type, " (cat/num = ", var_guess,")\n"))
    cat(paste0(spc, "type     ="), var_type,"\n")
    cat(paste0(spc, "na       ="), paste0(format_num(var_na)," of ",format_num(var_obs)," (",format_num(var_na_pct),"%)\n"))
    cat(paste0(spc, "unique   ="), paste0(format_num(var_unique),"\n"))
    cat(paste0(spc, "min|max  ="), paste0(format_num(var_min), " | ", format_num(var_max), "\n"))
    cat(paste0(spc, "q05|q95  ="), paste0(format_num(var_quantile["5%"]), " | ", format_num(var_quantile["95%"]), "\n"))
    cat(paste0(spc, "q25|q75  ="), paste0(format_num(var_quantile["25%"]), " | ", format_num(var_quantile["75%"]), "\n"))
    cat(paste0(spc, "median   ="), format_num(var_median), "\n")
    cat(paste0(spc, "mean     ="), format_num(var_mean), "\n")
  } else {
    result_num
  }
} # describe_num

#============================================================================
#  describe_cat (out = text | list)
#============================================================================
#' Describe categorial variable
#'
#' @param data A dataset
#' @param var Variable or variable name
#' @param max_cat Maximum number of categories displayed
#' @param out Output format ("text"|"list")
#' @param margin Left margin for text output (number of spaces)
#' @return Description as text or list
#' @importFrom magrittr "%>%"
#' @import rlang
#' @import dplyr
#' @examples
#' describe_cat(iris, Species)
#' @export

describe_cat <- function(data, var, max_cat = 10, out = "text", margin = 0) {

  if (missing(data)) stop("provide data to describe")

  if(!missing(var))  {
    var_quo <- enquo(var)
    var_txt <- quo_name(var_quo)[[1]]
  } else {
    var_txt = NA
  }

  var_name = var_txt
  var_type = ifelse(is.factor(data[[var_name]]),
                    "factor",
                    get_type(data[[var_name]]))

  var_obs = length(data[[var_name]])
  var_na = sum(is.na(data[[var_name]]))
  var_na_pct = var_na / var_obs * 100

  var_unique = length(unique(data[[var_name]]))

  # define variable for cran check
  grp <- NULL

  # group categorial variable and calulate frequency
  var_frequency <- data %>%
    select(grp = !!var_quo) %>%
    group_by(grp) %>%
    summarise(n = n()) %>%
    mutate(pct = n / sum(n) * 100) %>%
    mutate(cat_len = nchar(as.character(grp)))

  # limit len of catnames
  max_cat_len <- max(var_frequency$cat_len, na.rm = TRUE)
  if(max_cat_len < 7)  {
    max_cat_len = 7
  }
  if(max_cat_len > 20)  {
    max_cat_len = 20
  }

  # result as a list
  result_cat <- list(name = var_name,
                     type = var_type,
                     na = var_na,
                     na_pct = var_na_pct,
                     unique = var_unique,
                     frequency = var_frequency)

  # result as text
  if (out == "text")  {

    spc <- paste(rep(" ", margin), collapse = "")
    cat(paste0(spc, "variable ="), var_name, "\n")
    #cat(paste0(spc, "type     ="), paste0(var_type, " (cat/num = ", var_guess,")\n"))
    cat(paste0(spc, "type     ="), paste0(var_type,"\n"))
    cat(paste0(spc, "na       ="), paste0(format_num(var_na)," of ",format_num(var_obs)," (",format_num(var_na_pct),"%)\n"))
    cat(paste0(spc, "unique   ="), paste0(format_num(var_unique),"\n"))

    # show frequency for each category (maximum max_cat)
    for (i in seq(min(var_unique, max_cat)))  {
      var_name = format(var_frequency[[i, 1]], width = max_cat_len, justify = "left")
      cat(paste0(spc, " ", var_name,
                 " = ", format_num(var_frequency[[i, 2]]), " (",
                 format_num(var_frequency[[i,3]]),"%)\n" ))
    } # for

    # if more categories than displayed, show "..."
    if (var_unique > max_cat)  {
      cat(paste0(spc, " ..."))
    }
  } else {
    result_cat
  }
} # describe_cat


#============================================================================
#  describe_all
#============================================================================
#' Describe all variables of a dataset
#'
#' @param data A dataset
#' @param out Output format ("small"|"large")
#' @return Dataset
#' @import dplyr
#' @examples
#' describe_all(iris)
#' @export

describe_all <- function(data = NA, out = "large") {

  # define variables for package check
  variable <- NULL
  type <- NULL
  na <- NULL
  na_pct <- NULL
  unique <- NULL
  min <- NULL
  mean <- NULL
  max <- NULL

  # define result data.frame
  result <- data.frame(variable = character(),
                       type = character(),
                       na = integer(),
                       na_pct = double(),
                       unique = integer(),
                       min = double(),
                       mean = double(),
                       max = double()
  )

  # names of attributes in data
  var_names <- names(data)

  # create plot for each attribute
  for(i in seq_along(var_names))  {

    var_name = var_names[i]
    var_obs = length(data[[var_name]])

    var_type = ifelse(is.factor(data[[var_name]]),
                      "fct",
                      format_type(get_type(data[[var_name]])))

    var_na = sum(is.na(data[[var_name]]))
    var_na_pct = round(var_na / var_obs * 100,1)

    var_unique = length(unique(data[[var_name]]))

    if (get_type(data[[var_name]]) %in% c("logical","integer","double") & !is.factor(data[[var_name]]))  {
      var_min = min(data[[var_name]], na.rm = TRUE)
      var_mean = mean(data[[var_name]], na.rm = TRUE)
      var_max = max(data[[var_name]], na.rm = TRUE)
    } else {
      var_min = NA
      var_mean = NA
      var_max = NA

#      # if attribute is <hide> overrule type as "oth"
#      if (sum(data[[var_name]] == "<hide>") > 0)  {
#        var_type = "oth"
#      }

    } # if

    result <- rbind(result,
                    data.frame(variable = var_name,
                               type = var_type,
                               na = var_na,
                               na_pct = var_na_pct,
                               unique = var_unique,
                               min = round(var_min,2),
                               mean = round(var_mean,2),
                               max = round(var_max,2)
                    ) # data.frame
    ) # rbind
  } # for

  # limit number of columns if out = "small"
  if (out == "small")  {
    result <- select(result, variable, type, na, na_pct)
  }

  # output
  result

} # function describe_all

#============================================================================
#  describe_tbl, out = text | vector
#============================================================================
#' Describe table
#'
#' Describe table (e.g. number of rows and columns of dataset)
#'
#' @param data A dataset
#' @param target Target variable
#' @param out Output format ("text"|"vector")
#' @return Description as text or vector
#' @import rlang
#' @examples
#' describe_tbl(iris)
#'
#' iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
#' describe_tbl(iris, is_virginica)
#' @export

describe_tbl <- function(data, target, out = "text")  {

  if(!missing(target))  {
    target <- enquo(target)
    target_txt <- quo_name(target)[[1]]
  } else {
    target_txt = NA
  }

  # number of rows /columns of data
  describe_nrow <- nrow(data)
  describe_ncol <- ncol(data)

  # sum of target=1 (if defined)
  if (!missing(target))  {

    # min value is representing target = 0, rest target = 1
    v <- data %>% dplyr::pull(!!target)

    table_cnt  <- v %>% table()
    target0_val <- min(names(table_cnt))

    # if all 1, guess there is no 0
    if (target0_val == 1)  {
      target0_val <- 0
    }
    target0_cnt <- sum(ifelse(data[[target_txt]] == target0_val, 1, 0))
    target1_cnt <- nrow(data) - target0_cnt
    describe_target1_cnt <- target1_cnt
  } else {
    describe_target1_cnt = 0
  }

  # result as a vector (text)

  result_vector <- c(cases = format_num(describe_nrow),
                     attributes = format_num(describe_ncol),
                     targets = format_num(describe_target1_cnt),
                     targets_pct = format_num(describe_target1_cnt / describe_nrow, digits = 2))

  # result as text
  if (!is.na(target_txt))  {

    result_text <- paste0(describe_nrow,
                          ifelse(describe_nrow >= 1000,
                                 paste0(" (",format_num(describe_nrow),")"),
                                 ""),
                          " observations with ",
                          format_num(describe_ncol),
                          " attributes; ",
                          format_num(describe_target1_cnt),
                          " targets (",
                          format_num(describe_target1_cnt / describe_nrow * 100, digits = 1),
                          "%)")
  } else {

    result_text <- paste0(describe_nrow,
                          ifelse(describe_nrow >= 1000,
                                 paste0(" (",format_num(describe_nrow),")"),
                                 ""),
                          " observations with ",
                          format_num(describe_ncol),
                          " attributes")
  } # if

  # return output
  if (out == "vector")  {
    result_vector
  } else {
    cat(result_text)
  }
} # describe_tbl

#============================================================================
#  describe
#============================================================================
#' Describe a dataset or variable
#'
#' Describe a dataset or variable (depending on input parameters)
#'
#' @param data A dataset
#' @param var A variable of the dataset
#' @param target Target variable (0/1 or FALSE/TRUE)
#' @param out Output format ("text"|"list") of variable description
#' @param ... Further arguments
#' @return Description as table, text or list
#' @import rlang
#' @examples
#' # Load package
#' library(magrittr)
#'
#' # Describe a dataset
#' iris %>% describe()
#'
#' # Describe a variable
#' iris %>% describe(Species)
#' iris %>% describe(Sepal.Length)
#' @export

describe <- function(data, var, target, out = "text", ...)  {

  # parameter var
  if(!missing(var))  {
    var_quo <- enquo(var)
    var_txt <- quo_name(var_quo)[[1]]
  } else {
    var_txt = NA
  }

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    target_txt = NA
  }

  # decide which describe-function to use
  if (is.na(var_txt) & !is.na(target_txt))  {
    describe_tbl(data, target = !!target_quo)
  } else if (is.na(var_txt)) {
    describe_all(data, out = out, ...)
  } else if (!is.na(var_txt)) {

    # reduce variables of data (to improve speed and memory)
    data <- data[var_txt]

    # describe depending on type (cat/num)
    var_guess <- guess_cat_num(data[[var_txt]])
    if (var_guess == "num") {
      describe_num(data, !!var_quo, out = out, ...)
    } else if (var_guess == "cat") {
         describe_cat(data, !!var_quo, out = out, ...)
    } else {
      warning("please use a numeric or character attribute to describe")
    }
  } # if

} # describe

#============================================================================
#  Function: data_dict_md
#============================================================================
#' Create a data dictionary Markdown file
#'
#' @param data A dataframe (data dictionary for all variables)
#' @param title Title of the data dictionary
#' @param description Detailed description of variables in data (dataframe with columns 'variable' and 'description')
#' @param output_file Output filename for Markdown file
#' @param output_dir  Directory where the Markdown file is saved
#' @return Create Markdown file
#' @examples
#' # Data dictionary of a dataframe
#' data_dict_md(iris,
#'              title = "iris flower data set",
#'              output_dir = tempdir())
#'
#' # Data dictionary of a dataframe with additional description of variables
#' description <- data.frame(
#'                  variable = c("Species"),
#'                  description = c("Species of Iris flower"))
#' data_dict_md(iris,
#'              title = "iris flower data set",
#'              description = description,
#'              output_dir = tempdir())
#' @export

data_dict_md <- function(data, title = "", description = NA, output_file = "data_dict.md", output_dir)  {

  # output_dir must be defined
  if(missing(output_dir)) {
    stop("output_dir must be defined")
  }

  # describe data
  d <- data %>% describe()
  d$variable <- as.character(d$variable)   # prevent factor

  # join detailed description
  if (!missing(description)) {
    description$variable <- as.character(description$variable)
    description$description <- as.character(description$description)
    d <- d %>% left_join(description, by = "variable")

    # replace NA with blanks
    d <- d %>% clean_var(description, na = "")
  } else {
    d$description <- ""
  }

  # markdown title
  txt <- ""
  txt <- paste0(txt, "# Data Dictionary","\n")

  if (!missing(title))  {
    txt <- paste0(txt, "**", title, "**", "\n")
  }

  txt <- paste0(txt, "\n")

  # markdown table header
  txt <- paste0(txt, "| variable | type  | na   | %na | unique | description |\n")
  txt <- paste0(txt, "| -------- | ----  | ---: | -----: | -----: | ----------- |\n")

  # markdown table content
  for (i in seq_along(d$variable))  {
    txt <- paste0(txt, " | ", d[i, "variable"],
                  " | ", d[i, "type"],
                  " | ", d[i, "na"],
                  " | ", d[i, "na_pct"],
                  " | ", d[i, "unique"],
                  " | ", d[i, "description"], " | "
    )
    txt <- paste0(txt, "\n")
  }

  file_name = path.expand(file.path(output_dir, output_file))
  writeLines(txt, file_name)
} # data_dict_md

#============================================================================
#  explore_all
#============================================================================
#' Explore all variables
#'
#' Explore all variables of a dataset (create plots)
#'
#' @param data A dataset
#' @param target Target variable (0/1 or FALSE/TRUE)
#' @param ncol Layout of plots (number of columns)
#' @param density Use density for histogramms
#' @param legend_position Position of legend ("right"|"bottom"|"non")
#' @return Plot
#' @import rlang
#' @importFrom gridExtra grid.arrange
#' @examples
#' explore_all(iris)
#'
#' iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
#' explore_all(iris, target = is_virginica)
#' @export

explore_all <- function(data, target, ncol = 2, density = TRUE, legend_position = "non")  {

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    target_txt = NA
  }

  # varable name of target
  var_name_target = target_txt

  # names of attributes in data
  var_names <- names(data)

  # if target_explore is used, ignore target variable
  if (!is.na(var_name_target)) {
    var_names <- var_names[var_names != var_name_target]
  }

  #pre define list of plots
  plots <- list(mode = "list", length = length(var_names))

  #cat("creating plots")
  # create plot for each attribute
  for(i in seq_along(var_names))  {

    #cat(".")

    var_name <- var_names[i]

    # reduce variables of data (to improve speed and memory)
    if (is.na(var_name_target)) {
      data_tmp <- data[var_name]
    }
    else {
      data_tmp <- data[c(var_name, var_name_target)]
    }

    # intelligent guessing if num or cat
    # based on postfix and type of variable names
    var_type <- guess_cat_num(data_tmp[[var_name]])

    # no target, num
    if ( (var_type == "num") & (is.na(var_name_target)) & (density == FALSE)) {
      plots[[i]] <- explore_num(data_tmp, var_names[i])

      # no target, num, density
    } else if ( (var_type == "num") & (is.na(var_name_target)) & (density == TRUE)) {
      plots[[i]] <- explore_density(data_tmp, !!var_name)

      # no target, cat
    } else if ( (var_type == "cat") & is.na(var_name_target) ) {
      plots[[i]] <- explore_cat(data_tmp, var_names[i])

      # target, num
    } else if ( (var_type == "num") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (density == FALSE))  {
      plots[[i]] <- target_explore_num(data_tmp, var_names[i], var_target = var_name_target, legend_position = legend_position)

      # target, num, density
    } else if ( (var_type == "num") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (density == TRUE))  {
      plots[[i]] <- explore_density(data_tmp, !!var_name, !!var_name_target)

      # target, cat
    } else if ( (var_type == "cat") & !is.na(var_name_target) & (var_names[i] != var_name_target) ) {
      plots[[i]] <- target_explore_cat(data_tmp, var_names[i], var_target = var_name_target, legend_position = legend_position)
    } # if
  } # for

  #cat("\n")
  gridExtra::grid.arrange(grobs = plots, ncol = ncol)

} # explore_all

#============================================================================
#  explain_tree
#============================================================================

#' Explain a target using a simple decision tree (classification or regression)
#'
#' @param data A dataset
#' @param target Target variable
#' @param maxdepth Maximal depth of the tree
#' @param minsplit The minimum number of observations that must exist in a node in order for a split to be attempted
#' @param cp Complexity parameter
#' @param size Textsize of plot
#' @param ... Further arguments
#' @return Plot
#' @examples
#' data <- iris
#' data$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
#' data$Species <- NULL
#' explain_tree(data, target = is_versicolor)
#' @export

explain_tree <- function(data, target, maxdepth=3, minsplit=20, cp=0, size=0.7, ...)  {
  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    target_txt = NA
    return(NULL)
  }

  # convert target into formula
  formula_txt <- as.formula(paste(target_txt, "~ ."))

  if(guess_cat_num(data[target_txt]) == "cat")  {

    # create tree cat
    mod <- rpart::rpart(formula_txt,
                        data = data,
                        method = "class",
                        control = rpart::rpart.control(maxdepth=maxdepth, minsplit=minsplit, cp=cp))
  } else {
    # create tree num
    mod <- rpart::rpart(formula_txt,
                        data = data,
                        method = "anova",  #"class",
                        control = rpart::rpart.control(maxdepth=maxdepth, minsplit=minsplit, cp=cp))
  } # if

  # check if tree was created. If not just plot info-text
  if(nrow(mod$frame) > 1)  {

      # plot tree
      rpart.plot::rpart.plot(mod,
                     prefix="target = ",       # prefix text in first line in node
                     type=2,                   # 2: split variable name under box, 5: split variable name in the interior nodes
                     yesno=2,                  # show yes/no at each node
                     #extra=107,                # 106 = % observations + target
                     branch=0,                 # 0 = V shaped, 1 = squared
                     branch.type=5,            # 5 = proportional width
                     box.palette = 'Blues',    # colors for nodes
                     shadow.col = 0,           # color of shadow, 0 = none
                     cex = size,
                     ...)
  } else {

    plot_text("can't grow decision tree")
  } # if tree exists

} # explain_tree


#============================================================================
#  explain_logreg
#============================================================================
#' Explain a binary target using logistic regression
#'
#' @param data A dataset
#' @param target Target variable (binary)
#' @param ... Further arguments
#' @return Dataset with results (term, estimate, std.error, z.value, p.value)
#' @importFrom stats complete.cases as.formula glm
#' @examples
#' data <- iris
#' data$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
#' data$Species <- NULL
#' explain_logreg(data, target = is_versicolor)
#' @export

explain_logreg <- function(data, target, ...)  {

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    target_txt = NA
  }

  if(sum(complete.cases(data)) < nrow(data))  {
    warning("can't calculate logreg, drop rows with NA first")
    return()
  }

  if(length(unique(data[[target_txt]])) != 2)  {
    warning("target must be binary (e.g. 0/1, TRUE/FALSE, 'yes'/'no')")
    return()
  }

  # convert target into formula
  formula_txt <- as.formula(paste(target_txt, "~ ."))

  mod <- suppressWarnings(glm(formula_txt, data = data, family = "binomial"))
  mod_stepwise <- suppressWarnings(MASS::stepAIC(mod, trace = FALSE))

  #summary(mod)

  broom::tidy(mod_stepwise)

} # explain_logreg

#============================================================================
#  explore_cor
#============================================================================
#' Explore the correlation between two attributes
#'
#' @param data A dataset
#' @param x Attribute on x axis
#' @param y Attribute on y axis
#' @param target Target variable (binary)
#' @param bins Number of bins
#' @param min_val All values < min_val are converted to min_val
#' @param max_val All values > max_val are converted to max_val
#' @param auto_scale Use 0.2 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param color Color of the plot
#' @param ... Further arguments
#' @return Plot
#' @examples
#' explore_cor(iris, x = Sepal.Length, y = Sepal.Width)
#' @export

explore_cor <- function(data, x, y, target, bins = 8, min_val = NA, max_val = NA, auto_scale = TRUE, color = "grey", ...)  {

  # parameter x
  if(!missing(x))  {
    x_quo <- enquo(x)
    x_txt <- quo_name(x_quo)[[1]]
  } else {
    x_txt = NA
    return(NULL)
  }

  # parameter y
  if(!missing(y))  {
    y_quo <- enquo(y)
    y_txt <- quo_name(y_quo)[[1]]
  } else {
    y_txt = NA
    return(NULL)
  }

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    target_txt = NA
  }

  x_type = guess_cat_num(data[[x_txt]])
  y_type = guess_cat_num(data[[y_txt]])
  target_type = guess_cat_num(data[[target_txt]])

  if(x_type == "num")  {

    # autoscale (if mni_val and max_val not used)
    if (auto_scale == TRUE & is.na(min_val) & is.na(max_val))  {
      r <- quantile(data[[x_txt]], c(0.02, 0.98), na.rm = TRUE)
      min_val = r[1]
      max_val = r[2]
    }

    # trim min, max
    if (!is.na(min_val)) data <- data %>% filter(!!x_quo >= min_val)
    if (!is.na(max_val)) data <- data %>% filter(!!x_quo <= max_val)

  } # if num

  if(x_type == "num" & y_type == "num")  {

    # boxplot (x = num, y = num)
    p <- data %>%
      # cut only when more then 1 different value in data
      ggplot(aes(x = !!x_quo, y = !!y_quo)) +
      geom_boxplot(aes(group = cut(!!x_quo, bins)), fill = color) +
      theme_light()


  }
  else if(x_type == "cat" & y_type == "num") {

    data[[x_txt]] <- as.factor(data[[x_txt]])

    # boxplot (x = cat)
    p <- data %>%
      ggplot(aes(x = !!x_quo, y = !!y_quo)) +
      geom_boxplot(aes(group = !!x_quo), fill = color) +
      theme_light()
  }

  else if(x_type == "num" & y_type == "cat") {

    data[[y_txt]] <- as.factor(data[[y_txt]])

    # boxplot (x = cat)
    p <- data %>%
      ggplot(aes(x = !!y_quo, y = !!x_quo)) +
      geom_boxplot(aes(group = !!y_quo), fill = color) +
      theme_light() +
      coord_flip()
  }

  else if(x_type == "cat" & y_type == "cat") {

    data[[x_txt]] <- as.factor(data[[x_txt]])
    data[[y_txt]] <- as.factor(data[[y_txt]])

    p <- data %>%
      ggplot(aes(x = !!x_quo, fill = !!y_quo)) +
      geom_bar(position = "fill") +
      theme_light()
  }

  if(!is.na(target_txt) & (target_type == "cat")) {
    p <- p + facet_grid(vars(!!target_quo))
  }

  # plot grafic
  p

} # explore_cor

#============================================================================
#  report
#============================================================================
#' Generate a report of all attributes
#'
#' Generate a report of all attributes
#' If target is defined, the relation to the target is reported
#'
#' @param data A dataset
#' @param target Target variable (0/1 or FALSE/TRUE)
#' @param density Use density? (TRUE/FALSE)
#' @param output_file Filename of the html report
#' @param output_dir Directory where to save the html report
#' @import rmarkdown
#' @examples
#' if (rmarkdown::pandoc_available("1.12.3"))   {
#'   report(iris, output_dir = tempdir())
#' }
#' @export

report <- function(data, target, density = FALSE, output_file, output_dir)  {

  # pandoc must be available to generate report
  # if RStudio is used, pandoc should be available
  if (!rmarkdown::pandoc_available())  {
    stop("no report generated because pandoc is not available")
  }

  # pandoc version 1.12.3 or higher must be available
  if (!rmarkdown::pandoc_version() >= "1.12.3")  {
    stop("no report generated because pandoc version >= 1.12.3 needed")
  }

  # output_dir must be defined
  if(missing(output_dir)) {
    stop("output_dir must be defined")
  }

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_text <- quo_name(target_quo)[[1]]
  } else {
    target_quo = NA
    target_text = NA
  }

  # parameter density (set default value, based on target)
  if (missing(density))  {
    if (is.na(target_text)) {
      density = TRUE
    }
    else {
      density = FALSE
    }
  }

# report only attributes
if(is.na(target_text))  {
  input_file <- system.file("extdata", "template_report_attribute.Rmd", package="explore")
  if (missing(output_file)) {output_file = "report_attributes.html"}
  rmarkdown::render(input = input_file,
                    output_file = output_file,
                    output_dir = output_dir,
                    intermediates_dir = output_dir,
                    clean = TRUE
                    )

  # report target with density
} else if(density == TRUE)  {
  input_file <- system.file("extdata", "template_report_target_den.Rmd", package="explore")
  if (missing(output_file)) {output_file = "report_target_density.html"}
  var_name_target <- target_text  # needed in report template
  rmarkdown::render(input = input_file,
                    output_file = output_file,
                    output_dir = output_dir,
                    intermediates_dir = output_dir,
                    clean = TRUE
                    )

  # report target with percent
} else {
  input_file <- system.file("extdata", "template_report_target_pct.Rmd", package="explore")
  if (missing(output_file)) {output_file = "report_target.html"}
  var_name_target <- target_text # needed in report template
  rmarkdown::render(input = input_file,
                    output_file = output_file,
                    output_dir = output_dir,
                    intermediates_dir = output_dir,
                    clean = TRUE
                    )
} # if
} # report

#============================================================================
#  explore_shiny
#============================================================================
#' Explore dataset interactive
#'
#' Launches a shiny app to explore a dataset
#'
#' @param data A dataset
#' @param target Target variable (0/1 or FALSE/TRUE)
#' @importFrom magrittr "%>%"
#' @import rlang
#' @import dplyr
#' @import shiny
#' @importFrom DT DTOutput renderDT
#' @importFrom utils browseURL
#' @import rmarkdown
#' @examples
#' # Only run examples in interactive R sessions
#' if (interactive())  {
#'    explore_shiny(iris)
#' }
#' @export

explore_shiny <- function(data, target)  {

  # check if interactive session
  if (!interactive()) stop("This function can only be used in an interactive R session")

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_text <- quo_name(target_quo)[[1]]
  } else {
    target_quo = NA
    target_text = NA
  }

  # define variables for CRAN-package check
  type <- NULL
  variable <- NULL

  # get attribute types
  tbl_guesstarget <- describe(data) %>%
    filter(unique <= 2) %>%
    filter((type %in% c("log","int","dou","num") &
              (min == 0 | min == FALSE) &
              (max == 1 | max == TRUE)) |
              (type == "fct") ) %>%
    select(variable)
  guesstarget <- as.character(tbl_guesstarget[[1]])

  # check all attributes if usable
  for (i in names(data))  {
    if (get_type(data[[i]]) == "other")  {
      data[[i]] <- "<hide>"
    }
  }

  # define ui
  ui <- shiny::fluidPage(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::h3("explore"),
        shiny::hr(),
        shiny::selectInput(inputId = "target",
                           label = "target",
                           choices = c("<no target>",guesstarget),
                           selected = "target_ind"),
        shiny::selectInput(inputId = "var",
                           label = "attribute",
                           choices = names(data),
                           selected = "disp"),
        shiny::checkboxInput(inputId = "auto_scale", label="auto scale", value=TRUE),
        shiny::checkboxInput(inputId = "target_density", label="target density", value=FALSE),
        shiny::hr(),
        shiny::actionButton(inputId = "report", "report all")
        , width = 3),  #sidebarPanel
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("attribute",
                          shiny::conditionalPanel(condition = "input.target != '<no target>'",
                                                  shiny::plotOutput("graph_target")),
                          shiny::plotOutput("graph", height = 300),
                          shiny::verbatimTextOutput("text")
          ),
          #textOutput("text")
          shiny::tabPanel("explain",
                          shiny::plotOutput("graph_explain")),
          shiny::tabPanel("overview", shiny::br(),
                          shiny::verbatimTextOutput("describe_tbl"),
                          DT::DTOutput("describe_all"))
          ,shiny::tabPanel("data", shiny::br(),
                           DT::DTOutput("view"))
        ) # tabsetPanel
        , width = 9) # mainPanel
    ) # sidebarLayout
  ) # fluidPage

  # server: calculate statistics and generate plot
  server <- function(input, output, session) {

    observeEvent(input$report, {

      # get name of selected target
      # rmarkdown templates uses variables data and var_name_target
      # templates must be located in package or if code is only sourced in C:/R
      var_name_target = input$target
      path <- getwd()
      output_file <- paste0(path,"/report_explore.html")

      # check if explore package is loaded
      run_explore_package <- ifelse(max(search() == "package:explore") == 1, TRUE, FALSE)

      # report only attributes
      if(input$target == "<no target>")  {
        input_file <- ifelse(run_explore_package,
                             system.file("extdata", "template_report_attribute.Rmd", package="explore"),
                             "C:/R/template_report_attribute.Rmd")
        rmarkdown::render(input = input_file, output_file = output_file)

        # report target with density
      } else if(input$target_density == TRUE)  {
        input_file <- ifelse(run_explore_package,
                             system.file("extdata", "template_report_target_den.Rmd", package="explore"),
                             "C:/R/template_report_target_den.Rmd")
        rmarkdown::render(input = input_file, output_file = output_file)

        # report target with percent
      } else {
        input_file <- ifelse(run_explore_package,
                             system.file("extdata", "template_report_target_pct.Rmd", package="explore"),
                             "C:/R/template_report_target_pct.Rmd")
        rmarkdown::render(input = input_file, output_file = output_file)
      }

      browseURL(paste0("file://", output_file))
    })

    output$graph_target <- shiny::renderPlot({
      if(input$target != "<no target>" & input$var != input$target)  {
        data %>% explore(!!input$var, target = !!input$target, auto_scale = input$auto_scale, density = input$target_density)
      }
    }) # renderPlot graph_target

    output$graph_explain <- shiny::renderPlot({
      if(input$target != "<no target>") {
        data %>% explain_tree(target = !!input$target, size=0.9)
      }
    }) # renderPlot graph_explain

    output$graph <- shiny::renderPlot({
      data %>% explore(!!input$var, auto_scale = input$auto_scale)
    }) # renderPlot graph

    output$text <- shiny::renderPrint({
      data %>% describe(!!input$var, out = "text", margin = 4)
    }) # renderText

    output$describe_tbl <- shiny::renderPrint({
      data %>% describe_tbl(out = "text")
    }) # renderText

    output$describe_all <- DT::renderDT({
      DT::datatable(data = data %>% describe(out = "text"),
                    rownames = FALSE,
                    selection = 'none',
                    options = list(pageLength = 15))
    }) # renderDataTable


    output$view <- DT::renderDT({
      DT::datatable(data = data,
                    rownames = FALSE,
                    selection = 'none',
                    options = list(pageLength = 15, scrollX = TRUE))
    }) # renderDataTable

  } # server

  # run shiny app
  shiny::shinyApp(ui = ui, server = server)

} # explore_shiny


#============================================================================
#  explore
#============================================================================
#' Explore a dataset or variable
#'
#' @param data A dataset
#' @param var A variable
#' @param var2 A variable for checking correlation
#' @param target Target variable (0/1 or FALSE/TRUE)
#' @param density Using density for histograms
#' @param min_val All values < min_val are converted to min_val
#' @param max_val All values > max_val are converted to max_val
#' @param auto_scale Use 0.2 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param na Value to replace NA
#' @param ... Further arguments
#' @return Plot object
#' @import rlang
#' @examples
#' ## Launch Shiny app (in interactive R sessions)
#' if (interactive())  {
#'    explore(iris)
#' }
#'
#' ## Explore grafically
#'
#' # Load library
#' library(magrittr)
#'
#' # Explore a variable
#' iris %>% explore(Species)
#' iris %>% explore(Sepal.Length)
#' iris %>% explore(Sepal.Length, min_val = 4, max_val = 7)
#' iris %>% explore(Sepal.Length, density = FALSE)
#'
#' # Explore a variable with a target
#' iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
#' iris %>% explore(Species, target = is_virginica)
#' iris %>% explore(Sepal.Length, target = is_virginica)
#'
#' # Explore correlation between two variables
#' iris %>% explore(Species, Petal.Length)
#' iris %>% explore(Sepal.Length, Petal.Length)
#'
#' # Explore correlation between two variables and split by target
#' iris %>% explore(Sepal.Length, Petal.Length, target = is_virginica)
#'
#' @export

explore <- function(data, var, var2, target, density, min_val = NA, max_val = NA, auto_scale = TRUE, na = NA, ...)  {

  # parameter var
  if (!missing(var)) {
    var_quo <- enquo(var)
    var_text <- quo_name(var_quo)[[1]]
  } else {
    var_quo <- NA
    var_text <- NA
  }

  # parameter var2
  if (!missing(var2)) {
    var2_quo <- enquo(var2)
    var2_text <- quo_name(var2_quo)[[1]]
  } else {
    var2_quo <- NA
    var2_text <- NA
  }

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_text <- quo_name(target_quo)[[1]]
  } else {
    target_quo = NA
    target_text = NA
  }

  # parameter density (set default value, based on target)
  if (missing(density))  {
    if (is.na(target_text)) {
      density = TRUE
    }
    else {
      density = FALSE
    }
  }

  # intelligent guessing if num or cat
  # based on postfix and type of variable names
  if (!is.na(var_text))  {
    var_type <- guess_cat_num(data[[var_text]])
  } else {
    var_type = "?"
  }
  # decide which type of plot

  # interactive (shiny)
  if (is.na(var_text))  {
    explore_shiny(data)

    # var + var2 -> correlation
  } else if (!is.na(var_text) & !is.na(var2_text) & is.na(target_text))  {
    explore_cor(data[c(var_text, var2_text)],
                x = !!var_quo, y = !!var2_quo,
                min_val = min_val, max_val = max_val, na = na, ...)

    # var + var2 + target -> correlation
  } else if (!is.na(var_text) & !is.na(var2_text) & !is.na(target_text))  {
    #explore_cor(data[c(var_text, var2_text, target_text)], !!var_quo, !!var2_quo, !!target_quo, ...)
    explore_cor(data[c(var_text, var2_text, target_text)],
                x = !!var_quo, y = !!var2_quo, target = !!target_quo,
                min_val = min_val, max_val = max_val, na = na, ...)

    # var_type oth
  } else if (!is.na(var_text) & var_type == "oth")  {
    warning("please use a numeric or character attribute to explore")

    # no target, num, density
  } else if (is.na(target_text) & (var_type == "num") & (density == TRUE))  {
    explore_density(data[var_text],
                    !!var_quo,
                    min_val = min_val, max_val = max_val, na = na, ...)

    # no target, num
  } else if (is.na(target_text) & (var_type == "num") & (density == FALSE))  {
    explore_num(data[var_text],
                var_text,
                min_val = min_val, max_val = max_val, ...)

    # no target, cat
  } else if (is.na(target_text) & (var_type == "cat")) {
    explore_cat(data[var_text],
                var_text, ...)

    # target, num, density
  } else if (!is.na(target_text) & (var_type == "num") & (density == TRUE)) {
    explore_density(data[c(var_text, target_text)],
                    var = !!var_quo, target = !!target_quo,
                    min_val = min_val, max_val = max_val, na = na, ...)

    # target, num
  } else if (!is.na(target_text) & (var_type == "num")) {
    target_explore_num(data[c(var_text, target_text)],
                       var_text, var_target = target_text,
                       min_val = min_val, max_val = max_val, na = na, ...)

    # target, cat
  } else if (!is.na(target_text) & (var_type == "cat")) {
    target_explore_cat(data[c(var_text, target_text)],
                       var_text, var_target = target_text,
                       min_val = min_val, max_val = max_val, na = na, ...)

  }

} # explore

