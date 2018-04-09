################################################################################
# explore by Roland Krasser
#
# Version 0.2.4
# + guess_cat_num with vector as parameter instead of data + var
# + describe and explore with warning if guess_cat_num = "oth"
# + add get_type function
# + non cat/num attribute display as <hide>
# + change default color for explore_target to blue/gray
# - delete default DNS
#
# dwh_connect, dwh_disconnect, dwh_read_table, dwh_read_data
# explore, explore_all, explore_density, explore_shiny
# get_type, guess_cat_num, replace_na_with, format_num, format_target, get_nrow
# explore_cat, explore_num
# target_explore_cat, target_explore_num
################################################################################

#============================================================================
#  Settings & Init
#============================================================================
# load packages

# library(RODBC)         # ODBC-Verbindung
# library(dplyr)         # data manipulation and %>%
# library(ggplot2)       # plots
# library(gridExtra)     # plots next to each other
# library(shiny)         # interactive explore
# library(DT)            # render data tables
# library(rmarkdown)     # rendering markdown documents

#============================================================================
#  Function: Encrypt (Passwort)
#============================================================================
#' encrypt text
#'
#' @param text a text (character)
#' @param codeletters a string of letters that are used for encryption
#' @param shift number of elements shifted
#' @return encrypted text
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
#' @param text a text (character)
#' @param codeletters a string of letters that are used for decryption
#' @param shift number of elements shifted
#' @return decrypted text
#' @examples
#' decrypt("zw336 E693v")

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
#' @param dns DNS string
#' @param user user name
#' @param pwd password of user
#' @param pwd_crypt is password encryption used?
#' @return connection
#' @examples
#' con <- dwh_connect(dns = "DWH1", user = "u12345")
#' @export

dwh_connect <- function(dns, user = NA, pwd = NA, pwd_crypt = FALSE)  {

  if (is.na(user))  {
    # use single sign on
    channel <- RODBC::odbcConnect(dns)

  } else {
    # use user & passwort
    channel <- RODBC::odbcConnect(dns,
                                  uid=user,
                                  pwd=if (pwd_crypt == TRUE) decrypt(pwd) else pwd
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
#' @examples
#' dwh_disconnect(con)
#' @export

dwh_disconnect <- function(connection)  {
  RODBC::odbcClose(connection)
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
#' @return dataframe containing table data
#' @examples
#' dwh_read_table(con, "database.table_test")
#' @export

dwh_read_table <- function(connection, table, names_lower = TRUE)  {

  # define sql
  sql <- paste0("select * from ", table)

  # read data from dwh
  data <- RODBC::sqlQuery(connection, sql,
                          stringsAsFactors = FALSE,
                          dec = ",",
                          na.strings = "?")

  # convert names to lower case
  if (names_lower) {
    names(data) <- tolower(names(data))
  }

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
#' @return dataframe containing table data
#' @examples
#' dwh_read_data(con, "select * from database.table_test")
#' @export

dwh_read_data <- function(connection, sql, names_lower = TRUE)  {

  # read data from dwh
  data <- RODBC::sqlQuery(connection, sql,
                          stringsAsFactors = FALSE,
                          dec = ",",
                          na.strings = "?")

  # convert names to lower case
  if (names_lower) names(data) <- tolower(names(data))

  return(data)
}

#============================================================================
#  format_num
#============================================================================
#' format number
#'
#' formats a big number as k (1000) or M (100000)
#'
#' @param number a number (integer or real)
#' @param digits number of digits
#' @return formated number as text
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
#' format target
#'
#' formats a target as a 0/1 attribute
#'
#' @param target attribute as vector
#' @return formated target
#' @examples
#' data$target <- format_target(data$target)

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
#' explore categorial variable + target
#'
#' create a plot to explore relation between categorial variable and a binary target
#'
#' @param data a dataset
#' @param var_cat name of categorial variable
#' @param var_target name of target variable (0/1 or FALSE/TRUE)
#' @param min_val all values < min_val are converted to min_val
#' @param max_val all values > max_val are converted to max_val
#' @param flip should plot be flipped? (change of x and y)
#' @param num2char if TRUE, numeric values in variable are converted into character
#' @param title title of plot
#' @param autoscale not used, just for compatibility
#' @param max_cat maximum numbers of categories to be plotted
#' @param legend_position position of legend ("right"|"bottom"|"non")
#' @return plot object
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2
#' @examples
#' iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
#' target_explore_cat(iris, "Species", "is_virginica")

target_explore_cat <- function(data, var_cat, var_target = "target_ind", min_val = NA, max_val = NA, flip = TRUE, num2char = TRUE, title = NA, auto_scale = TRUE, max_cat = 30, legend_position = "bottom") {

  # rename variables, to use it (lazy evaluation)
  data_bar <- data %>%
    rename_(target = var_target) %>%
    rename_(cat = var_cat)

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
              size = 3, color = "#525252")

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
#' replace NA
#'
#' replace NA values of an attribute in a dataframe
#'
#' @param data a dataframe
#' @param var_name name of variable where NAs are replaced
#' @param with value instead of NA
#' @return updated dataframe
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
#' explore categorial variable + target
#'
#' create a plot to explore relation between numerical variable and a binary target
#'
#' @param data a dataset
#' @param var_cat name of numerical variable
#' @param var_target name of target variable (0/1 or FALSE/TRUE)
#' @param min_val all values < min_val are converted to min_val
#' @param max_val all values > max_val are converted to max_val
#' @param flip should plot be flipped? (change of x and y)
#' @param title title of plot
#' @param autoscale use 0.02 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param na value to replace NA
#' @param legend_position position of legend ("right"|"bottom"|"non")
#' @return plot object
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2
#' @examples
#' iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
#' target_explore_num(iris, "Sepal.Length", "is_virginica")

target_explore_num <- function(data, var_num, var_target = "target_ind", min_val = NA, max_val = NA, flip = TRUE, title = NA, auto_scale = TRUE, na = NA, legend_position = "bottom") {

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
                                              paste0("propensity by ", var_num),
                                              title)
  )

  return(result)

} # target_explore_num

#============================================================================
#  explore_cat
#============================================================================
#' explore categorial variable
#'
#' create a plot to explore categorial variable
#'
#' @param data a dataset
#' @param var_cat name of numerical variable
#' @param flip should plot be flipped? (change of x and y)
#' @param percent plot values as percentage (instead of absolute numbers)
#' @param color color of plot
#' @param autoscale use 0.02 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param max_cat maximum number of categories to be plotted
#' @return plot object (bar chart)
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2
#' @examples
#' explore_cat(iris, "Species")

explore_cat <- function(data, var_cat, flip = TRUE, percent = TRUE, color = "#cccccc", auto_scale = TRUE, max_cat = 30)  {

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
                size = 3, color = "#525252")

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
#' explore numerical variable
#'
#' create a plot to explore numerical variable
#'
#' @param data a dataset
#' @param var_num name of numerical variable
#' @param min_val all values < min_val are converted to min_val
#' @param max_val all values > max_val are converted to max_val
#' @param flip should plot be flipped? (change of x and y)
#' @param color color of plot
#' @param bins number of bins used for histogram
#' @param autoscale use 0.02 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @return plot object (histogram)
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2
#' @examples
#' explore_num(iris, "Sepal.Length")

explore_num <- function(data, var_num, min_val = NA, max_val = NA, flip = FALSE, color = "#cccccc", bins = 15, auto_scale = TRUE)  {

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
#' explore density of variable
#'
#' create a density plot to explore numerical variable
#'
#' @param data a dataset
#' @param var_num name of numerical variable
#' @param var_target name of target variable (0/1 or FALSE/TRUE)
#' @param min_val all values < min_val are converted to min_val
#' @param max_val all values > max_val are converted to max_val
#' @param color color of plot
#' @param autoscale use 0.02 and 0.98 percent quantile for min_val and max_val (if min_val and max_val are not defined)
#' @return plot object (density plot)
#' @importFrom magrittr "%>%"
#' @import rlang
#' @import dplyr
#' @import ggplot2
#' @examples
#' explore_density(iris, "Sepal.Length")
#' iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
#' explore_density(iris, Sepal.Length, target = is_virginica)

explore_density <- function(data, var, target, min_val = NA, max_val = NA, color = "#7f7f7f", auto_scale = TRUE, ...)   {

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
      geom_density(fill = color, alpha = 0.3) +
      ggtitle(paste0(var_txt, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)")) +
      labs(x = "", y = "") +
      theme_light()
  } else {
    data %>%
      ggplot(aes(var_, fill = factor(target_, levels = c(0,1), ordered = TRUE))) +
      geom_density(alpha = 0.3) +
      ggtitle(paste0(var_txt, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)")) +
      labs(x = "", y = "") +
      scale_fill_manual(values = c("#7f7f7f","#7dba00"), name = "target") +
      theme_light()
  } # if

} # explore_density

#============================================================================
#  format_type
#============================================================================
#' format type description
#'
#' format type description of varable to 3 letters (int|dou|log|chr)
#'
#' @param type type description ("integer", "double", "logical", character")
#' @return formated type description (int|dou|log|chr)
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
  }

  return("oth")
} # format_type

#============================================================================
#  get_type
#============================================================================
#' return type of variable
#'
#' return value of typeof, except if variable contains <hide>, then return "other"
#'
#' @param var a vector (dataframe column)
#' @return value of typeof or "other"
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

  return("other")

} # get_type


#============================================================================
#  guess_cat_num
#============================================================================
#' return if variable is categorial or nomerical
#'
#' guess if variable is categorial or numerical based on name, type and values of variable
#'
#' @param var a vector (dataframe column)
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
#' get number of rows for a grid plot
#'
#' @param varnames list of variables to be plotted
#' @param exclude number of variables that will be excluded from plot
#' @return number of rows
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
#' describe numerical variable
#'
#' @param data a dataset
#' @param var variable or variable name
#' @param out output format ("text"|"list")
#' @param margin left margin for text output (number of spaces)
#' @return description as text or list
#' @import rlang
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
#' describe categorial variable
#'
#' @param data a dataset
#' @param var variable or variable name
#' @param max_cat maximum number of categories displayed
#' @param out output format ("text"|"list")
#' @param margin left margin for text output (number of spaces)
#' @return description as text or list
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
#' describe all variables of a dataset
#'
#' @param data a dataset
#' @param out output format ("small"|"large")
#' @return dataset
#' @import dplyr
#' @examples
#' describe_all(iris)
#' @export

describe_all <- function(data = NA, out = "large") {

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
#' describe table
#'
#' describe table (e.g. number of rows and columns of dataset)
#'
#' @param data a dataset
#' @param target target variable
#' @param out output format ("text"|"list")
#' @return description as text or list
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
    describe_target1_cnt <- sum(data[[target_txt]])
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
#' describe a dataset, variable or table
#'
#' describe a dataset, variable or table (depending on input parameters)
#'
#' @param data a dataset
#' @param var a variable of the dataset
#' @param target target variable (0/1 or FALSE/TRUE)
#' @param out output format ("text"|"list")
#' @return description as table, text or list
#' @import rlang
#' @examples
#' describe(iris)
#' describe(iris, Species)
#' describe(iris, Sepal.Length)
#'
#' iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
#' describe(iris, target = is_virginica)
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
#  explore_all
#============================================================================
#' explore all variables
#'
#' explore all variables of a dataset (create plots)
#'
#' @param data a dataset
#' @param target target variable (0/1 or FALSE/TRUE)
#' @param ncol layout of plots (number of columns)
#' @param density use density for histogramms
#' @param legend_position position of legend ("right"|"bottom"|"non")
#' @return description as table, text or list
#' @import rlang
#' @import gridExtra
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
#  explore_shiny
#============================================================================
#' explore dataset interactive
#'
#' launches a shiny app to explore a dataset
#'
#' @param data a dataset
#' @param target target variable (0/1 or FALSE/TRUE)
#' @importFrom magrittr "%>%"
#' @import rlang
#' @import dplyr
#' @import shiny
#' @import DT
#' @import rmarkdown
#' @examples
#' explore_shiny(iris)
#' @export

explore_shiny <- function(data, target)  {

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_text <- quo_name(target_quo)[[1]]
  } else {
    target_quo = NA
    target_text = NA
  }

  # get attribute types
  tbl_guesstarget <- describe(data) %>%
    filter(unique <= 2) %>%
    filter((type %in% c("log","int","dou","num") & ((min == 0 | min == FALSE))) |
           (type == "fct") |
           (type == "chr")) %>%
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
          shiny::tabPanel("overview", shiny::br(),
                          shiny::verbatimTextOutput("describe_tbl"),
                          DT::dataTableOutput("describe_all"))
          ,shiny::tabPanel("data", shiny::br(),
                           DT::dataTableOutput("view"))
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
        data %>% explore(!!input$var, !!input$target, auto_scale = input$auto_scale, density = input$target_density)
      }
    }) # renderPlot graph_target

    output$graph <- shiny::renderPlot({
      data %>% explore(!!input$var, auto_scale = input$auto_scale)
    }) # renderPlot graph

    output$text <- shiny::renderPrint({
      data %>% describe(!!input$var, out = "text", margin = 4)
    }) # renderText

    output$describe_tbl <- shiny::renderPrint({
      data %>% describe_tbl(out = "text")
    }) # renderText

    output$describe_all <- DT::renderDataTable({
      DT::datatable(data = data %>% describe(out = "text"),
                    rownames = FALSE,
                    selection = 'none',
                    options = list(pageLength = 15))
    }) # renderDataTable


    output$view <- DT::renderDataTable({
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
#' explore a dataset or variable
#'
#' @param data a dataset
#' @param var a variable
#' @param target target variable (0/1 or FALSE/TRUE)
#' @param density using density for histograms
#' @param out plot layout ("single"|"double"|"all")
#' @param min_val all values < min_val are converted to min_val
#' @param max_val all values > max_val are converted to max_val
#' @param autoscale use 0.2 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param na value to replace NA
#' @return plot object
#' @import rlang
#' @examples
#' explore(iris, Species)
#' explore(iris, Sepal.Length)
#' explore(iris, Sepal.Length, density = FALSE)
#'
#' iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
#' explore(iris, Species, target = is_virginica)
#' explore(iris, Sepal.Length, target = is_virginica)
#' @export

explore <- function(data, var, target, density, out = "single", ...)  {

  # parameter var
  if (!missing(var)) {
    var_quo <- enquo(var)
    var_text <- quo_name(var_quo)[[1]]
  } else {
    var_quo <- NA
    var_text <- NA
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

    # var_type oth
  } else if (!is.na(var_text) & var_type == "oth")  {
    warning("please use a numeric or character attribute to explore")

    # single, no target, num, density
  } else if (is.na(target_text) & (var_type == "num") & (out == "single") & (density == TRUE))  {
    explore_density(data[var_text], !!var_quo, ...)

    # single, no target, num
  } else if (is.na(target_text) & (var_type == "num") & (out == "single") & (density == FALSE))  {
    explore_num(data[var_text], var_text, ...)

    # single, no target, cat
  } else if (is.na(target_text) & (var_type == "cat") & (out == "single")) {
    explore_cat(data[var_text], var_text, ...)

    # single, target, num, density
  } else if (!is.na(target_text) & (var_type == "num") & (out == "single") & (density == TRUE)) {
    explore_density(data[c(var_text, target_text)], var = !!var_quo, target = !!target_quo, ...)

    # single, target, num
  } else if (!is.na(target_text) & (var_type == "num") & (out == "single")) {
    target_explore_num(data[c(var_text, target_text)], var_text, var_target = target_text, ...)

    # single, target, cat
  } else if (!is.na(target_text) & (var_type == "cat") & (out == "single")) {
    target_explore_cat(data[c(var_text, target_text)], var_text, var_target = target_text, ...)

    # double, target, num, density
  } else if (!is.na(target_text) & (var_type == "num") & (density == TRUE) & (out == "double")) {
    p1 <- explore_density(data[c(var_text, target_text)], !!var_quo, ...)
    p2 <- target_explore_num(data[c(var_text, target_text)], var_text, var_target = target_text, ...)
    grid.arrange(p1, p2, ncol = 2)

    # double, target, num
  } else if (!is.na(target_text) & (var_type == "num") & (density == FALSE) & (out == "double")) {
    p1 <- explore_num(data[c(var_text, target_text)], var_text, ...)
    p2 <- target_explore_num(data[c(var_text, target_text)], var_text, var_target = target_text, ...)
    grid.arrange(p1, p2, ncol = 2)

    # double, target, cat
  } else if (!is.na(target_text) & (var_type == "cat") & (out == "double")) {
    p1 <- explore_cat(data[c(var_text, target_text)], var_text, ...)
    p2 <- target_explore_cat(data[c(var_text, target_text)], var_text, var_target = target_text, ...)
    grid.arrange(p1, p2, ncol = 2)

    # all, target, density
  } else if (!is.na(target_text) & (density == TRUE) & (out == "all")) {
    explore_all(data, target = !!target_quo, density = TRUE, ...)

    # all, target
  } else if (!is.na(target_text) & (density == FALSE) & (out == "all")) {
    explore_all(data, target = !!target_quo, ...)

    # all, no target, density
  } else if (is.na(target_text) & (density == TRUE) & (out == "all")) {
    explore_all(data, density = TRUE, ...)

    # all, no target
  } else if (is.na(target_text) & (density == FALSE) & (out == "all")) {
    explore_all(data, ...)

  }

} # explore
