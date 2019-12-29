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
#  balance_target
#============================================================================
#' Balance target variable
#'
#' Balances the target variable in your dataset.
#' Target must be 0/1, FALSE/TRUE ore no/yes
#'
#' @param data A dataset
#' @param target Target variable (0/1, TRUE/FALSE, yes/no)
#' @param min_prop Minimum proportion of one of the target categories
#' @return Data
#' @import rlang
#' @import dplyr
#' @examples
#' iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
#' balanced <- balance_target(iris, target = is_versicolor, min_prop = 0.5)
#' describe(balanced, is_versicolor)
#' @export

balance_target <- function(data, target, min_prop = 0.1) {

  # check if parameters are missing
  if (missing(data))  {
    stop("data is missing")
  }

  if (missing(target))  {
    stop("target is missing")
  }

  # check if min_prop has a meaningful value
  if (min_prop < 0 | min_prop > 1)  {
    stop("min_prop must be a value between 0 and 1")
  }

  # tidy eval for target
  target_quo <- enquo(target)
  target_txt <- quo_name(target_quo)[[1]]

  # check levels of target
  target_levels <- length(unique(data[[target_txt]]))
  if (target_levels > 2)  {
    stop(paste("target has", target_levels, "levels, expected 2"))
  }

  # balance
  observed_prop   <- data %>%
    dplyr::pull(!!target_quo) %>%
    table()
  minClass        <- min(observed_prop)
  names(minClass) <- names(which(observed_prop == minClass))
  maxClass        <- floor(minClass / min_prop - minClass)

  if (max(observed_prop) < maxClass) {
    return(data)

  } else {
    data_minClass <- data %>%
      dplyr::filter(!!target_quo == names(minClass)) %>%
      dplyr::sample_n(minClass)
    data_maxClass <- data %>%
      dplyr::filter(!!target_quo != names(minClass)) %>%
      dplyr::sample_n(maxClass)

    return(rbind(data_minClass, data_maxClass))
  }
} # balance_target

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

plot_text <- function(text="hello world", size=1.2, color="black")  {
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, text, cex = size, col = color)
}

#============================================================================
#  plot_var_info
#============================================================================
#' Plot a variable info
#'
#' Creates a ggplot with the variable-name as title and a text
#'
#' @param data A dataset
#' @param var Variable
#' @param info Text to plot
#' @return Plot (ggplot)
#' @import rlang
#' @import dplyr
#' @import ggplot2

plot_var_info <- function(data, var, info = "")  {

  # parameter var
  if(!missing(var))  {
    var_quo <- enquo(var)
    var_txt <- quo_name(var_quo)[[1]]
  } else {
    var_txt = NA
  }

  # plot variable info
  ggplot(NULL) +
    geom_blank() +
    geom_text(aes(x = 0, y = 0, label = info)) +
    theme_minimal() +
    labs(title = var_txt, x = "", y = " ") +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          plot.margin = unit(c(0.1,0.1,0.5,1), "cm")) #t,r,b,l
} # plot_var_info

#============================================================================
#  format_num_spcace
#============================================================================
#' Format number as character string (space as big.mark)
#'
#' Formats a big number using space as big.mark (1000 = 1 000)
#'
#' @param number A number (integer or real)
#' @param digits Number of digits
#' @return Formated number as text
#' @examples
#' format_num_space(5500, digits = 2)
#' @export

format_num_space <- function(number = 0, digits = 1)   {

  # no format if it is not numeric
  if (!is.numeric(number))  {
    return(number)
  }

  number <- format(round(number, digits),
                   big.mark = " ",
                   scientific = FALSE)

  number

} # format_num_space

#============================================================================
#  format_num_kMB
#============================================================================
#' Format number as character string (kMB)
#'
#' Formats a big number as k (1 000), M (1 000 000) or B (1 000 000 000)
#'
#' @param number A number (integer or real)
#' @param digits Number of digits
#' @return Formated number as text
#' @examples
#' format_num_kMB(5500, digits = 2)
#' @export

format_num_kMB <- function(number = 0, digits = 1)   {

  # no format if it is not numeric
  if (!is.numeric(number))  {
    return(number)
  }

  if (abs(number) >= 1000000000) {
    result = paste0(format(round(number / 1000000000, digits), digits = 15), "B")
  } else if (abs(number) >= 1000000) {
    result = paste0(format(round(number / 1000000, digits), digits = 15), "M")
  } else if (abs(number) >= 1000) {
    result = paste0(format(round(number / 1000, digits), digits = 15), "k")
  } else {
    result = paste0(format(round(number, digits), digits = 15))
  }

  result
  #result = format(round(number, digits), big.mark = ".", decimal.mark = ",", digits = 15)
}


#============================================================================
#  format_num_auto
#============================================================================
#' Format number as character string (auto)
#'
#' Formats a number depending on the value as number with space, scientific or
#' big number as k (1 000), M (1 000 000) or B (1 000 000 000)
#'
#' @param number A number (integer or real)
#' @param digits Number of digits
#' @return Formated number as text
#' @examples
#' format_num_kMB(5500, digits = 2)
#' @export

format_num_auto <- function(number = 0, digits = 1)   {

  # no format if it is not numeric
  if (!is.numeric(number))  {
    return(number)
  }

  # scientific
  if (abs(number) >= 100000000000) {
    return(as.character(format(number, scientific = TRUE)))
  }

  # space, no kMB
  if (abs(number) < 1000) {
    return(format_num_space(number, digits))
  }

  # space, with kMB
  return(format_num_space(number, digits))

} # format_num_auto

#============================================================================
#  format_target
#============================================================================
#' Format target
#'
#' Formats a target as a 0/1 variable. If target is numeric, 1 = above average.
#'
#' @param target Variable as vector
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
  } else if (is.numeric(target))  {
    result <- ifelse(target > mean(target, na.rm = TRUE), 1, 0)
  } else {
    result <- target
  }
} # format_target

#============================================================================
#  Function: replace_na_with
#============================================================================
#' Replace NA
#'
#' Replace NA values of a variable in a dataframe
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
#  format_type
#============================================================================
#' Format type description
#'
#' Format type description of varable to 3 letters (int|dbl|lgl|chr|dat)
#'
#' @param type Type description ("integer", "double", "logical", character", "date")
#' @return Formated type description (int|dbl|lgl|chr|dat)
#' @examples
#' format_type(typeof(iris$Species))
#' @export

format_type <- function(type) {
  if (type == "numeric")  {
    return("num")
  } else if (type == "integer")  {
    return("int")
  } else if (type == "double")  {
    return("dbl")
  } else if (type == "logical")  {
    return("lgl")
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

  if (var_class %in% c("Date", "POSIXct", "POSIXt"))  {
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
  if (class(var)[1] %in% c("numeric", "integer", "character", "logical", "Date", "POSIXct"))  {
    var_class <- class(var)[1]
  } else {
    return("oth")
  }
  ## intelligent guessing if num or cat (based on postfix of variable names)
  var_type <- typeof(var)
  # num with limited number of unique values is cat
  var_unique <- length(unique(var))
  # return result

  # treate Date always as cat
  if (var_class == "Date")  {
    return("cat")
  }

  # Decide on type and number of unique values
  if (var_type %in% c("integer", "double")) {
    if (var_unique < 10)  {
      return("cat")
    } else {
      return("num")
    }
  } else  {
    return("cat")
  }
} # guess_cat_num


#============================================================================
#  get_nrow()
#============================================================================
#' Get number of rows for a grid plot (deprecated, use total_fig_height() instead)
#'
#' @param varnames List of variables to be plotted
#' @param exclude Number of variables that will be excluded from plot
#' @param ncol Number of columns (default = 2)
#' @return Number of rows
#' @examples
#' get_nrow(names(iris), ncol = 2)
#' @export

get_nrow <- function(varnames, exclude = 0, ncol = 2)  {

  warning("get_nrow() is deprecated.\nPlease use total_fig_height() instead")

  n <- length(varnames) - exclude
  result <- ceiling(n / ncol)
  result
}

#============================================================================
#  total_fig_height()
#============================================================================
#' Get fig.height for RMarkdown-junk using explore_all()
#'
#' @param data A dataset
#' @param target Target variable
#' @param nvar Number of variables to plot
#' @param ncol Number of columns (default = 2)
#' @param size fig.height of 1 plot (default = 3)
#' @return Number of rows
#' @examples
#' total_fig_height(iris)
#' total_fig_height(iris, target = Species)
#' total_fig_height(nvar = 5)
#' @export

total_fig_height <- function(data, target, nvar = NA, ncol = 2, size = 3)  {

  if (!is.na(nvar)) {
    n_var <- nvar
    n <- n_var - ifelse(missing(target), 0, 1)
  } else {
    n_var <- ncol(data)
    n <- n_var - ifelse(missing(target), 0, 1)
  }
  result <- ceiling(n / ncol) * size
  result
}


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
#  Function: simplify_text
#============================================================================
#' Simplifies a text string
#'
#' A text string is converted into a simplified version by
#' trimming, converting to upper case, replacing german Umlaute,
#' dropping special characters like comma and semicolon and
#' replacing multiple spaces with one space.
#'
#' @param text text string
#' @return text string
#' @import dplyr
#' @examples
#' simplify_text(" Hello  World !, ")
#' @export

simplify_text <- function(text)  {

  # return original text if text is not character
  if (!is.character(text))  {
    return(text)
  }

  # simplify text
  text %>%
    stringr::str_trim() %>%
    stringr::str_to_upper() %>%
    stringr::str_replace_all("\u00C4", "AE") %>%
    stringr::str_replace_all("\u00D6", "OE") %>%
    stringr::str_replace_all("\u00DC", "UE") %>%
    stringr::str_replace_all("\u00DF", "SS") %>%
    stringr::str_replace_all("[^0-9A-Z@#:!?_\\s\\.\\-]", "") %>%
    stringr::str_replace_all("\\s+", " ")
}

#============================================================================
#  Function: rescale01
#============================================================================
#' Rescales a numeric variable into values between 0 and 1
#'
#' @param x numeric vector (to be rescaled)
#' @return vector with values between 0 and 1
#' @import dplyr
#' @examples
#' rescale01(0:10)
#' @export

rescale01 <- function(x)  {

  # rescale
    y <- x / ( max(x) - min(x) )
    y <- y - min(y)

  # return
  y
} # rescale

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
#' @param max_cat Maximum number of different factor levels for categorical variable (if more, .OTHER is added)
#' @param rescale01 Rescale into value between 0 and 1 (var must be numeric)
#' @param simplify_text if TRUE, a character variable is simplified (trim, upper, ...)
#' @param name New name of variable (as string)
#' @return Dataset
#' @import rlang
#' @import dplyr
#' @examples
#' clean_var(iris, Sepal.Width, max_val = 3.5, name = "sepal_width")
#' @export

clean_var <- function(data, var, na = NA, min_val = NA, max_val = NA, max_cat = NA, rescale01 = FALSE, simplify_text = FALSE, name = NA)  {

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

  # set levels based on max_cat
  if (!is.na(max_cat) & max_cat > 0)  {
    # factorise if necessary
    if (!is.factor(data[[var_txt]])) {
      data[[var_txt]] <- factor(data[[var_txt]])
    }

    # number of different levels of variable
    n_var_cat <- length(levels(data[[var_txt]]))

    # add level for NA (if in data)
    data[[var_txt]] <- forcats::fct_explicit_na(data[[var_txt]], na_level = ".NA")

    # keep max. different levels
    if (n_var_cat > max_cat)  {
      data[[var_txt]] <- forcats::fct_lump(data[[var_txt]], max_cat, other_level = ".OTHER")
    }
  } # if max_cat

  # simplify text
  if (simplify_text == TRUE & is.character(data[[var_txt]]))  {
    data[[var_txt]] <- simplify_text(data[[var_txt]])
  }

  # simplify text
  if (rescale01 == TRUE & is.numeric(data[[var_txt]]))  {
    data[[var_txt]] <- rescale01(data[[var_txt]])
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
#  count_pct
#============================================================================
#' Adds percentage to dplyr::count()
#'
#' Adds variables total and
#' pct (percentage) to dplyr::count()
#'
#' @param data A dataset
#' @param ... Other parameters passed to count()
#' @return Dataset
#' @import dplyr
#' @examples
#' count_pct(iris, Species)
#' @export

count_pct <- function(data, ...)  {

  d <- data %>%
    dplyr::count(...)

  names(d) <- c("value", "n")

  d <- d %>%
    dplyr::mutate(total = sum(n),
                  pct = n / sum(n) * 100.00)

  d
} # count_pct


