#' encrypt text
#'
#' @param text A text (character)
#' @param codeletters A string of letters that are used for encryption
#' @param shift Number of elements shifted
#' @return Encrypted text
#' @examples
#' encrypt("hello world")
#' @export

encrypt <- function (text, codeletters=c(toupper(letters),letters,0:9), shift=18)  {
  old=paste(codeletters,collapse="")
  new=paste(c(codeletters[(shift+1):nchar(old)],codeletters[1:shift]),collapse="")
  return (chartr(old,new,text))
}

#' decrypt text
#'
#' @param text A text (character)
#' @param codeletters A string of letters that are used for decryption
#' @param shift Number of elements shifted
#' @return Decrypted text
#' @examples
#' decrypt("zw336 E693v")
#' @export

decrypt <- function (text, codeletters=c(toupper(letters),letters,0:9), shift=18)  {
  old=paste(codeletters,collapse="")
  new=paste(c(codeletters[(shift+1):nchar(old)],codeletters[1:shift]),collapse="")
  return (chartr(new,old,text))
}

#' Balance target variable
#'
#' Balances the target variable in your dataset using downsampling.
#' Target must be 0/1, FALSE/TRUE ore no/yes
#'
#' @param data A dataset
#' @param target Target variable (0/1, TRUE/FALSE, yes/no)
#' @param min_prop Minimum proportion of one of the target categories
#' @param seed Seed for random number generator
#' @return Data
#' @examples
#' iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
#' balanced <- balance_target(iris, target = is_versicolor, min_prop = 0.5)
#' describe(balanced, is_versicolor)
#' @export

balance_target <- function(data, target, min_prop = 0.1, seed) {

  # check if parameters are missing
  check_data_frame_non_empty(data)
  rlang::check_required(target)

  # check if min_prop has a meaningful value
  check_number_decimal(min_prop, min = 0, max = 1)

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

    tmp_min <- data %>% dplyr::filter(!!target_quo == names(minClass))
    tmp_max <- data %>% dplyr::filter(!!target_quo != names(minClass))

    # set seed (if defined) to ensure reproducibility
    if (!missing(seed)) {set.seed(seed)}

    # sampling
    data_minClass <- tmp_min %>%
      dplyr::slice_sample(n = minClass)
    data_maxClass <- tmp_max %>%
      dplyr::slice_sample(n = maxClass)

    # mix it up
    data <- rbind(data_minClass, data_maxClass)
    if (!missing(seed)) {set.seed(seed)}
    data <- data %>% dplyr::slice_sample(n = nrow(data))

    # return
    data
    }
} # balance_target

#' Weight target variable
#'
#' Create weights for the target variable in your dataset
#' so that are equal weights for target = 0 and target = 1.
#' Target must be 0/1, FALSE/TRUE ore no/yes
#'
#' @param data A dataset
#' @param target Target variable (0/1, TRUE/FALSE, yes/no)
#' @return Weights for each observation (as a vector)
#' @examples
#' iris$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
#' weights <- weight_target(iris, target = is_versicolor)
#' versicolor <- iris$is_versicolor
#' table(versicolor, weights)
#' @export

weight_target <- function(data, target) {
  # check if parameters are missing
  check_data_frame_non_empty(data)
  rlang::check_required(target)

  # tidy eval for target
  target_quo <- enquo(target)
  target_txt <- quo_name(target_quo)[[1]]

  # check levels of target
  target_levels <- length(unique(data[[target_txt]]))
  if (target_levels > 2)  {
    stop(paste("target has", target_levels, "levels, expected 2"))
  }

  # weight
  observed_prop   <- data %>%
    dplyr::pull(!!target_quo) %>%
    table()
  minClass        <- min(observed_prop)
  names(minClass) <- names(which(observed_prop == minClass))

  weights = ifelse(data[[target_txt]] == names(minClass),
                   max(observed_prop)/min(observed_prop), 1)

  # return weights
  return(weights)

} # weight_target


#' Plot a text
#'
#' Plots a text (base plot) and let you choose text-size and color
#'
#' @param text Text as string
#' @param size Text-size
#' @param color Text-color
#' @param ggplot return a ggplot-object? (or base plot)
#' @return Plot
#' @examples
#' plot_text("hello", size = 2, color = "red")
#' @export

plot_text <- function(text="hello world", size=1.2, color="black", ggplot = FALSE)  {

  if (ggplot) {

    ggplot(NULL) +
      geom_blank() +
      geom_text(aes(x = 0, y = 0, label = text), size = 4 * size) +
      theme_void()
      #labs(title = var_txt, x = "", y = " ") +
    #  theme(axis.title.x=element_blank(),
    #        axis.text.x=element_blank(),
    #        axis.ticks.x=element_blank(),
    #        axis.title.y=element_blank(),
    #        axis.text.y=element_blank(),
    #        axis.ticks.y=element_blank(),
    #        plot.margin = unit(c(0.1,0.1,0.5,1), "cm")) #t,r,b,l
  } else {
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, text, cex = size, col = color)
  }
}

#' Plot a variable info
#'
#' Creates a ggplot with the variable-name as title and a text
#'
#' @param data A dataset
#' @param var Variable
#' @param info Text to plot
#' @return Plot (ggplot)

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

#' Plots a legend that can be used for explore_all with a binary target
#'
#' @param border Draw a border?
#' @return Base plot
#' @examples
#' plot_legend_targetpct(border = TRUE)
#' @export

plot_legend_targetpct <- function(border = TRUE) {

  graphics::par(mar=c(0,0,0,0))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  graphics::legend(0.5,0.5,
                   legend = c("00-05%", "06-20%", "21-40%","41+%"),
                   fill =c("#ECEFF1", "#CFD8DC", "#B0BEC5", "#90A4AE"),
                   horiz = TRUE,
                   xjust = 0.5,
                   yjust = 0.5,
                   border = TRUE,
                   box.lty = ifelse(border, 1, 0))

} # plot_legend_targetpct


#' Format number as character string (space as big.mark)
#'
#' Formats a big number using space as big.mark (1000 = 1 000)
#'
#' @param number A number (integer or real)
#' @param digits Number of digits
#' @return Formatted number as text
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

#' Format number as character string (kMB)
#'
#' Formats a big number as k (1 000), M (1 000 000) or B (1 000 000 000)
#'
#' @param number A number (integer or real)
#' @param digits Number of digits
#' @return Formatted number as text
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


#' Format number as character string (auto)
#'
#' Formats a number depending on the value as number with space, scientific or
#' big number as k (1 000), M (1 000 000) or B (1 000 000 000)
#'
#' @param number A number (integer or real)
#' @param digits Number of digits
#' @return Formatted number as text
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

#' Format type description
#'
#' Format type description of variable to 3 letters (int|dbl|lgl|chr|dat)
#'
#' @param type Type description ("integer", "double", "logical", character", "date")
#' @return Formatted type description (int|dbl|lgl|chr|dat)
#' @examples
#' format_type(typeof(iris$Species))
#' @export

format_type <- function(type) {
  if (type == "numeric")  {
    return("num")
  } else if (type == "integer")  {
    return("int")
  } else if (type == "integer64")  {
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

#' Return type of variable
#'
#' Return value of typeof, except if variable contains hide, then return "other"
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

  if (var_class %in% c("numeric", "integer", "integer64", "logical"))  {
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


#' Return if variable is categorical or numerical
#'
#' Guess if variable is categorical or numerical based on name, type and values of variable
#'
#' @param var A vector (dataframe column)
#' @param descr A description of the variable (optional)
#' @return "cat" (categorical), "num" (numerical) or "oth" (other)
#' @examples
#' guess_cat_num(iris$Species)
#' @export

guess_cat_num <- function(var, descr)  {
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
  if (class(var)[1] %in% c("numeric", "integer", "integer64", "character", "logical", "Date", "POSIXct"))  {
    var_class <- class(var)[1]
  } else {
    return("oth")
  }

  # variable type
  var_type <- typeof(var)

  # number of unique values
  if (missing(descr))  {
    var_unique <- length(unique(var))
  } else {
    var_unique <- descr$unique
  }

  # treat Date always as cat
  if (var_class == "Date")  {
    return("cat")
  }

  # Decide on type and number of unique values
  if (var_type %in% c("integer", "integer64", "double")) {
    if (var_unique < 10)  {
      return("cat")
    } else {
      return("num")
    }
  } else  {
    return("cat")
  }
} # guess_cat_num


#' Get number of rows for a grid plot
#'
#' This function is deprecated, please use [total_fig_height()] instead.
#'
#' @param varnames List of variables to be plotted
#' @param exclude Number of variables that will be excluded from plot
#' @param ncol Number of columns (default = 2)
#' @return Number of rows
#' @keywords internal
#' @examples
#' \dontrun{
#' get_nrow(names(iris), ncol = 2)
#' }
#' @export

get_nrow <- function(varnames, exclude = 0, ncol = 2)  {

  warning("get_nrow() is deprecated.\nPlease use total_fig_height() instead")

  n <- length(varnames) - exclude
  result <- ceiling(n / ncol)
  result
}

#' Get fig.height for RMarkdown-junk using explore_all()
#'
#' @param data A dataset
#' @param var_name_n Weights variable for count data? (TRUE / MISSING)
#' @param var_name_target Target variable (TRUE / MISSING)
#' @param nvar Number of variables to plot
#' @param ncol Number of columns (default = 2)
#' @param size fig.height of 1 plot (default = 3)
#' @return Number of rows
#' @examples
#' total_fig_height(iris)
#' total_fig_height(iris, var_name_target = "Species")
#' total_fig_height(nvar = 5)
#' @export

total_fig_height <- function(data, var_name_n, var_name_target,
                             nvar = NA, ncol = 2, size = 3)  {

  if (!is.na(nvar)) {
    n_var <- nvar
  } else {
    n_var <- ncol(data)
  }

  n_var <- n_var - ifelse(missing(var_name_target), 0, 1)
  n_var <- n_var - ifelse(missing(var_name_n), 0, 1)

  result <- ceiling(n_var / ncol) * size
  result
}

#' Put variables into "buckets" to create a set of plots instead one large plot
#'
#' @param data A dataset
#' @param bucket_size Maximum number of variables in one bucket
#' @param var_name_target Name of the target variable (if defined)
#' @param var_name_n Name of the weight (n) variable (if defined)
#' @return Buckets as a list
#' @examples
#' get_var_buckets(iris)
#' get_var_buckets(iris, bucket_size = 2)
#' get_var_buckets(iris, bucket_size = 2, var_name_target = "Species")
#' @export

get_var_buckets <- function(data, bucket_size = 100,
                            var_name_target = NA, var_name_n = NA) {

  target_defined <- !is.na(var_name_target)
  n_defined <- !is.na(var_name_n)

  # get variable names that can be used in explore-plots
  # if target or n is used, drop them
  names <- names(data)
  if (target_defined) {
    names <- names[names != var_name_target]
  }
  if (n_defined) {
    names <- names[names != var_name_n]
  }

  # initialize
  n_var <- length(names)
  n_bucket <- ceiling(length(names) / bucket_size)
  bucket_size_plot <- ceiling(n_var / n_bucket)

  bucket_var <- vector(mode = "list", length = n_bucket)

  # create buckets
  for (i in seq_len(n_bucket))  {

    start <- (i-1) * bucket_size_plot + 1
    end <- start + bucket_size_plot - 1
    if (end > n_var) {end <- n_var}

    bucket_var[[i]] <- names[start:end]

    if (target_defined) {
      bucket_var[[i]] <- c(bucket_var[[i]], var_name_target)
    }

    if (n_defined) {
      bucket_var[[i]] <- c(bucket_var[[i]], var_name_n)
    }


  } #for

  # return buckets
  bucket_var

} # get_var_buckets


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
  check_string(output_dir)

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


#' Simplifies a text string
#'
#' A text string is converted into a simplified version by
#' trimming, converting to upper case, replacing german Umlaute,
#' dropping special characters like comma and semicolon and
#' replacing multiple spaces with one space.
#'
#' @param text text string
#' @return text string
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

#' Rescales a numeric variable into values between 0 and 1
#'
#' @param x numeric vector (to be rescaled)
#' @return vector with values between 0 and 1
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
#' @param rescale01 IF TRUE, value is rescaled between 0 and 1 (var must be numeric)
#' @param simplify_text If TRUE, a character variable is simplified (trim, upper, ...)
#' @param name New name of variable (as string)
#' @return Dataset
#' @examples
#' library(magrittr)
#' iris %>% clean_var(Sepal.Width, max_val = 3.5, name = "sepal_width") %>% head()
#' iris %>% clean_var(Sepal.Width, rescale01 = TRUE) %>% head()
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
    if (any(is.na(data[[var_txt]])))  {
      levels(data[[var_txt]]) <- c(levels(data[[var_txt]]), ".NA")
      data[[var_txt]] <- ifelse(is.na(data[[var_txt]]), ".NA", data[[var_txt]])
    }

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
    if (name %in% var_names && name != var_txt)  {
      warning("variable ", name, " already exists in data. Did not rename, select other name!")
    } else {
      colnames(data)[colnames(data) == var_txt] <- name
    }
  }

  # return data
  data
} # clean_var

#' Adds percentage to dplyr::count()
#'
#' Adds variables total and
#' pct (percentage) to dplyr::count()
#'
#' @param data A dataset
#' @param ... Other parameters passed to count()
#' @return Dataset
#' @examples
#' count_pct(iris, Species)
#' @export

count_pct <- function(data, ...)  {

  d <- data %>%
    dplyr::count(...)

  #names(d) <- c("value", "n")

  d <- d %>%
    dplyr::mutate(total = sum(n),
                  pct = n / sum(n) * 100.00)

  d
} # count_pct

#' Show color vector as ggplot
#'
#' @param color Vector of colors
#' @return ggplot
#' @export
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate
#' @import ggplot2
#' @examples
#' show_color("gold")
#' show_color(c("blue", "red", "green"))

show_color <- function(color) {

  # undefined variables to pass CRAN checks
  color_id <- NULL
  val <- NULL

  if (is.list(color))  {
    color <- unlist(color)
  }

  if (is.null(names(color))) {
    names(color) <- seq_len(length(color))
  }

  data <- data.frame(
    color_id = names(color),
    color_hex = color,
    val = 100
  )

  data %>%
    mutate(color_id = factor(
      color_id,
      levels = names(color))) %>%
    ggplot(aes(color_id,
               val,
               fill = color_id)) +
    geom_col() +
    scale_fill_manual(values = color) +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_blank()) +
    theme(axis.text.x = element_blank())

} # show_color

#' Mix colors
#'
#' @param color1 Color 1
#' @param color2 Color 2
#' @param n Number of different colors that should be generated
#' @return Vector of color-codes
#' @export
#' @examples
#' mix_color("blue", n = 10)
#' mix_color("gold", "red", n = 4)

mix_color <- function(color1, color2 = NA, n = 5) {

  if (is.na(color2)) {
    colors <- grDevices::colorRampPalette(c("white", color1, "black"))(n + 2)
    colors <- colors[-1]  # drop first color (black)
    colors <- colors[-length(colors)]  # drop last color (white)
  } else {
    colors <- grDevices::colorRampPalette(c(color1, color2))(n)
  }

  # return colors as vector
  colors
} # mix_color

#' Get predefined colors
#'
#' @param name Name of color/color-vector
#' @param fill Fill color vector?
#' @param fill_color Color to use to fill color vector
#' @param fill_n Number of color codes to return
#' @return Vector of color-codes
#' @export
#' @examples
#' get_color("mario")
#'
#' get_color("mario")
#' show_color(get_color("mario"))
#' show_color(get_color("mario", fill = TRUE, fill_n = 10))
#'
#' col <- get_color("mario")
#' explore(iris, Sepal.Length, target = Species,
#'   color = col)
#' explore(iris, Sepal.Length, target = Species,
#'   color = c(col["peach"], col["bowser"], col["donkeykong"]))

get_color <- function(name, fill = FALSE, fill_color = "#DDDDDD", fill_n = 10) {

  color <- NULL

  color$a1 <- c("greylight" = "#a3a9b0", "red" = "#d32c1c", "blue" = "#5dbcd2",  "black" = "#000000", "greydark" = "#868e96")
#  color$amazon <- c("orange" = "#ff9900", "blue" = "#146eb4")
  color$apple <- c("green" = "#61bb46", "yellow" = "#fdb827", "orange"= "#f5821f", "red" = "#e03a3e", "violet" = "#963d97", "blue" = "#009ddc")
#  color$android <- c("green" = "#a4c639")
#  color$ferrari <- c("red" = "#e32119")
  color$google <- c("blue" = "#4285f4", "green" = "#34a853", "yellow"= "#fbbc05", "red" = "#ea4335")
#  color$ikea <- c("yellow" = "#ffcc00", "blue" = "#003399")
  color$mario <- c("mario" = "#e0102f", "luigi" = "#08a936", "peach" = "#f096be", "toad" = "#17419a", "bowser" = "#f8be10", "donkeykong" = "#742607")
#  color$nfl <- c("blue" = "#013369", "red" = "#d50a0a")
  color$python <- c("yellow" = "#ffde57", "blue" = "#4584b6", "grey" = "#646464")
  color$r <- c("blue" = "#2065b8")
  color$redbull <- c("yellow" = "#ffc906", "red" = "#cc1e4a", "blue" = "#223971", "bluedark" = "#121f45")
  color$slack <- c("blue" = "#36c5f0", "red" = "#e01e5a", "yellow" = "#ecb22e", "green" = "#2eb67d", "violet"= "#4a154b")
  color$ubuntu <- c("orange" = "#dd4814", "greydark" = "#333333", "greylight" = "#aea79f", "violet" = "#77216f")

  if (missing(name)) {
    return(color)
  }

  name <- tolower(name)
  color_vctr <- color[[name]]

  if (fill) {

    n_colors <- length(color_vctr)
    if (n_colors < fill_n) {
      color_add <- rep(fill_color, fill_n - n_colors)
      names(color_add) <- paste0("undef-", seq_along(color_add))
    }

    color_vctr <- c(color_vctr, color_add)

  }

  color_vctr

} #get_color

#' Cut a variable
#'
#' @param data Data frame
#' @param var Variable
#' @param bins Number of bins
#' @return Data frame

cut_vec_num_avg <- function(values, bins = 8)  {

  # define variables to pass CRAN checks
  grp_ <- NULL
  min_ <- NULL
  max_ <- NULL
  avg_ <- NULL

  # create bins
  cut_values <- cut(values, bins, labels = FALSE)

  # calc average
  data_cut <- data.frame(
    val = values,
    grp_ = cut_values)

  data_cut_avg <- data_cut %>%
    group_by(grp_) %>%
    summarize(min_ = min(val), max_ = max(val)) %>%
    ungroup() %>%
    mutate(avg_ = (max_ + min_)/2)

  data_cut <- data_cut %>%
    inner_join(data_cut_avg, by = "grp_")

  # return result
  data_cut[["avg_"]]

} ## cut_var
