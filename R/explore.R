#' Explore categorical variable + target
#'
#' Create a plot to explore relation between categorical variable and a binary target
#'
#' @param data A dataset
#' @param var Categorical variable
#' @param target Target variable (0/1 or FALSE/TRUE)
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
#' @importFrom utils head
#' @import dplyr
#' @import ggplot2

target_explore_cat <- function(data, var, target = "target_ind", min_val = NA, max_val = NA, flip = TRUE, num2char = TRUE, title = NA, auto_scale = TRUE, na = NA, max_cat = 30, legend_position = "bottom") {

  # definitions for CRAN package check
  n_target <- NULL
  n_pct <- NULL
  weight <- NULL
  target_pct <- NULL
  num <- NULL

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
  data_bar <- data %>%
    select(!!var_quo, !!target_quo)
  names(data_bar) <- c("cat", "target")

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

  # maximum percent value to be displayed
  max_pct <- max(data_bar$target_pct)

  # create plot
  plot_bar <- ggplot(data = data_bar) +
    geom_bar(aes(x=cat, y=target_pct, fill=weight), stat="identity") +
    theme(
      panel.background = element_rect("white"),
      panel.grid.major = element_line("grey85"),
      panel.grid.minor = element_line("grey85"),
      panel.border = element_rect(fill = NA, color = "lightgrey")) +
    theme(plot.margin=unit(c(0.5,0.5,0,1),"cm")) +   # o,r,u,l
    ggtitle(ifelse(is.na(title), var_txt, title)) +
    labs(x = "", y = "% target") +
    scale_fill_manual(name = "observations", values = bar_col) +
    theme(legend.position = legend_position) +
    geom_hline(yintercept = target_mean,
               color = "#7f7f7f", alpha = 0.5,
               linetype = "dashed", size = 1)

  # flip plot?
  if(flip)  {

      plot_bar <- plot_bar +
        geom_text(aes(x=cat, y=target_pct,
                      label = round(target_pct,1),
                      hjust = ifelse(target_pct < max_pct/10, -0.1, 1)),
                  position = position_dodge(width = 1),
                  vjust = 0.5,
                  size = 3.0,
                  color = "#525252") +
        coord_flip()

  } else {

    plot_bar <- plot_bar +
      geom_text(aes(x=cat, y=target_pct,
                    label = round(target_pct,1),
                    vjust = ifelse(target_pct < max_pct/10, -0.2, 1)),
                position = position_dodge(width = 1),
                hjust = 0.5,
                size = 3.0,
                color = "#525252")
  } # if flip

  # save result
  # result <- list(data_bar, plot_bar)
  # names(result) <- c("data","plot")

  return(plot_bar)

} # target_explore_cat

#' Explore categorical variable + target
#'
#' Create a plot to explore relation between numerical variable and a binary target
#'
#' @param data A dataset
#' @param var Numerical variable
#' @param target Target variable (0/1 or FALSE/TRUE)
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

target_explore_num <- function(data, var, target = "target_ind", min_val = NA, max_val = NA, flip = TRUE, title = NA, auto_scale = TRUE, na = NA, legend_position = "bottom") {

  # definitions for CRAN package check
  num <- NULL
  cat_label <- NULL
  explore_cat <- NULL

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
  data_bar <- data %>%
    select(!!var_quo, !!target_quo)
  names(data_bar) <- c("num", "target")

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
                               cat_label,
                               target,
                               flip = FALSE,
                               num2char = FALSE,
                               legend_position = legend_position,
                               title = ifelse(is.na(title),
                                              paste0(var_txt),
                                              title)
  )

  return(result)

} # target_explore_num

#' Explore categorical variable using bar charts
#'
#' Create a barplot to explore a categorical variable.
#' If a target is selected, the barplot is created for all levels of the target.
#'
#' @param data A dataset
#' @param var variable
#' @param target target (can have more than 2 levels)
#' @param flip Should plot be flipped? (change of x and y)
#' @param title Title of the plot (if empty var name)
#' @param numeric Display variable as numeric (not category)
#' @param max_cat Maximum number of categories to be plotted
#' @param max_target_cat Maximum number of categories to be plotted for target (except NA)
#' @param legend_position Position of the legend ("bottom"|"top"|"none")
#' @param label Show labels? (if empty, automatic)
#' @param label_size Size of labels
#' @param ... Further arguments
#' @return Plot object (bar chart)
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @import ggplot2
#' @export

explore_bar <- function(data, var, target, flip = NA, title = "", numeric = NA, max_cat = 30, max_target_cat = 5, legend_position = "right", label, label_size = 2.7, ...)  {

  # define variables for CRAN-package check
  na_ind <- NULL
  target_n <- NULL
  pct <- NULL

  # check parameter data
  assertthat::assert_that(!missing(data), msg = "expect a data table to explore")
  assertthat::assert_that(is.data.frame(data), msg = "expect a table of type data.frame")
  assertthat::assert_that(nrow(data) > 0, msg = "data has 0 observations")

  # parameter var
  if(!missing(var))  {
    var_quo <- enquo(var)
    var_txt <- quo_name(var_quo)[[1]]
    if (!var_txt %in% names(data)) {
      stop(paste0("variable '", var_txt, "' not found"))
    }
  } else {
    stop(paste0("variable missing"))
  }

  if(nrow(data) == 0) {
    p <- data %>% plot_var_info(!!var_quo, "no observations")
    return(p)
  }

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
    if (!target_txt %in% names(data)) {
      stop(paste0("target variable '", target_txt, "' not found"))
    }
  } else {
    target_txt = NA
  }

  # number of levels of target
  if (missing(target))  {
    n_target_cat <- 1
  } else {
    n_target_cat <- length(unique(data[[target_txt]]))
  }

  # guess flip
  if (missing(flip)) {
    if(is.numeric(data[[var_txt]])) {
      flip <- FALSE
    } else {
      flip <- TRUE
    }
  }

  # check NA
  na_check <- data %>%
    mutate(na_ind = ifelse(is.na(!!var_quo),1,0)) %>%
    summarize(na_cnt = sum(na_ind), na_pct = sum(na_ind)/n())
  na_cnt <- na_check[1,1]
  na_pct <- na_check[1,2]

  # number of levels of var (if not numeric)
  var_cat <- data %>% count(!!var_quo) %>% pull(!!var_quo)
  if ( (missing(numeric) | (!missing(numeric) & (numeric == FALSE))) &
      length(var_cat) > max_cat)  {
    data <- data %>% filter(!!var_quo %in% var_cat[1:max_cat])
    warning(paste("number of bars limited to", max_cat, "by parameter max_cat"))
  }

  # numeric? of use a factor for var if low number of cats
  if (!missing(numeric) & numeric == TRUE)  {
    data[[var_txt]] <- as.numeric(data[[var_txt]])
    if (missing(flip)) {
      flip <- FALSE
    }
  } else if ((!missing(numeric) & numeric == FALSE) |
             guess_cat_num(data[[var_txt]]) == "cat") {
    data[[var_txt]] <- factor(data[[var_txt]])
    data[[var_txt]] <- forcats::fct_explicit_na(data[[var_txt]], na_level = ".NA")
    if (missing(flip)) {
      flip <- TRUE
    }
  } # if

  # use a factor for target so that fill works
  if (n_target_cat > 1 && !is.factor(data[[target_txt]]))  {
    data[[target_txt]] <- factor(data[[target_txt]])
    data[[target_txt]] <- forcats::fct_explicit_na(data[[target_txt]], na_level = ".NA")

    # keep max. different levels
    if (n_target_cat > max_target_cat)  {
      data[[target_txt]] <- forcats::fct_lump(data[[target_txt]],max_target_cat, other_level = ".OTHER")
    }
    # recalculate number of levels in target
    n_target_cat <- length(levels(data[[target_txt]]))

  }

  # if no label parameter, decide on
  # number of bars if labels are plotted
  bars <- length(unique(data[[var_txt]])) * n_target_cat
  if (missing(label)) {
    if (bars <= 20)  {
      label <- TRUE
    } else {
      label <- FALSE
    }
  }

  # prepare + plot (with target)

  if (n_target_cat > 1)  {
    data_target <- data %>%
      group_by(!!target_quo) %>%
      summarise(target_n = n())

    data_var <- data %>%
      group_by(!!target_quo, !!var_quo) %>%
      summarise(n = n())

    data_bar <- data_var %>%
      inner_join(data_target, by = target_txt) %>%
      mutate(pct = round(n / target_n * 100.0, 1))

    max_pct = max(data_bar$pct)

    # plot
    p <- ggplot(data_bar, aes(x = !!var_quo)) +
      geom_col(aes(y = pct, fill = !!target_quo), position = "dodge") +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey")) +
      theme(legend.position = legend_position) +
      labs(x = "", y = "%")

  } else {

    # prepare + plot (no target)

    data_bar <- data %>%
      group_by(!!var_quo) %>%
      summarise(n = n()) %>%
      mutate(pct = round(n / sum(n) * 100.0, 1))

    max_pct = max(data_bar$pct)

    # plot
    p <- ggplot(data_bar, aes(x = !!var_quo)) +
      geom_col(aes(y = pct),
               position = "dodge",
               fill = "lightgrey",
               color = "lightgrey") +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey")) +
      labs(x = "", y = "%")

  }

  # color manual
  if (n_target_cat == 2)  {
    p <- p + scale_fill_manual(values = c("#CFD8DC","#90A4AE"))
  }

  # plot labels?

  # >1 cat, FLIP == TRUE
  if (label == TRUE & n_target_cat > 1 & flip == TRUE)  {
    p <- p + geom_text(aes(y = pct,
                           label = pct,
                           group = !!target_quo,
                           hjust = ifelse(pct < max_pct/10, -0.1, 1)
                       ),
                       position = position_dodge(width = 1),
                       vjust = 0.5,
                       size = label_size)
  }

  # >1 cat, FLIP == FALSE
  if (label == TRUE & n_target_cat > 1 & flip == FALSE)  {
    p <- p + geom_text(aes(y = pct,
                           label = pct,
                           group = !!target_quo,
                           vjust = ifelse(pct < max_pct/10, -0.3, 1)
    ),
    position = position_dodge(width = 0.9),
    hjust = 0.5,
    size = label_size)
  }

  # 1 cat, flip == TRUE
  if (label == TRUE & n_target_cat == 1 & flip == TRUE) {
    p <- p + geom_text(aes(y = pct,
                           label = pct,
                           hjust = ifelse(pct < max_pct/10, -0.1, 1)),
                       position = position_dodge(width = 0.9),
                       vjust = 0.5,
                       size = label_size)
  }

  # 1 cat, flip == FALSE
  if (label == TRUE & n_target_cat == 1 & flip == FALSE) {
    p <- p + geom_text(aes(y = pct,
                           label = pct,
                           vjust = ifelse(pct < max_pct/10, -0.5, 1)),
                       position = position_dodge(width = 0.9),
                       hjust = 0.5,
                       size = label_size)
  }

  # title
  if (!is.na(title) & nchar(title) > 0)  {
    p <- p + ggtitle(title)
  } else if (n_target_cat == 1) {
    p <- p + ggtitle(paste0(var_txt, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)"))
  } else {
    p <- p + ggtitle(paste0(var_txt))
  }

  # flip plot
  if (is.na(flip) | flip) {
    p <- p + coord_flip()
  }

  # plot result
  p

} # explore_bar

#' Explore density of variable
#'
#' Create a density plot to explore numerical variable
#'
#' @param data A dataset
#' @param var Variable
#' @param target Target variable (0/1 or FALSE/TRUE)
#' @param title Title of the plot (if empty var name)
#' @param min_val All values < min_val are converted to min_val
#' @param max_val All values > max_val are converted to max_val
#' @param color Color of plot
#' @param auto_scale Use 0.02 and 0.98 percent quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param max_target_cat Maximum number of levels of target shown in the plot (except NA).
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

explore_density <- function(data, var, target, title = "", min_val = NA, max_val = NA, color = "grey", auto_scale = TRUE, max_target_cat = 5, ...)   {

  # check parameter data
  assertthat::assert_that(!missing(data), msg = "expect a data table to explore")
  assertthat::assert_that(is.data.frame(data), msg = "expect a table of type data.frame")
  assertthat::assert_that(nrow(data) > 0, msg = "data has 0 observations")

  # parameter var
  if(!missing(var))  {
    var_quo <- enquo(var)
    var_txt <- quo_name(var_quo)[[1]]
    if (!var_txt %in% names(data)) {
      stop(paste0("variable '", var_txt, "' not found"))
    }
  } else {
    stop(paste0("variable missing"))
  }

  if(nrow(data) == 0) {
    p <- data %>% plot_var_info(!!var_quo, "no observations")
    return(p)
  }


  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
    if (!target_txt %in% names(data)) {
      stop(paste0("target variable '", target_txt, "' not found"))
    }
  } else {
    target_txt = NA
  }

  # define variables for CRAN-package check
  var_ <- NULL
  na_ind <- NULL
  target_ <- NULL

  # number of levels of target
  if (missing(target))  {
    n_target_cat = 1
  } else {
    n_target_cat <- length(unique(data[[target_txt]]))
  }

  # count NA
  na_check <- data %>%
    mutate(na_ind = ifelse(is.na(!!var_quo),1,0)) %>%
    summarize(na_cnt = sum(na_ind), na_pct = sum(na_ind)/n())
  na_cnt <- na_check[1,1]
  na_pct <- na_check[1,2]

  # autoscale (if mni_val and max_val not used)
  if (auto_scale == TRUE & is.na(min_val) & is.na(max_val))  {
    r <- quantile(data[[var_txt]], c(0.02, 0.98), na.rm = TRUE)
    min_val = r[1]
    max_val = r[2]
  }

  # trim min, max
  if (!is.na(min_val)) data <- data %>% filter(!!var_quo >= min_val)
  if (!is.na(max_val)) data <- data %>% filter(!!var_quo <= max_val)

   if (is.na(target_txt))  {

    # plot denisity var, no target
    p <- data %>%
      ggplot(aes(!!var_quo)) +
      geom_density(fill = color, alpha = 0.7) +
      #ggtitle(paste0(var_txt, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)")) +
      labs(x = "", y = "") +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey"))

  } else {

    # factorise target
    if (!is.factor(data[[target_txt]]))  {
      data[[target_txt]] <- factor(data[[target_txt]])
      data[[target_txt]] <- forcats::fct_explicit_na(data[[target_txt]], na_level = ".NA")
      # keep max. different levels
      if (n_target_cat > max_target_cat)  {
        data[[target_txt]] <- forcats::fct_lump(data[[target_txt]],max_target_cat, other_level = ".OTHER")
      }
    }

    # create plot var + target
    p <- data %>%
      ggplot(aes(!!var_quo, fill = !!target_quo)) +
      geom_density(alpha = 0.7) +
      #ggtitle(paste0(var_txt, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)")) +
      #ggtitle(var_txt) +
      labs(x = "", y = "") +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey"))


    # target with 2 levels
    if (n_target_cat == 2)  {
       p <- p + scale_fill_manual(values = c("#CFD8DC","#90A4AE"), name = target_txt)
    }

  } # if

  # title
  if (!is.na(title) & nchar(title) > 0)  {
    p <- p + ggtitle(title)
  } else if (is.na(target_txt)) {
    p <- p + ggtitle(paste0(var_txt, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)"))
  } else {
    p <- p + ggtitle(paste0(var_txt))
  }

  # plot
  p

} # explore_density

#' Explore all variables
#'
#' Explore all variables of a dataset (create plots)
#'
#' @param data A dataset
#' @param n Weights variable (only for count data)
#' @param target Target variable (0/1 or FALSE/TRUE)
#' @param ncol Layout of plots (number of columns)
#' @param split Split by target (TRUE|FALSE)
#' @return Plot
#' @import rlang
#' @importFrom gridExtra grid.arrange
#' @examples
#' explore_all(iris)
#'
#' iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
#' explore_all(iris, target = is_virginica)
#' @export

explore_all <- function(data, n, target, ncol = 2, split = TRUE)  {

  # check parameter data
  assertthat::assert_that(!missing(data), msg = "expect a data table to explore")
  assertthat::assert_that(is.data.frame(data), msg = "expect a table of type data.frame")
  assertthat::assert_that(nrow(data) > 0, msg = "data has 0 observations")

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
    guess_target <- guess_cat_num(data[[target_txt]])
  } else {
    target_txt = NA
    guess_target = "oth"
  }

  # parameter n
  if(!missing(n))  {
    n_quo <- enquo(n)
    n_txt <- quo_name(n_quo)[[1]]
  } else {
    n_txt = NA
  }

  # variable name of target
  var_name_target = target_txt

  # names of variables in data
  var_names <- names(data)

  # if target_explore is used, ignore target variable
  if (!is.na(var_name_target)) {
    var_names <- var_names[var_names != var_name_target]
  }

  # if n is used, ignore n variable
  if (!is.na(n_txt)) {
    var_names <- var_names[var_names != n_txt]
  }

  #pre define list of plots
  plots <- list(mode = "list", length = length(var_names))

  #cat("creating plots")
  # create plot for each variable
  for(i in seq_along(var_names))  {

    #cat(".")

    var_name <- var_names[i]

    # reduce variables of data (to improve speed and memory)
    if (is.na(var_name_target) & is.na(n_txt)) {
      data_tmp <- data[var_name]
    }
    else if (!is.na(var_name_target) & is.na(n_txt)) {
      data_tmp <- data[c(var_name, var_name_target)]
    }
    else if (is.na(var_name_target) & !is.na(n_txt)) {
      data_tmp <- data[c(var_name, n_txt)]
    }
    else if (!is.na(var_name_target) & !is.na(n_txt)) {
      data_tmp <- data[c(var_name, n_txt, var_name_target)]
    }

    # intelligent guessing if num or cat
    # based on postfix and type of variable names
    var_type <- guess_cat_num(data_tmp[[var_name]])

    # count data, no target
    if (!is.na(n_txt) & (is.na(var_name_target)))  {
      plots[[i]] <- explore_count(data_tmp, !!sym(var_name), n = !!n_quo, pct = TRUE)

    # count data, target
    } else if (!is.na(n_txt) & (!is.na(var_name_target)))  {
        plots[[i]] <- explore_count(data_tmp, !!sym(var_name), n = !!n_quo, target = !!target_quo, split = split)

    # no target, num
    } else if ( (var_type == "num") & (is.na(var_name_target))) {
      plots[[i]] <- explore_density(data_tmp, !!sym(var_name))

      # no target, cat
    } else if ( (var_type == "cat") & is.na(var_name_target) ) {
      plots[[i]] <- explore_bar(data_tmp, !!sym(var_name))

      # num target, num -> explore_cor
    } else if ( (var_type == "num") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (guess_target == "num"))  {
      plots[[i]] <- explore_cor(data_tmp, x = !!sym(var_name), y = !!target_quo, title = var_name)

      # num target, cat -> explore_cor
    } else if ( (var_type == "cat") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (guess_target == "num"))  {
      plots[[i]] <- explore_cor(data_tmp, y = !!sym(var_name), x = !!target_quo, title = var_name)

      # target, num
    } else if ( (var_type == "num") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (split == FALSE))  {
      plots[[i]] <- target_explore_num(data_tmp, !!sym(var_name), target = !!target_quo, legend_position = "none")

      # target, num, split
    } else if ( (var_type == "num") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (split == TRUE))  {
      plots[[i]] <- explore_density(data_tmp, !!sym(var_name), target = !!target_quo)

      # target, cat
    } else if ( (var_type == "cat") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (split == FALSE)) {
      plots[[i]] <- target_explore_cat(data_tmp, !!sym(var_name), target = !!target_quo, legend_position = "none")

      # target, cat, split
    } else if ( (var_type == "cat") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (split == TRUE)) {
      plots[[i]] <- explore_bar(data_tmp, !!sym(var_name), target = !!target_quo)

    } else {
      plots[[i]] <- plot_var_info(data_tmp, !!var_name, info = "can't explore\n(data type not supported)")
    } # if
  } # for

  #cat("\n")
  gridExtra::grid.arrange(grobs = plots, ncol = ncol)

} # explore_all

#' Explore the correlation between two variables
#'
#' @param data A dataset
#' @param x Variable on x axis
#' @param y Variable on y axis
#' @param target Target variable (categorical)
#' @param bins Number of bins
#' @param min_val All values < min_val are converted to min_val
#' @param max_val All values > max_val are converted to max_val
#' @param auto_scale Use 0.2 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param title Title of the plot
#' @param color Color of the plot
#' @param ... Further arguments
#' @return Plot
#' @examples
#' explore_cor(iris, x = Sepal.Length, y = Sepal.Width)
#' @export

explore_cor <- function(data, x, y, target, bins = 8, min_val = NA, max_val = NA, auto_scale = TRUE, title = NA, color = "grey", ...)  {

  # check parameter data
  assertthat::assert_that(!missing(data), msg = "expect a data table to explore")
  assertthat::assert_that(is.data.frame(data), msg = "expect a table of type data.frame")
  assertthat::assert_that(nrow(data) > 0, msg = "data has 0 observations")

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

  if(nrow(data) == 0) {
    p <- data %>% plot_var_info(!!x_quo, "no observations")
    return(p)
  }

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    target_txt = NA
  }

  # describe x, y
  x_descr <- data %>% describe(!!x_quo, out = "list")
  y_descr <- data %>% describe(!!y_quo, out = "list")

  # guess cat/num x,y
  x_type = guess_cat_num(data[[x_txt]], descr = x_descr)
  y_type = guess_cat_num(data[[y_txt]], descr = y_descr)

  # guess cat/num target
  if (!is.na(target_txt)) {
    target_type = guess_cat_num(data[[target_txt]])
  }
  else {
    target_type = "oth"
  }

  # auto_scale?
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

  # use geom_point?
  use_points <- ifelse(
      x_type == "num" & y_type == "num" &
      nrow(data) <= 500 &
      x_descr$unique / nrow(data) >= 0.1 &
      y_descr$unique / nrow(data) >= 0.1,
      TRUE,
      FALSE
  )

  if(x_type == "num" & y_type == "num" & use_points)  {

    if (!is.na(target_txt)) {

    p <- data %>%
      ggplot(aes(x = !!x_quo, y = !!y_quo, color = !!target_quo)) +
      geom_point(alpha = 0.45, size = 2.5) +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey"))

    } else {

      p <- data %>%
        ggplot(aes(x = !!x_quo, y = !!y_quo)) +
        geom_point(alpha = 0.45, size = 2.5) +
        theme(
          panel.background = element_rect("white"),
          panel.grid.major = element_line("grey85"),
          panel.grid.minor = element_line("grey85"),
          panel.border = element_rect(fill = NA, color = "lightgrey"))

    }
  }

  else if(x_type == "num" & y_type == "num" & !use_points)  {

    # boxplot (x = num, y = num)
    p <- data %>%
      # cut only when more then 1 different value in data
      ggplot(aes(x = !!x_quo, y = !!y_quo)) +
      geom_boxplot(aes(group = cut(!!x_quo, bins)), fill = color) +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey"))

  }

  else if(x_type == "cat" & y_type == "num") {

    data[[x_txt]] <- as.factor(data[[x_txt]])

    # boxplot (x = cat)
    p <- data %>%
      ggplot(aes(x = !!x_quo, y = !!y_quo)) +
      geom_boxplot(aes(group = !!x_quo), fill = color) +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey"))

  }

  else if(x_type == "num" & y_type == "cat") {

    data[[y_txt]] <- as.factor(data[[y_txt]])

    # boxplot (x = cat)
    p <- data %>%
      ggplot(aes(x = !!y_quo, y = !!x_quo)) +
      geom_boxplot(aes(group = !!y_quo), fill = color) +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey")) +
      coord_flip()
  }

  else if(x_type == "cat" & y_type == "cat") {

    data[[x_txt]] <- as.factor(data[[x_txt]])
    data[[y_txt]] <- as.factor(data[[y_txt]])

    p <- data %>%
      ggplot(aes(x = !!x_quo, fill = !!y_quo)) +
      geom_bar(position = "fill") +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey"))
  }

  # facet
  if(!is.na(target_txt) & (target_type == "cat") & !use_points) {
    p <- p + facet_grid(vars(!!target_quo))
  }

  # title
  if (!is.na(title) & nchar(title) > 0)  {
    p <- p + ggtitle(title)
  }

  # plot grafic
  p

} # explore_cor

#' Explore table
#'
#' Explore a table. Plots variable types, variables with no variance and variables with NA
#'
#' @param data A dataset
#' @param n Weight variable for count data
#' @importFrom magrittr "%>%"
#' @import dplyr
#' @examples
#' explore_tbl(iris)
#' @export

explore_tbl <- function(data, n)  {

  # define variables to pass CRAN-checks
  type <- NULL
  na <- NULL
  measure <- NULL

  # check parameter data
  assertthat::assert_that(!missing(data), msg = "expect a data table to explore")
  assertthat::assert_that(is.data.frame(data), msg = "expect a table of type data.frame")
  assertthat::assert_that(nrow(data) > 0, msg = "data has 0 observations")

  # parameter n
  if (!missing(n)) {
    n_quo <- enquo(n)
    n_txt <- quo_name(n_quo)[[1]]
    if (!n_txt %in% names(data))  {
      stop(paste0("variable '", n_txt, "' not found"))
    }
    info_obs <- paste("with", format_num_kMB(nrow(data)), "observations")
  } else {
    n_quo <- NA
    n_txt <- NA
  }

  # info text for observations
  if (is.na(n_txt)) {
    info_obs <- paste0("with ", format_num_kMB(nrow(data)), " observations")
  } else {
    total_nrow <- sum(data[[n_txt]])
    info_obs <- paste0("with ", format_num_kMB(total_nrow), " observations (",
                      nrow(data), " in raw data)")
  }

  # describe data
  d <- describe_all(data)

  # number of variables in data
  n_var <- nrow(d)

  # prepare "all variables"
  bar1 <- d %>%
    count(type) %>%
    mutate(
      measure = "all",
      n_pct = n / n_var * 100
    )

  # prepare "no variance"
  suppressWarnings(
    bar2 <- d %>%
      filter(type != "oth") %>%
      filter(unique == 1) %>%
      count(type) %>%
      mutate(
        measure = "no variance",
        n_pct = n / n_var * 100
      )
  )

  # prepare "with NA"
  suppressWarnings(
    bar3 <- d %>%
      filter(na > 0) %>%
      count(type) %>%
      mutate(
        measure = "with NA",
        n_pct = n / n_var * 100
      )
  )

  # prepare plot
  bar <- bind_rows(bar1, bar2, bar3)
  type_default <- min(as.character(bar$type), na.rm = TRUE)
  bar <- bar %>% clean_var(type, na = type_default)
  bar$type <- factor(bar$type, levels = c("lgl","int","dbl","fct","chr","dat","oth"))

  # define colors
  color_mapping <- c("lgl" = "blue",
                     "int" = "cornflowerblue",
                     "dbl" = "cyan",
                     "fct" = "yellow",
                     "chr" = "orange",
                     "dat" = "brown",
                     "oth" = "red")
  # plot
  bar %>%
    ggplot(aes(measure, n, fill = type)) +
    geom_col() +
    scale_fill_manual(values = color_mapping) +
    #geom_text(aes(measure, n, group = type, label = as.character(n)), size = 2.5) +
    geom_text(aes(label = n, hjust = ifelse(n == 0, 0, 1)),
              position = "stack"
              ) +
    labs(title = paste(ncol(data), "variables"),
         subtitle = info_obs,
         y = "variables",
         x = "") +
    coord_flip() +
    theme(
      panel.background = element_rect("white"),
      panel.grid.major = element_line("grey85"),
      panel.grid.minor = element_line("grey85"),
      panel.border = element_rect(fill = NA, color = "lightgrey"))

} # explore_tbl

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

  # get variable types
  # tbl_guesstarget <- describe(data) %>%
  #   filter(unique <= 2) %>%
  #   filter((type %in% c("lgl","int","dbl","num") &
  #             (min == 0 | min == FALSE) &
  #             (max == 1 | max == TRUE)) |
  #             (type == "fct") ) %>%
  #   select(variable)
  # guesstarget <- as.character(tbl_guesstarget[[1]])

   tbl_guesstarget <- describe(data) %>%
     filter(type %in% c("lgl","int","dbl","num","fct","chr")) %>%
     select(variable)
  guesstarget <- as.character(tbl_guesstarget[[1]])

  # check all variables if usable
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
                           label = "variable",
                           choices = names(data),
                           selected = "disp"),
        shiny::checkboxInput(inputId = "auto_scale", label="auto scale", value=TRUE),
        shiny::checkboxInput(inputId = "split", label="split by target", value=TRUE),
        shiny::hr(),
        shiny::actionButton(inputId = "report", "report all")
        , width = 3),  #sidebarPanel
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("variable",
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
      #path <- getwd()
      output_dir <- normalizePath(path.expand(tempdir()))
      output_file <- "report_explore.html"

      # show waiting-window
      shiny::showModal(modalDialog("Generating report ... (this may take a while)", footer = NULL))

      # check if explore package is loaded
      run_explore_package <- ifelse(max(search() == "package:explore") == 1, TRUE, FALSE)

      # report only variables
      if(input$target == "<no target>")  {
        input_file <- ifelse(run_explore_package,
                             system.file("extdata", "template_report_variable.Rmd", package="explore"),
                             "C:/R/template_report_variable.Rmd")
        rmarkdown::render(input = input_file, output_file = output_file, output_dir = output_dir)

        # report target with split
      } else if(input$split == TRUE)  {
        input_file <- ifelse(run_explore_package,
                             system.file("extdata", "template_report_target_split.Rmd", package="explore"),
                             "C:/R/template_report_target_split.Rmd")
        rmarkdown::render(input = input_file, output_file = output_file, output_dir = output_dir)

        # report target with percent
      } else {
        input_file <- ifelse(run_explore_package,
                             system.file("extdata", "template_report_target_pct.Rmd", package="explore"),
                             "C:/R/template_report_target_pct.Rmd")
        rmarkdown::render(input = input_file, output_file = output_file, output_dir = output_dir)
      }

      # ready
      shiny::removeModal()

      # show Report
      browseURL(paste0("file://", file.path(output_dir, output_file)), browser = NULL)
    })

    output$graph_target <- shiny::renderPlot({
      if(input$target != "<no target>" & input$var != input$target)  {
        data %>% explore(!!sym(input$var), target = !!sym(input$target), auto_scale = input$auto_scale, split = input$split)
      }
    }) # renderPlot graph_target

    output$graph_explain <- shiny::renderPlot({
      if(input$target != "<no target>") {
        if (ncol(data) > 20) {
          # show waiting-window
          shiny::showModal(modalDialog("Growing tree ... (this may take a while)", footer = NULL))
          # grow decision tree
          data %>% explain_tree(target = !!sym(input$target), size=0.9)
          # ready
          shiny::removeModal()
        } else {
          # grow decision tree
          data %>% explain_tree(target = !!sym(input$target), size=0.9)
        } # if ncol
      } # if input$target
    }) # renderPlot graph_explain

    output$graph <- shiny::renderPlot({
      data %>% explore(!!sym(input$var), auto_scale = input$auto_scale)
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

#' Explore a dataset or variable
#'
#' @param data A dataset
#' @param var A variable
#' @param var2 A variable for checking correlation
#' @param n A Variable for number of observations (count data)
#' @param target Target variable (0/1 or FALSE/TRUE)
#' @param targetpct Plot variable as target% (FALSE/TRUE)
#' @param split Alternative to targetpct (split = !targetpct)
#' @param min_val All values < min_val are converted to min_val
#' @param max_val All values > max_val are converted to max_val
#' @param auto_scale Use 0.2 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param na Value to replace NA
#' @param ... Further arguments (like flip = TRUE/FALSE)
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

explore <- function(data, var, var2, n, target, targetpct, split, min_val = NA, max_val = NA, auto_scale = TRUE, na = NA, ...)  {

  # check parameter data
  assertthat::assert_that(!missing(data), msg = "expect a data table to explore")
  assertthat::assert_that(is.data.frame(data), msg = "expect a table of type data.frame")
  assertthat::assert_that(nrow(data) > 0, msg = "data has 0 observations")

  # parameter var
  if (!missing(var)) {
    var_quo <- enquo(var)
    var_text <- quo_name(var_quo)[[1]]
    if (!var_text %in% names(data))  {
      stop(paste0("variable '", var_text, "' not found"))
    }
  } else {
    var_quo <- NA
    var_text <- NA
  }

  # parameter var2
  if (!missing(var2)) {
    var2_quo <- enquo(var2)
    var2_text <- quo_name(var2_quo)[[1]]
    if (!var2_text %in% names(data))  {
      stop(paste0("variable '", var2_text, "' not found"))
    }
  } else {
    var2_quo <- NA
    var2_text <- NA
  }

  # parameter n
  if (!missing(n)) {
    n_quo <- enquo(n)
    n_text <- quo_name(n_quo)[[1]]
    if (!n_text %in% names(data))  {
      stop(paste0("variable '", n_text, "' not found"))
    }
  } else {
    n_quo <- NA
    n_text <- NA
  }

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_text <- quo_name(target_quo)[[1]]
    if (!target_text %in% names(data))  {
      stop(paste0("target variable '", target_text, "' not found"))
    }
    guess_target <- guess_cat_num(data[[target_text]])
  } else {
    target_quo = NA
    target_text = NA
    guess_target = "oth"
  }

  # parameter targetpct & split (set default value)
  if (missing(targetpct)) {
    if (missing(split)) {
        split = TRUE
    }
  } else {
    split = !targetpct
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

    # count data
  } else if (!is.na(n_text) & is.na(target_text))  {
    explore_count(data[unique(c(var_text, n_text))], cat = !!var_quo, n = !!n_quo, split = split, pct = TRUE, ...)

    # count data + target
  } else if (!is.na(n_text)  & !is.na(target_text))  {
    explore_count(data[unique(c(var_text, n_text, target_text))], cat = !!var_quo, n = !!n_quo, target = !!target_quo, split = split, ...)

    # var + var2 -> correlation
  } else if (!is.na(var_text) & !is.na(var2_text) & is.na(target_text))  {
    explore_cor(data[unique(c(var_text, var2_text))],
                x = !!var_quo, y = !!var2_quo,
                min_val = min_val, max_val = max_val,
                auto_scale = auto_scale, na = na, ...)

    # var + var2 + target -> correlation
  } else if (!is.na(var_text) & !is.na(var2_text) & !is.na(target_text))  {
    #explore_cor(data[c(var_text, var2_text, target_text)], !!var_quo, !!var2_quo, !!target_quo, ...)
    explore_cor(data[unique(c(var_text, var2_text, target_text))],
                x = !!var_quo, y = !!var2_quo, target = !!target_quo,
                min_val = min_val, max_val = max_val,
                auto_scale = auto_scale, na = na, ...)

    # var num + target num -> correlation
  } else if (!is.na(var_text) & is.na(var2_text) & var_type == "num" & !is.na(target_text) & guess_target == "num")  {
    explore_cor(data[unique(c(var_text, target_text))],
                x = !!var_quo, y = !!target_quo,
                min_val = min_val, max_val = max_val,
                auto_scale = auto_scale, na = na,
                title = target_text, ...)

    # var cat + target num -> correlation
  } else if (!is.na(var_text) & is.na(var2_text) & var_type == "cat" & !is.na(target_text) & guess_target == "num")  {
    explore_cor(data[unique(c(var_text, target_text))],
                y = !!var_quo, x = !!target_quo,
                min_val = min_val, max_val = max_val,
                auto_scale = auto_scale, na = na,
                title = target_text, ...)

    # var_type oth
  } else if (!is.na(var_text) & var_type == "oth")  {
    warning("please use a numeric or character variable to explore")
    plot_var_info(data, !!var_quo, info = "can't explore\n(data type not supported)")

    # no target, num
  } else if (is.na(target_text) & (var_type == "num"))  {
    explore_density(data[var_text],
                    !!var_quo,
                    min_val = min_val, max_val = max_val,
                    auto_scale = auto_scale, na = na, ...)

    # no target, cat
  } else if (is.na(target_text) & (var_type == "cat")) {
    explore_bar(data[var_text], !!var_quo, ...)

    # target, num, split
  } else if (!is.na(target_text) & (var_type == "num") & (split == TRUE)) {
    explore_density(data[unique(c(var_text, target_text))],
                    var = !!var_quo, target = !!target_quo,
                    min_val = min_val, max_val = max_val,
                    auto_scale = auto_scale, na = na, ...)

    # target, num
  } else if (!is.na(target_text) & (var_type == "num")) {
    target_explore_num(data[unique(c(var_text, target_text))],
                       !!var_quo, target = !!target_quo,
                       min_val = min_val, max_val = max_val,
                       auto_scale = auto_scale, na = na, ...)

    # target, cat, split
  } else if (!is.na(target_text) & (var_type == "cat") & (split == TRUE)) {
    explore_bar(data[unique(c(var_text, target_text))],
                       !!var_quo, target = !!target_quo,
                       ...)
    # target, cat
  } else if (!is.na(target_text) & (var_type == "cat")) {
    target_explore_cat(data[unique(c(var_text, target_text))],
                       !!var_quo, target = !!target_quo,
                       min_val = min_val, max_val = max_val, na = na, ...)
  } # if

} # explore


#' Explore variable + binary target (values 0/1)
#'
#' Create a plot to explore relation between a variable and a binary target
#' as target percent. The target variable is choosen automatically
#' if possible (name starts with 'target')
#'
#' @param data A dataset
#' @param var Numerical variable
#' @param target Target variable (0/1 or FALSE/TRUE)
#' @param title Title of the plot
#' @param min_val All values < min_val are converted to min_val
#' @param max_val All values > max_val are converted to max_val
#' @param auto_scale Use 0.2 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param na Value to replace NA
#' @param flip Flip plot? (for categorical variables)
#' @param ... Further arguments
#' @return Plot object
#' @examples
#' iris$target01 <- ifelse(iris$Species == "versicolor",1,0)
#' explore_targetpct(iris)
#' @importFrom magrittr "%>%"
#' @import rlang
#' @export

explore_targetpct <- function(data, var, target = NULL, title = NULL, min_val = NA, max_val = NA, auto_scale = TRUE, na = NA, flip = NA, ...) {

  # check parameter data
  assertthat::assert_that(!missing(data), msg = "expect a data table to explore")
  assertthat::assert_that(is.data.frame(data), msg = "expect a table of type data.frame")
  assertthat::assert_that(nrow(data) > 0, msg = "data has 0 observations")

  # parameter var
  if (!missing(var)) {
    var_quo <- enquo(var)
    var_text <- quo_name(var_quo)[[1]]
    if (!var_text %in% names(data))  {
      stop(paste0("variable '", var_text, "' not found"))
    }
  } else {
    var_quo <- NA
    var_text <- NA
  }

  # intelligent guessing if num or cat
  # based on postfix and type of variable names
  if (!is.na(var_text))  {
    var_type <- guess_cat_num(data[[var_text]])
  } else {
    var_type = "?"
  }

  # guessing of target
  if (missing(target)) {
    var_names <- names(data)
    target_pos <- stringr::str_detect(var_names, "target*")
    target_var <- var_names[target_pos == TRUE]

    if (length(target_var) == 1) {
      target_quo <- sym(target_var[1])
      target_text <- target_var[1]
    } else {
      stop("target not defined, guessing not possible")
    }
  } else {
    target_quo <- enquo(target)
    target_text <- quo_name(target_quo)[[1]]
  }

  if (var_type == "num") {

    # default flip = TRUE
    if (is.na(flip)) {
      flip <- TRUE
    }

    # plot
    p <- target_explore_num(data,
                            var = !!var_quo,
                            target = !!target_quo,
                            title = title,
                            min_val = min_val, max_val = max_val,
                            auto_scale = auto_scale,
                            na = na,
                            flip = flip,
                            ...)
  } else if (var_type == "cat") {

    # default flip = TRUE
    if (is.na(flip)) {
      flip <- TRUE
    }

    # plot
    p <- target_explore_cat(data,
                            var = !!var_quo,
                            target = !!target_quo,
                            title = title,
                            min_val = min_val, max_val = max_val,
                            na = na,
                            flip = flip,
                            ...)
  } else {
    p <- plot_var_info(!!var_quo, info="can't plot\ntyype not supported")
  }

  p

} # explore_targetpct

#' Explore count data (categories + frequency)
#'
#' Create a plot to explore count data (categories + freuency)
#' Variable named 'n' is auto detected as Frequency
#'
#' @param data A dataset (categories + frequency)
#' @param cat Numerical variable
#' @param n Number of observations (frequency)
#' @param target Target variable
#' @param pct Show as percent?
#' @param split Split by target (FALSE/TRUE)
#' @param title Title of the plot
#' @param numeric Display variable as numeric (not category)
#' @param max_cat Maximum number of categories to be plotted
#' @param max_target_cat Maximum number of categories to be plotted for target (except NA)
#' @param flip Flip plot? (for categorical variables)
#' @return Plot object
#' @examples
#' library(dplyr)
#' iris %>%
#'   count(Species) %>%
#'   explore_count(Species)
#' @importFrom magrittr "%>%"
#' @import rlang
#' @export

explore_count <- function(data, cat, n, target, pct = FALSE, split = TRUE, title = NA, numeric = FALSE, max_cat = 30, max_target_cat = 5, flip = NA)  {

  # define variables for CRAN-package check
  plot_cat <- NULL
  plot_n <- NULL
  plot_target <- NULL
  plot_n_sum <- NULL
  plot_n_pct <- NULL
  plot_n_tot <- NULL

  # check parameters
  assertthat::assert_that(!missing(data), msg = "expect a data table to explore")
  assertthat::assert_that(is.data.frame(data), msg = "expect a table of type data.frame")
  assertthat::assert_that(nrow(data) > 0, msg = "data has 0 observations")
  assertthat::assert_that(ncol(data) >= 2, msg = "explect at least 2 variables in data")

  # parameter var
  if(!missing(cat))  {
    cat_quo <- enquo(cat)
    cat_txt <- quo_name(cat_quo)[[1]]
    if (!cat_txt %in% names(data)) {
      stop(paste0("variable '", cat_txt, "' not found"))
    }
  } else {
    cat_txt <- names(data)[1]
  }

  # parameter var
  if(!missing(n))  {
    n_quo <- enquo(n)
    n_txt <- quo_name(n_quo)[[1]]
    if (!n_txt %in% names(data)) {
      stop(paste0("variable '", n_txt, "' not found"))
    }
  } else {
    if("n" %in% names(data)) {
      n_txt <- "n"
    } else {
      stop("variable n not defined")
    }
  }

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
    if (!target_txt %in% names(data)) {
      stop(paste0("variable '", target_txt, "' not found"))
    }
  } else {
    target_txt <- ""
  }

  # number of levels of target
  if (missing(target))  {
    n_target_cat <- 1
  } else {
    n_target_cat <- length(unique(data[[target_txt]]))
  }

  # guess flip
  if (missing(flip)) {
    if(is.numeric(data[[cat_txt]])) {
      flip <- FALSE
    } else {
      flip <- TRUE
    }
  }

  # check NA
  na_cnt <- data %>%
    filter(is.na(.data[[cat_txt]])) %>%
    summarize(na_cnt = sum(.data[[n_txt]])) %>%
    pull(na_cnt)
  n_tot <- data %>% summarise(n_tot = sum(.data[[n_txt]])) %>% pull(n_tot)
  na_pct <- na_cnt / n_tot * 100

  # limit number of levels of var (if not numeric)
  var_cat <- data %>% count(!!cat_quo) %>% pull(!!cat_quo)
  if ( (missing(numeric) | (!missing(numeric) & (numeric == FALSE))) &
       length(var_cat) > max_cat)  {
    data <- data %>% filter(!!cat_quo %in% var_cat[1:max_cat])
    warning(paste("number of bars limited to", max_cat, "by parameter max_cat"))
  }

  # numeric? of use a factor for var if low number of cats
  if (!missing(numeric) & numeric == TRUE)  {
    data[[cat_txt]] <- as.numeric(data[[cat_txt]])
    if (missing(flip)) {
      flip <- FALSE
    }
  } else if ((!missing(numeric) & numeric == FALSE) |
             guess_cat_num(data[[cat_txt]]) == "cat") {
    data[[cat_txt]] <- factor(data[[cat_txt]])
    data[[cat_txt]] <- forcats::fct_explicit_na(data[[cat_txt]], na_level = ".NA")
    if (missing(flip)) {
      flip <- TRUE
    }
  } # if

  # use a factor for target so that fill works
  if (n_target_cat > 1 && !is.factor(data[[target_txt]]))  {
    data[[target_txt]] <- factor(data[[target_txt]])
    data[[target_txt]] <- forcats::fct_explicit_na(data[[target_txt]], na_level = ".NA")

    # keep max. different levels
    if (n_target_cat > max_target_cat)  {
      data[[target_txt]] <- forcats::fct_lump(data[[target_txt]],max_target_cat, other_level = ".OTHER")
    }
    # recalculate number of levels in target
    n_target_cat <- length(levels(data[[target_txt]]))

  }


  ## no target defined
  if(target_txt == "")  {

    # prepare data (no target)
    data_plot <- data[, c(cat_txt, n_txt)]
    names(data_plot) <- c("plot_cat","plot_n")
    data_plot <- suppressMessages(
      data_plot %>%
        group_by(plot_cat) %>%
        summarise(plot_n_sum = sum(plot_n)) %>%
        mutate(plot_n_tot = sum(plot_n_sum)) %>%
        mutate(plot_n_pct = plot_n_sum / plot_n_tot * 100)
    )

    # geom_col (absolute | percent) no target
    if(pct == FALSE)  {
      p <- data_plot %>%
        ggplot(aes(x = plot_cat, y = plot_n_sum)) +
        geom_col(position = "dodge", color = "lightgrey", fill = "lightgrey") +
        theme(
          panel.background = element_rect("white"),
          panel.grid.major = element_line("grey85"),
          panel.grid.minor = element_line("grey85"),
          panel.border = element_rect(fill = NA, color = "lightgrey")) +
        labs(y = "count", x = "")
    } else {
      p <- data_plot %>%
        ggplot(aes(x = plot_cat, y = plot_n_pct)) +
        geom_col(position = "dodge", color = "lightgrey", fill = "lightgrey") +
        theme(
          panel.background = element_rect("white"),
          panel.grid.major = element_line("grey85"),
          panel.grid.minor = element_line("grey85"),
          panel.border = element_rect(fill = NA, color = "lightgrey")) +
      labs(y = "%", x = "")
    }

    ## target defined
  } else {

    # prepare data (target)
    data_plot <- data[, c(target_txt, cat_txt, n_txt)]
    names(data_plot) <- c("plot_target", "plot_cat","plot_n")
    data_plot <- suppressMessages(
      data_plot %>%
        group_by(plot_target, plot_cat) %>%
        summarise(plot_n_sum = sum(plot_n)) %>%
        mutate(plot_n_tot = sum(plot_n_sum)) %>%
        mutate(plot_n_pct = plot_n_sum / plot_n_tot * 100)
    )

    # geom_col (absolute | percent) with target
    if(pct == TRUE | split == TRUE)  {
      # geom_col dodge
      p <- data_plot %>%
        ggplot(aes(x = plot_cat, y = plot_n_pct, fill = plot_target)) +
        geom_col(position = "dodge") +
        theme(
          panel.background = element_rect("white"),
          panel.grid.major = element_line("grey85"),
          panel.grid.minor = element_line("grey85"),
          panel.border = element_rect(fill = NA, color = "lightgrey")) +
        labs(y = "%", x = "", fill = target_txt)
    } else {
      # geom_col stack
      p <- data_plot %>%
        ggplot(aes(x = plot_cat, y = plot_n_sum, fill = plot_target)) +
        geom_col(position = "stack") +
        theme(
          panel.background = element_rect("white"),
          panel.grid.major = element_line("grey85"),
          panel.grid.minor = element_line("grey85"),
          panel.border = element_rect(fill = NA, color = "lightgrey")) +
        labs(y = "count", x = "", fill = target_txt)
    }
  } # if target


  # flip plot
  if (is.na(flip) | flip) {
    p <- p + coord_flip()
  }

  # title
  if (!is.na(title) & nchar(title) > 0)  {
    p <- p + ggtitle(title)
  } else if (missing(target)) {
    p <- p + ggtitle(paste0(cat_txt, ", NA = ", na_cnt, " (",round(na_pct,1), "%)"))
  } else {
    p <- p + ggtitle(cat_txt)
  }

  # return plot
  p
  #data_plot

} # explore_count
