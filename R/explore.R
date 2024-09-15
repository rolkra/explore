## default color "#ADD8E6", #6BAED6 (original)
## default color "#ADD8E6", "#6BAED6" (test)
## default color "#ADD8E6", "#7BB8DA" (actual)

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
#' @param color Color vector (4 colors)
#' @param legend_position Position of legend ("right"|"bottom"|"non")
#' @return Plot object

target_explore_cat <- function(data, var, target = "target_ind", min_val = NA, max_val = NA, flip = TRUE, num2char = TRUE, title = NA, auto_scale = TRUE, na = NA, max_cat = 25, color = c("#ECEFF1", "#CFD8DC", "#B0BEC5", "#90A4AE"), legend_position = "bottom") {

  rlang::check_required(data)
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
  if (!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    target_txt = NA
  }

  if (length(color) < 4) {
    color = c("#ECEFF1", "#CFD8DC", "#B0BEC5", "#90A4AE")
  }

  # rename variables, to use it (lazy evaluation)
  data_bar <- data %>%
    dplyr::select(!!var_quo, !!target_quo)
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
    summarise(n = n(), n_target = sum(target), .groups = "drop") %>%
    mutate(
      n_pct = n / sum(n)*100,
      target_pct = n_target/n*100
      )

  # calculate mean
  target_mean <- sum(data_bar$n_target) / sum(data_bar$n) * 100

  # define categories
  data_bar <- data_bar %>%
    mutate(weight = ifelse (n_pct <= 5, "00-05%",ifelse (n_pct <= 20, "06-20%", ifelse(n_pct <= 40, "21-40%", "41+%")))) %>%
    select(cat, n, n_pct, weight, n_target, target_pct)

  # convert to character
  if(num2char)  {
    data_bar$cat <- as.character(data_bar$cat)
  }

  # define colors
  bar_col <- color
  names(bar_col) <- c("00-05%", "06-20%", "21-40%", "41+%")


  # limit number of categories
  if(nrow(data_bar) > max_cat)  {
    data_bar <- utils::head(data_bar, n = max_cat)
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

#' Explore Nuberical variable + target
#'
#' Create a plot to explore relation between numerical variable and a binary target
#'
#' @param data A dataset
#' @param var Numerical variable
#' @param target Target variable (0/1 or FALSE/TRUE)
#' @param min_val All values < min_val are converted to min_val
#' @param max_val All values > max_val are converted to max_val
#' @param bins Nuber of bins
#' @param flip Should plot be flipped? (change of x and y)
#' @param title Title of plot
#' @param auto_scale Use 0.02 and 0.98 quantile for min_val and max_val (if min_val and max_val are not defined)
#' @param na Value to replace NA
#' @param color Color vector (4 colors)
#' @param legend_position Position of legend ("right"|"bottom"|"non")
#' @return Plot object

target_explore_num <- function(data, var, target = "target_ind", min_val = NA, max_val = NA, bins = 10, flip = TRUE, title = NA, auto_scale = TRUE, na = NA, color = c("#ECEFF1", "#CFD8DC", "#B0BEC5", "#90A4AE"), legend_position = "bottom") {

  # definitions for CRAN package check
  num <- NULL
  cat_label <- NULL
  explore_cat <- NULL

  # parameter var
  if (!missing(var))  {
    var_quo <- enquo(var)
    var_txt <- quo_name(var_quo)[[1]]
  } else {
    var_txt = NA
  }

  # parameter target
  if (!missing(target)) {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    target_txt = NA
  }

  # rename variables, to use it (lazy evaluation)
  data_bar <- data %>%
    dplyr::select(!!var_quo, !!target_quo)
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
    data_bar <- dplyr::mutate(data_bar, explore_cat = cut(num, bins))
  } else {
    data_bar <- data_bar %>% mutate(explore_cat = min_val)
  }

  cat_labels <- data_bar %>%
    dplyr::group_by(explore_cat) %>%
    dplyr::summarize(cat_label = mean(num), n = n())

  data_bar <- data_bar %>%
    dplyr::inner_join(y = cat_labels, by = "explore_cat")

  #result <- data_bar

  result <- target_explore_cat(data_bar,
                               cat_label,
                               target,
                               flip = FALSE,
                               num2char = FALSE,
                               color = color,
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
#' @param color Color for bar
#' @param legend_position Position of the legend ("bottom"|"top"|"none")
#' @param label Show labels? (if empty, automatic)
#' @param label_size Size of labels
#' @param ... Further arguments
#' @return Plot object (bar chart)
#' @export

explore_bar <- function(data, var, target, flip = NA, title = "", numeric = NA, max_cat = 30, max_target_cat = 5, color = c("#ADD8E6", "#7BB8DA"), legend_position = "right", label, label_size = 2.7, ...)  {

  # define variables for CRAN-package check
  na_ind <- NULL
  target_n <- NULL
  pct <- NULL

  # check parameter data
  check_data_frame_non_empty(data)
  rlang::check_required(var)
  # parameter var
  var_quo <- enquo(var)
  var_txt <- quo_name(var_quo)[[1]]
  if (!var_txt %in% names(data)) {
      stop(paste0("variable '", var_txt, "' not found"))
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
    #warning(paste("number of bars limited to", max_cat, "by parameter max_cat"))
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
    data[[var_txt]] <- forcats::fct_na_value_to_level(data[[var_txt]], level = ".NA")
    if (missing(flip)) {
      flip <- TRUE
    }
  } # if

  # use a factor for target so that fill works
  if (n_target_cat > 1) {
    if (!is.factor(data[[target_txt]]))  {
      data[[target_txt]] <- factor(data[[target_txt]])
    }
    data[[target_txt]] <- forcats::fct_na_value_to_level(data[[target_txt]], level = ".NA")

    # keep max. different levels
    if (n_target_cat > max_target_cat)  {
      data[[target_txt]] <- forcats::fct_lump_n(data[[target_txt]], n = max_target_cat, other_level = ".OTHER")
    }
    # recalculate number of levels in target
    n_target_cat <- length(unique(data[[target_txt]]))

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
               fill = color[1],
               color = color[1]) +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey")) +
      labs(x = "", y = "%")

  }

  # color manual
  if (n_target_cat >= 2 & length(color) >= n_target_cat)  {
    ##p <- p + scale_fill_manual(values = c("#CFD8DC","#90A4AE"))
    p <- p + scale_fill_manual(values = as.vector(color))
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
#    p <- p + ggtitle(paste0(var_txt, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)"))
    na_txt <- ifelse(na_cnt == 0,
                     paste0("na = ", na_cnt),
                     paste0("na = ", na_cnt, " (", round(na_pct*100,1), "%)")
    )

    p <- p + labs(title = var_txt, subtitle = paste0(
      na_txt, ", unique = ", length(unique(data[[var_txt]])))) +
      theme(plot.subtitle = element_text(size = 9, color = "#707070"))
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
#' @examples
#' explore_density(iris, "Sepal.Length")
#' iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
#' explore_density(iris, Sepal.Length, target = is_virginica)
#' @export

explore_density <- function(data, var, target, title = "", min_val = NA, max_val = NA, color = c("#ADD8E6", "#7BB8DA"), auto_scale = TRUE, max_target_cat = 5, ...)   {

  # check parameter data
  check_data_frame_non_empty(data)

  # parameter var
  rlang::check_required(var)
  var_quo <- enquo(var)
  var_txt <- quo_name(var_quo)[[1]]
  if (!var_txt %in% names(data)) {
    stop(paste0("variable '", var_txt, "' not found"))
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

  # mean
  show_mean_var <- FALSE
  if (is.na(min_val) & is.na(max_val) & missing(target)) {
    mean_var <- mean(data[[var_txt]], na.rm = TRUE)
    if (!is.na(mean_var)) {
     show_mean_var <- TRUE
    }
  }

  # count NA
  na_check <- data %>%
    mutate(na_ind = ifelse(is.na(!!var_quo),1,0)) %>%
    summarize(na_cnt = sum(na_ind), na_pct = sum(na_ind)/n())
  na_cnt <- na_check[1,1]
  na_pct <- na_check[1,2]

  # raw_min, raw_max
  raw_min <- min(data[[var_txt]], na.rm = TRUE)
  raw_max <- max(data[[var_txt]], na.rm = TRUE)
  if(!is.na(min_val)) { raw_min <- max(min_val, raw_min) }
  if(!is.na(max_val)) { raw_max <- min(max_val, raw_max) }

  # autoscale (if mni_val and max_val not used)
  if (auto_scale && is.na(min_val) && is.na(max_val))  {
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
      geom_density(fill = color[1], alpha = 0.7) +
      #ggtitle(paste0(var_txt, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)")) +
      labs(x = "", y = "") +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey"))

    if (show_mean_var) {
      if ((auto_scale == FALSE) | (mean_var <= max_val)) {
        p <- p + geom_vline(xintercept = mean_var,
                            #color = "#7f7f7f", alpha = 0.5,
                            color = mix_color(color[1], "black", n = 5)[2],
                            alpha = 0.75, linetype = "dashed", lwd = 1)
      }
    }

  } else {

    # factorise target
      if (!is.factor(data[[target_txt]]))  {
        data[[target_txt]] <- factor(data[[target_txt]])
      }
      data[[target_txt]] <- forcats::fct_na_value_to_level(data[[target_txt]], level = ".NA")
      # keep max. different levels
      if (n_target_cat > max_target_cat)  {
        data[[target_txt]] <- forcats::fct_lump(data[[target_txt]],max_target_cat, other_level = ".OTHER")
      }

      # recalculate levels of target cat
      n_target_cat <- length(unique(data[[target_txt]]))

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
    if (n_target_cat >= 2 & length(color) >= n_target_cat)  {
       p <- p + scale_fill_manual(values = as.vector(color), name = target_txt)
    }

  } # if

  # title
  if (!is.na(title) & nchar(title) > 0)  {
    p <- p + ggtitle(title)
  } else if (is.na(target_txt)) {
    #p <- p + ggtitle(paste0(var_txt, ", NA = ", na_cnt, " (",round(na_pct*100,1), "%)"))
    na_txt <- ifelse(na_cnt == 0,
                     paste0("na = ", na_cnt),
                     paste0("na = ", na_cnt, " (", round(na_pct*100,1), "%)")
    )
    p <- p + labs(title = var_txt, subtitle = paste0(
      na_txt, ", min = ",
      format_num_auto(raw_min, digits=6), ", max = ",
      format_num_auto(raw_max, digits=6), ""
      )) +
      theme(plot.subtitle = element_text(size = 9, color = "#707070"))

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
#' @param targetpct Plot variable as target% (FALSE/TRUE)
#' @param color Forece a default color (if possible)
#' @param split Split by target (TRUE|FALSE)
#' @return Plot
#' @examples
#' explore_all(iris)
#'
#' iris$is_virginica <- ifelse(iris$Species == "virginica", 1, 0)
#' explore_all(iris, target = is_virginica)
#' @export

explore_all <- function(data, n, target, ncol = 2, targetpct, color = c("#ADD8E6", "#7BB8DA"), split = TRUE)  {

  # check parameter data
  check_data_frame_non_empty(data)

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

  # parameter targetpct & split (set default value)
  if (missing(targetpct)) {
    if (missing(split)) {
      split = TRUE
    }
  } else {
    split = !targetpct
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
      plots[[i]] <- explore_count(data_tmp, !!sym(var_name), n = !!n_quo, pct = TRUE, color = color[1])

    # count data, target
    } else if (!is.na(n_txt) & (!is.na(var_name_target)))  {
        plots[[i]] <- explore_count(data_tmp, !!sym(var_name), n = !!n_quo, target = !!target_quo, split = split, color = color)

    # no target, num
    } else if ( (var_type == "num") & (is.na(var_name_target))) {
      plots[[i]] <- explore_density(data_tmp, !!sym(var_name), color = color[1])

      # no target, cat
    } else if ( (var_type == "cat") & is.na(var_name_target) ) {
      plots[[i]] <- explore_bar(data_tmp, !!sym(var_name), color = color[1])

      # num target, num -> explore_cor
    } else if ( (var_type == "num") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (guess_target == "num"))  {
      plots[[i]] <- explore_cor(data_tmp, x = !!sym(var_name), y = !!target_quo, title = var_name, color = color)

      # num target, cat -> explore_cor
    } else if ( (var_type == "cat") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (guess_target == "num"))  {
      plots[[i]] <- explore_cor(data_tmp, y = !!sym(var_name), x = !!target_quo, title = var_name, color = color)

      # target, num
    } else if ( (var_type == "num") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (split == FALSE))  {
      plots[[i]] <- target_explore_num(data_tmp, !!sym(var_name), target = !!target_quo, legend_position = "none", color = color)

      # target, num, split
    } else if ( (var_type == "num") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (split == TRUE))  {
      plots[[i]] <- explore_density(data_tmp, !!sym(var_name), target = !!target_quo, color = color)

      # target, cat
    } else if ( (var_type == "cat") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (split == FALSE)) {
      plots[[i]] <- target_explore_cat(data_tmp, !!sym(var_name), target = !!target_quo, legend_position = "none", color = color)

      # target, cat, split
    } else if ( (var_type == "cat") & !is.na(var_name_target) & (var_names[i] != var_name_target) & (split == TRUE)) {
      plots[[i]] <- explore_bar(data_tmp, !!sym(var_name), target = !!target_quo, color = color)

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

explore_cor <- function(data, x, y, target, bins = 8, min_val = NA, max_val = NA, auto_scale = TRUE, title = NA, color = c("#ADD8E6", "#7BB8DA"), ...)  {

  # check parameter data
  check_data_frame_non_empty(data)

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

  # use geom_point?
  use_points <- ifelse(
    x_type == "num" & y_type == "num" &
      nrow(data) <= 500 &
      x_descr$unique / nrow(data) >= 0.1 &
      y_descr$unique / nrow(data) >= 0.1,
    TRUE,
    FALSE
  )

  # correlation for geom_point
  if (use_points) {
    reg_line <- stats::lm(data[[y_txt]] ~ data[[x_txt]])
    reg_intercept <- reg_line$coefficients[1]
    reg_slope <- reg_line$coefficients[2]
    reg_data <- data[, c(x_txt, y_txt)] %>%
      dplyr::filter(!is.na(!!x_quo) & !is.na(!!y_quo))
    reg_corr <- stats::cor(reg_data[[y_txt]], reg_data[[x_txt]])
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

  if(x_type == "num" & y_type == "num" & use_points)  {

    if (!is.na(target_txt)) {

    # use a factor for target so that fill works
    if (!is.factor(data[[target_txt]]))  {
      data[[target_txt]] <- factor(data[[target_txt]])
    }
    data[[target_txt]] <- forcats::fct_na_value_to_level(data[[target_txt]], level = ".NA")

    ## points x,y + color by target
    p <- data %>%
      ggplot(aes(x = !!x_quo, y = !!y_quo, color = !!target_quo)) +
      geom_point(alpha = 0.75, size = 2.5) +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey"))

    if (length(color) >= nrow(unique(data[target_txt])))  {
      ##p <- p + scale_fill_manual(values = c("#CFD8DC","#90A4AE"))
      p <- p + scale_color_manual(values = as.vector(color))
    }


    } else {

      ## points x,y
      color_dark <- mix_color(color[1], "black", n = 5)[2]
      p <- data %>%
        ggplot(aes(x = !!x_quo, y = !!y_quo)) +
        geom_point(alpha = 0.6, size = 2.5, shape = 21,
                   color = color_dark, fill = color[1]) +
        geom_abline(intercept = reg_intercept, slope = reg_slope,
                    #color = "#7f7f7f", alpha = 0.5,
                    color = color_dark,
                    alpha = 0.75, linetype = "solid", size = 1) +
        theme(
          panel.background = element_rect("white"),
          panel.grid.major = element_line("grey85"),
          panel.grid.minor = element_line("grey85"),
          panel.border = element_rect(fill = NA, color = "lightgrey")) +
        labs(subtitle = paste("Correlation =", round(reg_corr,2)))

      if (is.na(title)) {
        p <- p + ggtitle(" ")
      }

    }
  }

  else if(x_type == "num" & y_type == "num" & !use_points)  {

    data[[x_txt]] <- cut_vec_num_avg(data[[x_txt]], bins = bins)

    # boxplot (x = num, y = num)
    p <- data %>%
      ggplot(aes(x = !!x_quo, y = !!y_quo)) +
      geom_boxplot(aes(group = !!x_quo), fill = color[1]) +
      theme(
        panel.background = element_rect("white"),
        panel.grid.major = element_line("grey85"),
        panel.grid.minor = element_line("grey85"),
        panel.border = element_rect(fill = NA, color = "lightgrey"))

  }

  else if(x_type == "cat" & is.numeric(data[[y_txt]]) & (x_descr$unique == nrow(data))) {

     p <- explore_col(data, !!x_quo, !!y_quo, ...)

  }

  else if(x_type == "cat" & y_type == "num") {

    data[[x_txt]] <- as.factor(data[[x_txt]])

    # boxplot (x = cat)
    p <- data %>%
      ggplot(aes(x = !!x_quo, y = !!y_quo)) +
      geom_boxplot(aes(group = !!x_quo), fill = color[1]) +
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
      geom_boxplot(aes(group = !!y_quo), fill = color[1]) +
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
#' @examples
#' explore_tbl(iris)
#' @export

explore_tbl <- function(data, n)  {
  # define variables to pass CRAN-checks
  type <- NULL
  na <- NULL
  measure <- NULL
  group <- NULL

  # check parameter data
  check_data_frame_non_empty(data)

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

  # prepare bars
  bar <- d %>%
    mutate(group = case_when(
      na > 0 ~ "with NA",
      unique == 1 ~ "no variance",
      TRUE ~ "ok"
    )) %>%
    count(type, group)

  bar$group <- factor(
    bar$group,
    levels = c("with NA", "no variance", "ok"),
    ordered = TRUE)

  bar_all = bar %>%
    group_by(group) %>%
    summarise(n = sum(n), .groups = "drop") %>%
    mutate(type = "variables (all)")

  # prepare plot
  bar <- bind_rows(bar, bar_all)
  #type_default <- min(as.character(bar$type), na.rm = TRUE)
  #bar <- bar %>% clean_var(type, na = type_default)

  # define colors
  color_mapping <- c("no variance" = "lightblue",
                     "with NA" = "coral",
                     "ok" = "grey")
  # plot
  bar %>%
    ggplot(aes(n, type, fill = group)) +
    geom_col() +
    scale_fill_manual(values = color_mapping) +
    #geom_text(aes(measure, n, group = type, label = as.character(n)), size = 2.5) +
    geom_text(aes(label = dplyr::na_if(n, 0)),na.rm = TRUE,
              position = "stack"
              ) +
    labs(title = paste(ncol(data), "variables"),
         subtitle = info_obs,
         x = "variables",
         y = "") +
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
#' @param color Color for plots (vector)
#' @examples
#'
#' # Only run examples in interactive R sessions
#' if (interactive())  {
#'    explore_shiny(iris)
#' }
#' @export

explore_shiny <- function(data, target, color = c("#ADD8E6", "#7BB8DA"))  {

  # check if interactive session
  if (!interactive()) {
    warning("This function can only be used in an interactive R session")
    return(invisible())
  }

  # parameter target
  if (!missing(target))  {
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
     dplyr::filter(type %in% c("lgl","int","dbl","num","fct","chr")) %>%
     dplyr::select(variable)
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
        shiny::checkboxInput(inputId = "targetpct", label="% target (0/1)", value=FALSE),
        shiny::hr(),
        shiny::actionButton(inputId = "report", "report all")
        , width = 3),  #sidebarPanel
      shiny::mainPanel(
        shiny::tabsetPanel(
          shiny::tabPanel("variable",
                          shiny::conditionalPanel(condition = "input.target != '<no target>'",
                                                  plotly::plotlyOutput("graph_target")),
                          plotly::plotlyOutput("graph", height = 300),
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

    shiny::observeEvent(input$report, {

      # get name of selected target
      # rmarkdown templates uses variables data and var_name_target
      # templates must be located in package
      var_name_target = input$target
      #path <- getwd()
      output_dir <- normalizePath(path.expand(tempdir()))
      output_file <- "report_explore.html"

      # show waiting-window
      shiny::showModal(shiny::modalDialog("Generating report ... (this may take a while)", footer = NULL))

      # report only variables
      if(input$target == "<no target>")  {
        input_file <- system.file("extdata", "template_report_variable.Rmd", package="explore")
        color_report_plot <- color
        rmarkdown::render(input = input_file, output_file = output_file, output_dir = output_dir)

        # report target with split
      } else if(input$targetpct == FALSE)  {
        input_file <- system.file("extdata", "template_report_target_split.Rmd", package="explore")
        color_report_plot <- color
        rmarkdown::render(input = input_file, output_file = output_file, output_dir = output_dir)

        # report target with percent
      } else {
        input_file <- system.file("extdata", "template_report_target_pct.Rmd", package="explore")
        color_report_plot <- color
        rmarkdown::render(input = input_file, output_file = output_file, output_dir = output_dir)
      }

      # ready
      shiny::removeModal()

      # show Report
      utils::browseURL(paste0("file://", file.path(output_dir, output_file)), browser = NULL)
    })

    output$graph_target <- plotly::renderPlotly({

      if (input$target != "<no target>" & input$var != input$target &
          input$targetpct)  {

        # target with targetpct
        suppressWarnings(interact(
         explore(data, !!sym(input$var), target = !!sym(input$target),
                         auto_scale = input$auto_scale, split = !input$targetpct)
       ))

      } else if (input$target != "<no target>" & input$var != input$target &
                 !input$targetpct)  {

        # target, split
        suppressWarnings(interact(
          explore(data, !!sym(input$var), target = !!sym(input$target),
                  auto_scale = input$auto_scale, split = !input$targetpct,
                  color = color)
        ))

      }
    }) # renderPlot graph_target

    output$graph_explain <- shiny::renderPlot({
      if(input$target != "<no target>") {
        if (ncol(data) > 20) {
          # show waiting-window
          shiny::showModal(shiny::modalDialog("Growing tree ... (this may take a while)", footer = NULL))
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

    output$graph <- plotly::renderPlotly({

      suppressWarnings(interact(
        explore(data, !!sym(input$var), auto_scale = input$auto_scale,
                color = color, max_cat = 20, label = FALSE)
      ))
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
#' @param target Target variable (0/1 or `FALSE`/`TRUE`)
#' @param targetpct Plot variable as target% (`FALSE`/`TRUE`)
#' @param split Alternative to targetpct (split = !targetpct)
#' @param min_val All values < min_val are converted to `min_val`
#' @param max_val All values > max_val are converted to `max_val`
#' @param auto_scale Use 0.2 and 0.98 quantile for `min_val` and `max_val` (if `min_val` and `max_val` are not defined)
#' @param na Value to replace `NA`
#' @param ... Further arguments (like flip = `TRUE`/`FALSE`)
#' @return Plot object
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
  if (missing(data) && missing(var) ) {
    message("Demo: explore the palmer penguins dataset!")
    data <- use_data_penguins()
  } else {
    check_data_frame_non_empty(data)
  }

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
    explore_shiny(data, ...)

    # count data
  } else if (!is.na(n_text) && is.na(target_text))  {
    explore_count(data[unique(c(var_text, n_text))], cat = !!var_quo, n = !!n_quo, split = split, pct = TRUE, ...)

    # count data + target
  } else if (!is.na(n_text)  && !is.na(target_text))  {
    explore_count(data[unique(c(var_text, n_text, target_text))], cat = !!var_quo, n = !!n_quo, target = !!target_quo, split = split, ...)

    # var + var2 -> correlation
  } else if (!is.na(var_text) && !is.na(var2_text) & is.na(target_text))  {
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
  } else if (is.na(target_text) && var_type == "cat") {
    explore_bar(data[var_text], !!var_quo, ...)

    # target, num, split
  } else if (!is.na(target_text) && (var_type == "num") && (split == TRUE)) {
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
  } else if (!is.na(target_text) && (var_type == "cat") && (split == TRUE)) {
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
#' @export

explore_targetpct <- function(data, var, target = NULL, title = NA, min_val = NA, max_val = NA, auto_scale = TRUE, na = NA, flip = NA, ...) {

  # check parameter data
  check_data_frame_non_empty(data)

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
#' @param color Color for bar
#' @param flip Flip plot? (for categorical variables)
#' @return Plot object
#' @examples
#' library(dplyr)
#' iris %>%
#'   count(Species) %>%
#'   explore_count(Species)
#' @export

explore_count <- function(data, cat, n, target, pct = FALSE, split = TRUE, title = NA, numeric = FALSE, max_cat = 30, max_target_cat = 5, color = c("#ADD8E6", "#7BB8DA"), flip = NA)  {

  # define variables for CRAN-package check
  plot_cat <- NULL
  plot_n <- NULL
  plot_target <- NULL
  plot_n_sum <- NULL
  plot_n_pct <- NULL
  plot_n_tot <- NULL

  # check parameters
  check_data_frame_non_empty(data)
  if (ncol(data) < 2) {
    stop("data must contain at least 2 variables.")
  }

  # parameter var
  if (!missing(cat))  {
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
    #warning(paste("number of bars limited to", max_cat, "by parameter max_cat"))
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
    data[[cat_txt]] <- forcats::fct_na_value_to_level(data[[cat_txt]], level = ".NA")
    if (missing(flip)) {
      flip <- TRUE
    }
  } # if

  # use a factor for target so that fill works
  if (n_target_cat > 1 && !is.factor(data[[target_txt]]))  {
    data[[target_txt]] <- factor(data[[target_txt]])
    data[[target_txt]] <- forcats::fct_na_value_to_level(data[[target_txt]],level = ".NA")

    # keep max. different levels
    if (n_target_cat > max_target_cat)  {
      data[[target_txt]] <- forcats::fct_lump(data[[target_txt]],max_target_cat, other_level = ".OTHER")
    }
    # recalculate number of levels in target
    n_target_cat <- length(unique(data[[target_txt]]))

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
        geom_col(position = "dodge", color = color[1], fill = color[1]) +
        theme(
          panel.background = element_rect("white"),
          panel.grid.major = element_line("grey85"),
          panel.grid.minor = element_line("grey85"),
          panel.border = element_rect(fill = NA, color = "lightgrey")) +
        labs(y = "count", x = "")
    } else {
      p <- data_plot %>%
        ggplot(aes(x = plot_cat, y = plot_n_pct)) +
        geom_col(position = "dodge", color = color[1], fill = color[1]) +
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

    # color manual
    if (n_target_cat >= 2 & length(color) >= n_target_cat)  {
      ##p <- p + scale_fill_manual(values = c("#CFD8DC","#90A4AE"))
      p <- p + scale_fill_manual(values = as.vector(color))
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

    na_txt <- ifelse(na_cnt == 0,
                     paste0("na = ", na_cnt),
                     paste0("na = ", na_cnt, " (", round(na_pct,1), "%)")
    )

    p <- p + labs(title = cat_txt, subtitle = paste0(
      na_txt, ", unique = ", length(unique(data[[cat_txt]])))) +
      theme(plot.subtitle = element_text(size = 9, color = "#707070"))

  } else {
    p <- p + ggtitle(cat_txt)
  }

  # return plot
  p
  #data_plot

} # explore_count

#' Explore data without aggregation (label + value)
#'
#' Label and Value are in the data. Create a bar plot where the heights of the
#' bars represent the values for each label.
#'
#' @param data A dataset (categories + frequency)
#' @param var_label Variable containing the label
#' @param var_value Variable containing the value
#' @param title Title of the plot
#' @param subtitle Subtitle of the plot
#' @param numeric Display variable as numeric (not category)
#' @param max_cat Maximum number of categories to be plotted
#' @param na Value to use for NA
#' @param flip Flip plot? (for categorical variables)
#' @param color Color for bar
#' @return Plot object
#' @examples
#' library(magrittr)
#' data <- data.frame(label = LETTERS[1:5], value = c(1.5,2,1.2,3,2.6))
#' data %>% explore_col(label, value)
#'
#' @export

explore_col <- function(data, var_label, var_value,
                        title = NA, subtitle = "",
                        numeric = FALSE,
                        max_cat = 30,
                        na = 0,
                        flip = NA,
                        color = "#ADD8E6") {

  # check parameters
  check_data_frame_non_empty(data)
  if (ncol(data) < 2) {
    stop("data must contain at least 2 variables.")
  }

  # parameter var_label
  if (!missing(var_label))  {
    lab_quo <- enquo(var_label)
    lab_txt <- quo_name(lab_quo)[[1]]
    if (!lab_txt %in% names(data)) {
      stop(paste0("variable '", lab_txt, "' not found"))
    }
  } else {
    lab_txt <- names(data)[1]
  }

  # parameter var_value
  if (!missing(var_value))  {
    val_quo <- enquo(var_value)
    val_txt <- quo_name(val_quo)[[1]]
    if (!val_txt %in% names(data)) {
      stop(paste0("variable '", val_txt, "' not found"))
    }
  } else {
    val_txt <- names(data)[2]
  }

  if (!is.numeric(data[[val_txt]])) {
    stop("var_value must be a numeric variable")
  }

  # replance NA values (is wanted)
  if (!is.na(na)) {
    data[val_txt] <- ifelse(is.na(data[[val_txt]]), na, data[[val_txt]])
  }

  data_plot <- data[ , c(lab_txt, val_txt)]
  names(data_plot) <- c("label", "value")

  explore_count(data_plot, label, n = value,
                title = title, numeric = numeric, max_cat = max_cat,
                flip = flip, color = color) +
    ggplot2::labs(subtitle = subtitle) +
    # ggplot2::xlab(lab_txt) +
    ggplot2::ylab(val_txt)

} # explore_col


#' Make a explore-plot interactive
#'
#' @param obj A object (e.g. ggplot2-object)
#' @param lower_title Lowering the title in ggplot2-object(`FALSE`/`TRUE`)
#' @param hide_geom_text Hiding geom_text in ggplot2-object (`FALSE`/`TRUE`)
#' @return Plot object
#' @examples
#' library(dplyr)
#' if (interactive())  {
#'    iris %>% explore(Sepal.Length, target = Species) %>% interact()
#' }
#' @export

interact <- function(obj, lower_title = TRUE, hide_geom_text = TRUE) {

  if("ggplot" %in% class(obj)) {

    # hide geom_text (ugly tool tip) Layer 2
    if (hide_geom_text & length(obj$layers) >= 2) {
      if ("GeomText" %in% class(obj$layers[[2]]$geom)) {
        obj$layers[[2]] <- NULL
      }
    }

    # hide geom_text (ugly tool tip) Layer 3
    if (hide_geom_text & length(obj$layers) >= 3) {
      if ("GeomText" %in% class(obj$layers[[3]]$geom)) {
        obj$layers[[3]] <- NULL
      }
    }

    # lower_title (to get space for plotly-top-menu)
    if (lower_title) {
      obj <- obj + ggplot2::theme(plot.margin = ggplot2::unit(c(1.1,0.1,0.1,0.1), 'cm'))
    }

    # fix subtitle (if exist)
    if (!is.null(obj$labels$subtitle) & !is.null(obj$labels$title)) {
      obj$labels$title <- paste0(obj$labels$title,"<br><sup>", obj$labels$subtitle)
      obj$labels$subtitle <- NULL
    }

    suppressWarnings(plotly::ggplotly(obj))

  } else {

    # if obj is not an ggplot-object, simply return it
    obj

  }
} #interact
