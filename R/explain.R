#============================================================================
#  explain_tree
#============================================================================

#' Explain a target using a simple decision tree (classification or regression)
#'
#' @param data A dataset
#' @param target Target variable
#' @param n weigths (for count data)
#' @param max_cat Drop categorical variables with higher number of levels
#' @param max_target_cat Maximum number of categories to be plotted for target (except NA)
#' @param maxdepth Maximal depth of the tree (rpart-parameter)
#' @param minsplit The minimum number of observations that must exist in a node to split.
#' @param cp Complexity parameter (rpart-parameter)
#' @param size Textsize of plot
#' @param ... Further arguments
#' @return Plot
#' @importFrom tidyr uncount
#' @examples
#' data <- iris
#' data$is_versicolor <- ifelse(iris$Species == "versicolor", 1, 0)
#' data$Species <- NULL
#' explain_tree(data, target = is_versicolor)
#' @export

explain_tree <- function(data, target, n, max_cat = 10, max_target_cat = 5, maxdepth = 3, minsplit = nrow(data)/10, cp = 0, size = 0.7, ...)  {

  # define variables to pass CRAN-checks
  type <- NULL
  variable <- NULL

  # parameter data
  if(missing(data))  {
    stop(paste0("data missing"))
  }

  # parameter n
  if(!missing(n))  {
    n_quo <- enquo(n)
    n_txt <- quo_name(n_quo)[[1]]
    # convert all character to factor
    data[sapply(data, is.character)] <- lapply(
      data[sapply(data, is.character)], as.factor)
    # uncount data
    data <- data %>% tidyr::uncount(weights = !!n_quo)
  } else {
    n_txt = NA
  }

  # parameter target
  if(!missing(target))  {
    target_quo <- enquo(target)
    target_txt <- quo_name(target_quo)[[1]]
  } else {
    target_txt = NA
    return(NULL)
  }

  # drop variables, that are not usable
  d <- describe(data)
  var_keep <- d %>%
    filter(type %in% c("lgl", "int", "dbl", "chr", "fct")) %>%
    filter(type != "chr" | (type == "chr" & unique <= max_cat) | variable == target_txt) %>%
    pull(variable)
  data <- data %>% select(one_of(as.character(var_keep)))

  # minimum 2 variables left?
  if (ncol(data) < 2) {
    p <- plot_text("can't grow decision tree")
    return(invisible(p))
  }

  # observations?
  if(nrow(data) == 0) {
    p <- plot_text("can't grow decision tree")
    return(invisible(p))
  }

  # target all NA?
  if (all(is.na(data[[target_txt]])))  {
    p <- plot_text("can't grow decision tree")
    return(invisible(p))
  }

  # convert target into formula
  formula_txt <- as.formula(paste(target_txt, "~ ."))

  if(guess_cat_num(data[[target_txt]]) == "cat")  {

    # convert target to factor if necessary
    if (!is.factor(data[[target_txt]]))  {
      data[[target_txt]] <- factor(data[[target_txt]])
      data[[target_txt]] <- forcats::fct_lump(data[[target_txt]], max_target_cat, other_level = ".OTHER")
    }

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
#' Explain a binary target using a logistic regression (glm).
#' Model chosen by AIC in a Stepwise Algorithm (MASS::stepAIC).
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

  # parameter data
  if(missing(data))  {
    stop(paste0("data missing"))
  }

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
