#' Create an empty dataset
#'
#' @param obs Number of observations
#' @param add_id Add an id
#' @return Dataset
#' @examples
#' create_data_empty()
#' @export

create_data_empty <- function(obs = 1000, add_id = FALSE) {

  data <- data.frame(
    row.names = seq(1, obs)
  )

  if (add_id)  {
    data[["id"]] <- seq(1, obs)
  }

  data

} # create_data_empty


#' Create data person
#'
#' Artificial data that can be used for unit-testing or teaching
#'
#' @param obs Number of observations
#' @param seed Seed for randomization (integer)
#'
#' @return A dataframe
#' @export

create_data_person <- function(obs = 1000, seed = 123) {

  # reproducible random numbers
  set.seed(seed)

  # number of observations
  nobs <- obs

  # random data
  data <- tibble::tibble(
    age = sample(16:95, nobs, replace = TRUE),
    gender = sample(c("Male","Female", "X"), prob = c(0.49, 0.49, 0.02), nobs, replace = TRUE),
    eye_color = sample(c("blue","green","brown"), nobs, replace = TRUE),
    shoe_size = trunc(stats::rnorm(nobs, mean = 43, sd = 3)),
    iq = trunc(stats::rnorm(nobs, mean = 100, sd = 20)),
    education = sample(c(0:100), nobs, replace = TRUE),
    income = sample(c(0:100), nobs, replace = TRUE),

    handset = sample(c("Apple","Android", "Other"), prob = c(0.4,0.5, 0.1), nobs, replace = TRUE),

    pet = sample(c("dog","cat","other","no"),
                 prob = c(0.23,0.22,0.11,0.35),
                 nobs, replace = TRUE),

    favorite_pizza = sample(c("Margaritha", "Carciofi","Pepperoni", "Hawai", "Quattro Statgioni", "Provenciale"), nobs, replace = TRUE),
    favorite_icecream = sample(c("Vanilla", "Chocolate","Strawberry", "Lemon", "Cookie", "Hazelnut","Apple"),
                               prob=c(0.3,0.2,0.2,0.1,0.1,0.05,0.05),
                               nobs, replace = TRUE),
    likes_garlic = as.integer(sample(0:1, nobs, prob = c(0.4, 0.6), replace = TRUE)),
    likes_sushi = as.integer(sample(0:1, nobs, prob = c(0.5, 0.5), replace = TRUE)),
    likes_beatles = as.integer(sample(0:1, nobs, prob = c(0.6, 0.4), replace = TRUE)),
    likes_beer = as.integer(sample(0:1, nobs, prob = c(0.6, 0.4), replace = TRUE))
  )

  # not random correlations
  random01 <- stats::runif(nobs)
  data$likes_beatles <- ifelse(data$likes_beatles + data$age/50 * random01>= 1, 1L, 0L)
  data$likes_beer <- ifelse(data$gender == "Male" & random01 >= 0.2, 1L, data$likes_beer)
  data$likes_sushi <- ifelse(data$age/75 + random01/4 >= 1, 0L, data$likes_sushi)
  data$shoe_size <- ifelse(data$gender == "Female"  & random01 >= 0.3, data$shoe_size - 1.8, data$shoe_size)
  data$income <- ifelse(data$education > 50 & random01 > 0.3, data$income + data$education/2, data$income)
  data$handset <- ifelse(data$handset == "Android" & data$income>75 & data$education<25, "Apple", data$handset)
  # add extreme values
  #data[1,"age"] <- 5000

  # return data
  data

} #create_data_person


#' Create data buy
#'
#' Artificial data that can be used for unit-testing or teaching
#'
#' Variables in dataset:
#' * id = Identifier
#' * period = Year & Month (YYYYMM)
#' * city_ind = Indicating if customer is residing in a city (1 = yes, 0 = no)
#' * female_ind = Gender of customer is female (1 = yes, 0 = no)
#' * fixedvoice_ind = Customer has a fixed voice product (1 = yes, 0 = no)
#' * fixeddata_ind = Customer has a fixed data product (1 = yes, 0 = no)
#' * fixedtv_ind = Customer has a fixed tv product (1 = yes, 0 = no)
#' * mobilevoice_ind = Customer has a mobile voice product (1 = yes, 0 = no)
#' * mobiledata_ind = Customer has a mobile data product (1 = yes, 0 = no)
#' * bbi_speed_ind = Customer has a Broadband Internet (BBI) with extra speed
#' * bbi_usg_gb = Broadband Internet (BBI) usage in Gigabyte (GB) last month
#' * hh_single = Expected to be a Single Household (1 = yes, 0 = no)
#'
#' Target in dataset:
#' * buy (may be renamed) = Did customer buy a new product in next month?
#' (1 = yes, 0 = no)
#' @param obs Number of observations
#' @param target_name Variable name of target
#' @param factorise_target Should target variable be factorised?
#' (from 0/1 to facotr no/yes)?
#' @param target1_prob Probability that buy = 1
#' @param add_extreme Add an obervation with extreme values?
#' @param flip_gender Should Male/Female be flipped in data?
#' @param add_id Add an id-variable to data?
#' @param seed Seed for randomization
#'
#' @return A dataframe
#' @export

create_data_buy = function(obs = 1000,
                           target_name = "buy",
                           factorise_target = FALSE,
                           target1_prob = 0.5,
                           add_extreme = TRUE,
                           flip_gender = FALSE,
                           add_id = FALSE,
                           seed = 123) {

  # define variables for CRAN-package check
  target_ind <- NULL
  age <- NULL
  city_ind <- NULL

  # set seed (randomization)
  set.seed(seed)

  # create basic dataset
  data <- data.frame(
    id = as.integer(seq(from = 1, to = obs)),
    period = as.integer(rep(202012, obs)),
    target_ind = sample(
      c(0L, 1L),
      obs,
      prob = c(1 - target1_prob, target1_prob),
      replace = TRUE)
  )

  # add features
  data <- data %>%
    dplyr::mutate(
      age = as.integer(round(ifelse(target_ind == 1,
                         stats::rnorm(obs, mean = 45, sd = 10),
                         stats::rnorm(obs, mean = 60, sd = 10)
      ), 0)),
      city_ind = ifelse(target_ind == 1,
                        sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.4, 0.6)),
                        sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.6, 0.4))
      ),
      female_ind = ifelse(target_ind == 1,
                          sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.3, 0.7)),
                          sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.7, 0.3))
      ),
      fixedvoice_ind = ifelse(age > 70,
                              sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.3, 0.7)),
                              sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.95, 0.05))
      ),
      fixeddata_ind = 1L,
      fixedtv_ind = ifelse(target_ind == 1,
                           sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.4, 0.6)),
                           sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.8, 0.2))
      ),
      mobilevoice_ind = sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.4, 0.6)),
      mobiledata_ind = sample(c("NO","MOBILE STICK", "BUSINESS"), obs, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
      bbi_speed_ind = ifelse(age > 60,
                             sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.9, 0.1)),
                             sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.2, 0.8))
      ),
      bbi_usg_gb = as.integer(ifelse(age > 75,
                          round(stats::rnorm(obs, mean = 10, sd = 1)),
                          round(stats::rnorm(obs, mean = 50, sd = 10))
      ) + city_ind * 20 + target_ind * 10),
      hh_single = ifelse(age < 35 & city_ind == 1,
                         sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.2, 0.8)),
                         sample(c(0L, 1L), obs, replace = TRUE, prob = c(0.7, 0.3))
      )
    ) # mutate

  # add extreme values?
  if (add_extreme) {
    extreme <- data[nrow(data), ]
    extreme$bbi_usg_gb <- 100000L
    extreme$target_ind <- 0L
    data[nrow(data), ] <- extreme[1, ]
  }

  # factorise target?
  if (factorise_target) {
    data$target_ind <- factor(data$target_ind,
                              levels = c(0, 1),
                              labels = c("no", "yes"))
  }

  # rename target?
  if (target_name != "target_ind") {
    names(data)[[grep("target_ind", names(data))]] <- target_name
  }

  # flip gender?
  if (flip_gender) {
    data[["female_ind"]] <- ifelse(data[["female_ind"]] == 1, 0, 1)
  }

  # add id?
  if (!add_id)  {
    data$id <- NULL
  }

  # return data
  data

} # create_data_buy

#' Create data random
#'
#' Random data that can be used for unit-testing or teaching
#'
#' Variables in dataset:
#' * id = Identifier
#' * var_X = variable containing values between 0 and 100
#'
#' Target in dataset:
#' * target_ind (may be renamed) = random values (1 = yes, 0 = no)
#'
#' @param obs Number of observations
#' @param vars Number of variables
#' @param target_name Variable name of target
#' @param factorise_target Should target variable be factorised?
#' (from 0/1 to facotr no/yes)?
#' @param target1_prob Probability that buy = 1
#' @param add_id Add an id-variable to data?
#' @param seed Seed for randomization
#'
#' @return A dataframe
#' @export

create_data_random = function(obs = 1000, vars = 10,
                              target_name = "target_ind",
                              factorise_target = FALSE,
                              target1_prob = 0.5,
                              add_id = TRUE,
                              seed = 123) {

  # set seed (randomization)
  set.seed(seed)

  # create basic dataset
  data <- data.frame(
    id = seq(from = 1, to = obs),
    target_ind = sample(c(0L, 1L),
                        obs,
                        prob = c(1 - target1_prob, target1_prob),
                        replace = TRUE)
  )

  # add features
  for (i in seq_len(vars)) {
    data[[paste0("var_",i)]] <- as.integer(
      round(stats::runif(obs)*100,0))
  }


  # factorise target?
  if (factorise_target) {
    data$buy <- factor(data$buy,
                       levels = c(0, 1),
                       labels = c("no", "yes"))
  }

  # rename target?
  if (target_name != "target_ind") {
    data[[target_name]] <- data$target_ind
    data$target_ind <- NULL
  }

  # add id?
  if (!add_id)  {
    data$id <- NULL
  }

  # return data
  data

} # create_data_random
