

#' Title People-Like-Me methods for single testing individual
#'
#' @param train_data
#' @param test_data
#' @param outcome_var
#' @param time_var
#' @param id_var
#' @param brokenstick_knots
#' @param anchor_time
#' @param linear_formula
#' @param gamlss_formula
#' @param gamlss_sigma
#' @param match_methods
#' @param weight
#' @param match_alpha
#' @param match_number
#' @param match_plot
#' @param predict_plot
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
people_like_me <- function(train_data,
                           test_data,
                           outcome_var = "ht",
                           time_var = "time",
                           id_var = "id",
                           tmin = 0,
                           tmax = 17,
                           brokenstick_knots,
                           anchor_time,
                           linear_formula = "ht ~ as.factor(time) * sex + ethnic + genotype + baseline",
                           gamlss_formula = "ht ~ cs(time, df = 3)",
                           gamlss_sigma = "~ cs(time, df = 1)",
                           match_methods = "mahalanobis",
                           weight = FALSE,
                           match_alpha = NULL,
                           match_number = NULL,
                           match_plot = TRUE,
                           predict_plot = TRUE,
                           ...) {

  ## user defined variables
  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)

  # browser()
  ## extract the test baseline information
  test_baseline <- test_data %>%
    group_by(!!id_var) %>%
    arrange(!!time_var) %>%
    slice(1L) %>%
    ## change the baseline outcome_vars as new variable
    # dplyr::select(baseline = !!outcome_var, everything()) %>%
    ## move the original time_var as all ZEROs
    dplyr::select(- !!time_var)

  # Tue Jul 25 22:09:42 2023 ------------------------------
  ## will add other methods probably will add ifelse
  ## currently just the brokenstick model
  brokenstick <- impute_brokenstick(outcome_var = !!outcome_var,
                                    time_var = !!time_var,
                                    id_var = !!id_var,
                                    bs_knots = brokenstick_knots,
                                    anchor_time = anchor_time,
                                    data = train_data)

  ## linear regression is the necessary one will be kept
  lm_bks <- lm(as.formula(linear_formula),
               data = brokenstick)
  # Tue Jul 25 22:30:34 2023 ------------------------------
  test_baseline[paste0("anchor_", anchor_time)] <- NA

  data_test1 <- test_baseline %>%
    # dplyr::select(-!!time_var) %>%
    group_by(!!id_var) %>%
    pivot_longer(cols = contains("anchor_"),
                 names_to = "time0",
                 names_prefix = "anchor_",
                 values_to = "lm_bks_target") %>%
    rename(baseline = !!outcome_var,
           !!time_var := time0)

  lp_test <- data_test1 %>%
    ungroup() %>%
    mutate(lm_bks_target = predict(lm_bks, newdata = data_test1)) %>%
    dplyr::select(!!id_var, !!time_var, contains("lm_bks_target")) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rename(!!outcome_var := lm_bks_target)

  lp_train <- brokenstick %>%
    ungroup() %>%
    mutate(lm_bks_target = predict(lm_bks)) %>%
    dplyr::select(!!id_var, !!time_var, contains("lm_bks_target")) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rename(!!outcome_var := lm_bks_target)

  ## end of 01_impute.R file ------------------------

  ## this is the distance for just one individual
  distance <- distance_df(lb_train = lp_train,
                          lb_test_ind = lp_test,
                          match_methods = match_methods,
                          id_var = !!id_var,
                          outcome_var = !!outcome_var,
                          time_var = !!time_var)

  subset <- match(distance_df = distance,
                  train = train_data,
                  test_one = test_data,
                  id_var = !!id_var,
                  outcome_var = !!outcome_var,
                  time_var = !!time_var,
                  match_alpha = match_alpha,
                  match_number = match_number,
                  match_plot = match_plot)

  ## the dataset is ready ---------------------------
  gamlss1 <- predict_gamlss(matching = subset$subset,
                            test_one = test_data,
                            id_var = !!id_var,
                            time_var = !!time_var,
                            outcome_var = !!outcome_var,
                            weight = weight,
                            tmin = tmin,
                            tmax = tmax,
                            gamlss_formula = gamlss_formula,
                            gamsigma_formula = gamlss_sigma,
                            predict_plot = predict_plot)

  results <- list(plot = gamlss1$predictive_centiles,
                  matches = subset$plot,
                  predicted = gamlss1$centiles_predicted,
                  observed = gamlss1$centiles_observed,
                  gamlss_data = subset$subset)

  attr(results, "distance") <- distance
  attr(results, "brokenstick_model") <- brokenstick$model_bks
  attr(results, "brokenstick_impute") <- brokenstick$data_anchor
  attr(results, "linear_model") <- lm_bks

  return(results)
}




#' Title People-Like-Me methods for multiple testing dataset individuals
#'

people_like_us <- function(train_data,
                           test_data,
                           outcome_var = "ht",
                           time_var = "time",
                           id_var = "id",
                           tmin = 0,
                           tmax = 17,
                           brokenstick_knots,
                           anchor_time,
                           linear_formula = "ht ~ as.factor(time) * sex + ethnic + genotype + baseline",
                           gamlss_formula = "ht ~ cs(time, df = 3)",
                           gamlss_sigma = "~ cs(time, df = 1)",
                           match_methods = c("euclidean", "mahalanobis", "single"),
                           weight = FALSE,
                           match_alpha = NULL,
                           match_number = NULL,
                           match_plot = TRUE,
                           predict_plot = TRUE,
                           ...) {

  ## user defined variables
  outcome_var <- ensym(outcome_var)
  time_var <- ensym(time_var)
  id_var <- ensym(id_var)

  # id_train <- dplyr::select(train_data, !!id_var) %>% unique() %>% unlist()
  id_test <- dplyr::select(test_data, !!id_var) %>% unique() %>% unlist()

  ## extract the test baseline information
  test_baseline <- test_data %>%
    group_by(!!id_var) %>%
    arrange(!!time_var) %>%
    slice(1L) %>%
    ## change the baseline outcome_vars as new variable
    # dplyr::select(baseline = !!outcome_var, everything()) %>%
    ## move the original time_var as all ZEROs
    dplyr::select(- !!time_var)

  # Tue Jul 25 22:09:42 2023 ------------------------------
  ## will add other methods probably will add ifelse
  ## currently just the brokenstick model
  brokenstick <- impute_brokenstick(outcome_var = !!outcome_var,
                                    time_var = !!time_var,
                                    id_var = !!id_var,
                                    bs_knots = brokenstick_knots,
                                    anchor_time = anchor_time,
                                    data = train_data)

  ## linear regression is the necessary one will be kept
  lm_bks <- lm(as.formula(linear_formula),
               data = brokenstick)
  # Tue Jul 25 22:30:34 2023 ------------------------------
  # test_baseline[paste0("anchor_", anchor_time)] <- NA

  data_test1 <- test_baseline %>%
    # dplyr::select(-!!time_var) %>%
    ## this is the unnest way of doing testing dataset
    mutate(time = list(anchor_time)) %>%
    unnest(cols = !!time_var) %>%
    ## original way of augment the testing dataset
    # group_by(!!id_var) %>%
    # pivot_longer(cols = contains("anchor_"),
    #              names_to = "time0",
    #              names_prefix = "anchor_",
    #              values_to = "lm_bks_target") %>%
    rename(baseline = !!outcome_var)

  lp_test <- data_test1 %>%
    ungroup() %>%
    mutate(lm_bks_target = predict(lm_bks, newdata = .)) %>%
    dplyr::select(!!id_var, !!time_var, contains("lm_bks_target")) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rename(!!outcome_var := lm_bks_target)

  lp_train <- brokenstick %>%
    ungroup() %>%
    mutate(lm_bks_target = predict(lm_bks)) %>%
    dplyr::select(!!id_var, !!time_var, contains("lm_bks_target")) %>%
    as.matrix() %>%
    as.data.frame() %>%
    rename(!!outcome_var := lm_bks_target)

  # browser()
  ## end of 01_impute.R file ------------------------
  subset <- lp_test %>%
    group_by(!!id_var) %>%
    group_map(~dis_match(lb_train = lp_train,
                         lb_test_ind = .,
                         train = train_data,
                         match_methods = match_methods,
                         id_var = !!id_var,
                         outcome_var = !!outcome_var,
                         time_var = !!time_var,
                         match_alpha = match_alpha,
                         match_number = match_number,
                         match_time = match_time,
                         match_plot = match_plot))


  ## the dataset is ready ---------------------------
  results <- test_data %>%
    group_by(!!id_var) %>%
    group_map(~as.data.frame(.)) %>%
    map2(subset, ~predict_gamlss(matching = .y$subset,
                                 test_one = .x,
                                 id_var = !!id_var,
                                 time_var = !!time_var,
                                 outcome_var = !!outcome_var,
                                 tmin = tmin,
                                 tmax = tmax,
                                 weight = weight,
                                 gamlss_formula = gamlss_formula,
                                 gamsigma_formula = gamlss_sigma,
                                 predict_plot = predict_plot))

  attr(results, "subset") <- subset$plot
  # attr(results, "matching_plot") <- subset$matching_plot
  # attr(results, "brokenstick_model") <- brokenstick$model_bks
  # attr(results, "brokenstick_impute") <- brokenstick$data_anchor
  # attr(results, "baseline") <- brokenstick$data_baseline
  # attr(results, "linear_model") <- summary(lm_bks)
  #
  return(results)
}
