## 2.1 lmm dynamic prediction {{{-----------------------------------------------
#' Title
#'
#' @param fitting
#' @param baseline
#'
#' @return
#' @export
#'
#' @examples
lmm_pred <- function(train_data,
                     test_data,
                     baseline) {

  # train_data <- train
  # test_data <- train_data
  ctrl <- lmeControl(opt = 'optim')
  fitting <-  lme(ht ~ bs(time, knots = c(10, 12, 15), degree = 3) * sex + genotype - 1,
                  random = ~ 1 + bs(time, df = 5, degree = 2, intercept = FALSE)| id,
                  control = ctrl,
                  data = train_data)
  time_vec <- unique(test_data$time)

  lmmpred_95 <- IndvPred_lme(
    lmeObject = fitting,
    newdata = baseline,
    timeVar = "time",
    M = 500,
    times = time_vec,
    all_times = TRUE,
    return_data = TRUE,
    level = 0.95,
    interval = "prediction",
    seed = 555)


  lmmpred_90 <- IndvPred_lme(
    lmeObject = fitting,
    newdata = baseline,
    timeVar = "time",
    M = 500,
    times = time_vec,
    all_times = TRUE,
    return_data = TRUE,
    level = 0.90,
    interval = "prediction",
    seed = 555)

  lmmpred_50 <- IndvPred_lme(
    lmeObject = fitting,
    newdata = baseline,
    timeVar = "time",
    M = 500,
    times = time_vec,
    all_times = TRUE,
    return_data = TRUE,
    level = 0.5,
    interval = "prediction",
    seed = 555)

  lmm_results <-
    dplyr::select(lmmpred_50,
                  id, time,
                  # observed = ht,
                  pred,
                  centile25 = low,
                  centile75 = upp) %>%
    full_join(dplyr::select(lmmpred_90,
                            id, time,
                            # observed = ht,
                            pred,
                            centile05 = low,
                            centile95 = upp),
              by = c("id", "time", "pred")) %>%
    full_join(dplyr::select(lmmpred_95,
                            id, time,
                            # observed = ht,
                            pred,
                            centile025 = low,
                            centile975 = upp),
              by = c("id", "time", "pred")) %>%
    mutate(time = round(time, 2)) %>%
    na.omit() %>%
    as.data.frame() %>%
    right_join(train_data) %>%
    mutate(coverage50 = ifelse(ht >= `centile25` & ht <= `centile75`, 1, 0),
           coverage80 = ifelse(ht >= `centile05` & ht <= `centile95`, 1, 0),
           coverage95 = ifelse(ht >= `centile025` & ht <= `centile975`, 1, 0),
           # mse = (actual - `50`)^2,
           # biassq = bias^2,
           # var = mse - bias^2,
           bias = abs(ht - pred))

  return(lmm_results)
}
## }}}--------------------------------------------------------------------------

