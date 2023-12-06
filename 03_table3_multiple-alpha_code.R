## packages upload -------------------------------------------------------------
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(splines)
library(tibble)
library(nlme)
library(gamlss)
library(JMbayes)

knots = c(10, 12, 15)
# pred_time = c(3, 6, 9, 12)
# pred_time = c(4, 8, 12)
# pred_time = c(5, 10, 15)
pred_time = c(6, 10, 11, 12)
# pred_time = c(6, 9, 12)
# pred_time = c(8, 10, 12)
alpha = c("multiple")

source("R/00_functions.R")
load("R/train_test.rda")
print("loading works-----------------------------")

set.seed(555)

## people like me method -----------------------------------------------------
test_data <- test
train_data <- train
test_baseline <-
  test_data %>%
  group_by(id) %>%
  slice(1L)

id_train <- unique(train_data$id)
id_test <- unique(test_data$id)
print("1. model works---------------------")

bks <- brokenstick::brokenstick(ht ~ time | id,
                                data = train_data,
                                knots = knots)

dataset_baseline <- train_data %>%
  group_by(id) %>%
  slice(1L) %>%
  dplyr::select(-time) %>%
  dplyr::select(baseline = ht, everything())
print("2.baseline works")

## checkpoint ------------------------------------------------------------------
bks_pred_knots <- predict(bks,
                          x = pred_time,
                          shape = "long",
                          # group = train_data$id,
                          include_data = FALSE) %>%
  dplyr::select(id, time, `.pred`)
print("this is bks_pred_knots for training --------------------")

train_pred <- bks_pred_knots %>%
  select_if(not_all_na)

train_new <- full_join(train_pred, dataset_baseline)
## add one factor time_var variable

test_new <- test_baseline %>%
  mutate(time = list(pred_time)) %>%
  unnest(cols = c(time)) %>%
  rename(baseline = ht)

## 1.2 linear fitting
lm_bks_train <- lm("`.pred` ~ as.factor(time) * sex + genotype + baseline",
                   data = train_new)
predicted_train <- predict(lm_bks_train)
predicted_test<- predict(lm_bks_train, newdata = test_new)

# Mon Nov 27 11:07:00 2023 ------------------------------
# s <- coef(lm_bks_train) %>% as.matrix()
# mm <- model.matrix(lm_bks_train, data = train_new)
# g(mm, s, nderiv = 1)
View(bks)
summary(bks)


lb_train <- train_new %>%
  ungroup() %>%
  mutate(lm_bks_target = predicted_train) %>%
  dplyr::select(lm_bks_target) %>%
  cbind(train_pred) %>%
  as.data.frame() %>%
  dplyr::select(contains(c("id", "time", "lm_bks_target"))) %>%
  as.matrix() %>%
  as.data.frame() %>%
  mutate(lm_bks_target = as.numeric(lm_bks_target))


test_new[, "lm_bks_target"] = as.numeric(predicted_test)
lb_test <- test_new %>%
  dplyr::select(contains(c("id", "time", "lm_bks")))


## test_mhl_p060-------------------
test_mhl_p060 <- map(id_test,
                    ~try(pred_matching(
                      lb_data = lb_train,
                      lb_test = lb_test,
                      test_data = test_data,
                      train_data = train_data,
                      match_methods = "mahalanobis",
                      match_alpha = 0.60,
                      gamlss_formula = "ht ~ cs(time, df = 3)",
                      gamsigma_formula = "~ cs(time, df = 1)",
                      match_plot = FALSE,
                      predict_plot = FALSE,
                      sbj = .)),
                    .progress = list(type = "iterator",
                                     format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                     clear = TRUE))


## test_mhl_p070-------------------
test_mhl_p065 <- map(id_test,
                    ~try(pred_matching(
                      lb_data = lb_train,
                      lb_test = lb_test,
                      test_data = test_data,
                      train_data = train_data,
                      match_methods = "mahalanobis",
                      match_alpha = 0.65,
                      gamlss_formula = "ht ~ cs(time, df = 3)",
                      gamsigma_formula = "~ cs(time, df = 1)",
                      match_plot = FALSE,
                      predict_plot = FALSE,
                      sbj = .)),
                    .progress = list(type = "iterator",
                                     format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                     clear = TRUE))

## test_mhl_p075-------------------
test_mhl_p070 <- map(id_test,
                    ~try(pred_matching(
                      lb_data = lb_train,
                      lb_test = lb_test,
                      test_data = test_data,
                      train_data = train_data,
                      match_methods = "mahalanobis",
                      match_alpha = 0.70,
                      gamlss_formula = "ht ~ cs(time, df = 3)",
                      gamsigma_formula = "~ cs(time, df = 1)",
                      match_plot = FALSE,
                      predict_plot = FALSE,
                      sbj = .)),
                    .progress = list(type = "iterator",
                                     format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                     clear = TRUE))


## test_mhl_p080-------------------
test_mhl_p075 <- map(id_test,
                     ~try(pred_matching(
                       lb_data = lb_train,
                       lb_test = lb_test,
                       test_data = test_data,
                       train_data = train_data,
                       match_methods = "mahalanobis",
                       match_alpha = 0.75,
                       gamlss_formula = "ht ~ cs(time, df = 3)",
                       gamsigma_formula = "~ cs(time, df = 1)",
                       match_plot = FALSE,
                       predict_plot = FALSE,
                       sbj = .)),
                     .progress = list(type = "iterator",
                                      format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                      clear = TRUE))

## test_mhl_p085-------------------
test_mhl_p080 <- map(id_test,
                     ~try(pred_matching(
                       lb_data = lb_train,
                       lb_test = lb_test,
                       test_data = test_data,
                       train_data = train_data,
                       match_methods = "mahalanobis",
                       match_alpha = 0.80,
                       gamlss_formula = "ht ~ cs(time, df = 3)",
                       gamsigma_formula = "~ cs(time, df = 1)",
                       match_plot = FALSE,
                       predict_plot = FALSE,
                       sbj = .)),
                     .progress = list(type = "iterator",
                                      format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                      clear = TRUE))

## test_mhl_p090-------------------
test_mhl_p085 <- map(id_test,
                    ~try(pred_matching(
                      lb_data = lb_train,
                      lb_test = lb_test,
                      test_data = test_data,
                      train_data = train_data,
                      match_methods = "mahalanobis",
                      match_alpha = 0.85,
                      gamlss_formula = "ht ~ cs(time, df = 3)",
                      gamsigma_formula = "~ cs(time, df = 1)",
                      match_plot = FALSE,
                      predict_plot = FALSE,
                      sbj = .)),
                    .progress = list(type = "iterator",
                                     format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                     clear = TRUE))

## test_mhl_p095-------------------
test_mhl_p090 <- map(id_test,
                    ~try(pred_matching(
                      lb_data = lb_train,
                      lb_test = lb_test,
                      test_data = test_data,
                      train_data = train_data,
                      match_methods = "mahalanobis",
                      match_alpha = 0.90,
                      gamlss_formula = "ht ~ cs(time, df = 3)",
                      gamsigma_formula = "~ cs(time, df = 1)",
                      match_plot = FALSE,
                      predict_plot = FALSE,
                      sbj = .)),
                    .progress = list(type = "iterator",
                                     format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                     clear = TRUE))


test_mhl_p095 <- map(id_test,
                     ~try(pred_matching(
                       lb_data = lb_train,
                       lb_test = lb_test,
                       test_data = test_data,
                       train_data = train_data,
                       match_methods = "mahalanobis",
                       match_alpha = 0.95,
                       gamlss_formula = "ht ~ cs(time, df = 3)",
                       gamsigma_formula = "~ cs(time, df = 1)",
                       match_plot = FALSE,
                       predict_plot = FALSE,
                       sbj = .)),
                     .progress = list(type = "iterator",
                                      format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                      clear = TRUE))

print("5.1 mahalanobis p09 -----------------------")
## test_eld_n10-----------------------
test_eld_n10 <- map(id_test,
                    ~pred_matching(
                      lb_data = lb_train,
                      lb_test = lb_test,
                      test_data = test_data,
                      train_data = train_data,
                      match_methods = "euclidean",
                      match_num = 10,
                      gamlss_formula = "ht ~ cs(time, df = 3)",
                      gamsigma_formula = "~ cs(time, df = 1)",
                      match_plot = FALSE,
                      predict_plot = FALSE,
                      sbj = .),
                    .progress = list(type = "iterator",
                                     format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                     clear = TRUE))
print("5.2 euclidean n10 -----------------------")
## test_mhl_n10 ----------------
test_mhl_n10 <- map(id_test,
                    ~pred_matching(
                      lb_data = lb_train,
                      lb_test = lb_test,
                      test_data = test_data,
                      train_data = train_data,
                      match_methods = "mahalanobis",
                      match_num = 10,
                      gamlss_formula = "ht ~ cs(time, df = 3)",
                      gamsigma_formula = "~ cs(time, df = 1)",
                      match_plot = FALSE,
                      predict_plot = FALSE,
                      sbj = .),
                    .progress = list(type = "iterator",
                                     format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                     clear = TRUE))
print("5.3 mahalanobis n10-----------------------")


test_sgl10_n10 <- map(id_test,
                      ~pred_matching(
                        lb_data = lb_train,
                        lb_test = lb_test,
                        test_data = test_data,
                        train_data = train_data,
                        match_methods = "single",
                        match_num = 10,
                        match_time = 12,
                        match_alpha = NULL,
                        gamlss_formula = "ht ~ cs(time, df = 3)",
                        gamsigma_formula = "~ cs(time, df = 1)",
                        match_plot = FALSE,
                        predict_plot = FALSE,
                        sbj = .),
                      .progress = list(type = "iterator",
                                       format = "Calculating {cli::pb_bar} {cli::pb_percent}",
                                       clear = TRUE))


lmm_test <- lmm_pred(train_data, test_data, test_baseline)


save(test_eld_n10,
     test_mhl_n10,
     test_mhl_p060,
     test_mhl_p065,
     test_mhl_p070,
     test_mhl_p075,
     test_mhl_p080,
     test_mhl_p085,
     test_mhl_p090,
     test_mhl_p095,
     test_sgl10_n10,
     file = paste0("anchor_time_", list(pred_time),
                   "_alpha_", list(alpha),
                   "_", Sys.time(), ".Rdata"))


save(lmm_test,
     file = paste0("results_lmm_", Sys.time(), ".Rdata"))
