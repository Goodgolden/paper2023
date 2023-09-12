

# functions used in the project ------------------------------------------------
## 0.1 norm L2 -------------
norm2 <- function(v) {
  sqrt(sum(v^2))
}

## 0.3 not all na ----------------
not_all_na <- function(x) {
  any(!is.na(x))
}

## 0.4 not any na ---------------
not_any_na <- function(x) {
  all(!is.na(x))
}

## 0.5 not in ----------------
`%!in%` <- Negate(`%in%`)


## 0.8 euclidean_n -----------------
euclidean_n <- function(Dmatrix,
                        match_num) {
  matching <<- Dmatrix %>%
    apply(2, norm, type = "2") %>%
    ## using Frobenius norm
    # apply(lb_sub, 2, norm, type = "f") %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(diff) %>%
    slice(1:match_num)

  return(matching)
}

## 0.9 singletime_n ---------------------
singletime_n <- function(Dmatrix,
                         match_time,
                         match_num) {
  matching <<- Dmatrix %>%
    filter(as.numeric(rownames(.)) == match_time) %>%
    t() %>%
    ## using Frobenius norm
    # apply(lb_sub, 2, norm, type = "f") %>%
    as.data.frame() %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    arrange(abs(diff)) %>%
    slice(1:match_num)

  return(matching)
}



## 2.1 lmm dynamic prediction -----------------------
lmm_pred <- function(train_data,
                     test_data,
                     baseline) {
  # baseline <- test_baseline
  ctrl <- lmeControl(opt = 'optim')
  fitting <-  lme(ht ~ bs(time, knots = c(10, 12, 15), degree = 3) * sex - 1,
                  random = ~ 1 + time| id,
                  control = ctrl,
                  data = train_data)

  time_vec <- unique(test_data$time)
  lmmpred_90 <- IndvPred_lme(
    lmeObject = fitting,
    newdata = baseline,
    timeVar = "time",
    M = 500,
    times = time_vec,
    all_times = TRUE,
    return_data = TRUE,
    level = 0.9,
    interval = "prediction",
    seed = 555) %>%
    dplyr::select(id, time,
                  # observed = ht,
                  pred,
                  centile05 = low,
                  centile95 = upp)


  lmmpred_80 <- IndvPred_lme(
    lmeObject = fitting,
    newdata = baseline,
    timeVar = "time",
    M = 500,
    times = time_vec,
    all_times = TRUE,
    return_data = TRUE,
    level = 0.8,
    interval = "prediction",
    seed = 555) %>%
    dplyr::select(id, time,
                  # observed = ht,
                  pred,
                  centile10 = low,
                  centile90 = upp)

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
    seed = 555) %>%
    dplyr::select(id, time,
                  # observed = ht,
                  pred,
                  centile25 = low,
                  centile75 = upp)

  lmm <- lmmpred_50 %>%
    full_join(lmmpred_80) %>%
    full_join(lmmpred_90) %>%
    # mutate(time = round(time, 2)) %>%
    ## na.omit() %>%
    as.data.frame() %>%
    mutate(id = as.character(id)) %>%
    right_join(test_data) %>%
    dplyr::filter(time != 0) %>%
    mutate(coverage50 = ifelse(ht >= `centile25` & ht <= `centile75`, 1, 0),
           coverage80 = ifelse(ht >= `centile10` & ht <= `centile90`, 1, 0),
           coverage90 = ifelse(ht >= `centile05` & ht <= `centile95`, 1, 0),
           # mse = (actual - `50`)^2,
           # biassq = bias^2,
           # var = mse - bias^2,
           bias = abs(ht - pred))
  return(lmm)
}



## 3.1 generate spline ----------------------------------
gen_spline <- function(x, knots, degree, theta) {
  basis <- bs(x = x,
              knots = knots,
              degree = degree,
              intercept = FALSE)

  y.spline <- basis %*% theta
  dt <- data.table::data.table(x = x, y.spline = as.vector(y.spline))

  return(list(dt = dt,
              basis = basis,
              knots = knots))
}

## 3.2 plot spline basis -------------------------------
plot_basis <- function(basisdata) {
  dtbasis <- as.data.table(basisdata$basis)
  dtbasis[, x := seq(0, 1, length.out = .N)]
  dtmelt <- melt(data = dtbasis,
                 id = "x",
                 variable.name = "basis",
                 variable.factor = TRUE)

  ggplot(data = dtmelt,
         aes(x = x, y = value, group = basis)) +
    geom_line(aes(color = basis), size = 1) +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(0, 1),
                       breaks = c(0, basisdata$knots, 1)) +
    theme(panel.grid.minor = element_blank())
}

## 3.3 plot spline ----------------------------
plot_spline <- function(basisdata, points = FALSE) {
  p <- ggplot(data = basisdata$dt)

  if (points) {p <- p + geom_point(aes(x = x, y = y), color = "grey75")}

  p <- p + geom_line(aes(x = x, y = y.spline), color = "red", size = 1) +
    # scale_y_continuous(limits = c(0, 1)) +
    # scale_x_continuous(limits = c(0, 1), breaks = knots) +
    theme(panel.grid.minor = element_blank())

  return(p)
}

## 3.4 generate bspline individual random effect ----------------------
gen_ind_ranef <- function(subset,
                          knots = c(10, 12, 15),
                          degree = 3,
                          sigma = 1.04,
                          vcov = vcov5) {

  ran_coef <- MASS::mvrnorm(n = 1,
                            mu = rep(0, nrow(vcov)),
                            Sigma = vcov)

  ran_ef <- gen_spline(subset$time,
                       knots = knots,
                       degree = degree,
                       theta = ran_coef)

  res_error <- rnorm(n = nrow(subset), sd = sigma)
  random <- ran_ef$dt + res_error
  return(random)
}

## 3.5 generate all bspline random effect -------------------
gen_all_ranef <- function(fullset = margin_mean,
                          id = "id",
                          knots = c(10, 12, 15),
                          degree = 3,
                          sigma = 1.04,
                          vcov = vcov5,
                          seed) {
  set.seed(seed)
  simulation_random <- margin_mean %>%
    group_by(id) %>%
    group_map(~gen_ind_ranef(subset = .x,
                             knots = knots,
                             degree = degree,
                             sigma = sigma,
                             vcov = vcov),
              .keep = TRUE) %>%
    map("y.spline") %>%
    unlist()
  simulation_full <- margin_mean %>%
    mutate(varibility = simulation_random) %>%
    mutate(ht = `.fixed` + varibility)

  return(simulation_full)
}


## 3.4b generate simple individual random effect -----------------------
gen_ind_rans <- function(subset,
                          sigma = sigma5,
                          vcov = vcov5) {

  ran_coef <- MASS::mvrnorm(n = 1,
                            mu = rep(0, nrow(vcov)),
                            Sigma = vcov)
  ran_ef <- ran_coef[[2]] * subset$time  + ran_coef[[1]]
  res_error <- rnorm(n = nrow(subset), sd = sigma)
  random <- ran_ef + res_error
  return(random)
}

## 3.5b generate simple all random effect ---------------------------
gen_all_rans <- function(fullset = margin_mean,
                          id = "id",
                          sigma = sigma5,
                          vcov = vcov5,
                          seed) {
  set.seed(seed)
  simulation_random <- margin_mean %>%
    group_by(id) %>%
    group_map(~gen_ind_ranef(subset = .x,
                             sigma = sigma,
                             vcov = vcov),
              .keep = TRUE) %>%
    unlist()
  simulation_full <- margin_mean %>%
    mutate(varibility = simulation_random) %>%
    mutate(ht = `.fixed` + varibility)

  return(simulation_full)
}


pred_matching <- function(lb_data,
            lb_test,
            train_data,
            test_data,
            match_methods = c("mahalanobis", "euclidean", "single"),
            match_num = NULL,
            match_alpha = NULL,
            match_time = NULL,
            gamlss_formula = "ht ~ cs(time, df = 3)",
            gamsigma_formula = "~ cs(time, df = 1)",
            match_plot = FALSE,
            predict_plot = FALSE,
            sbj) {
  if (is.null(match_num) & is.null(match_alpha)) {
    stop("provide matching number or critical values for PLM methods")
  }
  if (!is.null(match_num) & !is.null(match_alpha)) {
    stop("provide either matching number or critical values for PLM methods, not both")
  }
  
  subject <- lb_test %>%
    mutate(id = as.character(id)) %>%
    dplyr::filter(id == sbj)
  
  ind_time <- test_data %>%
    mutate(id = as.character(id)) %>%
    dplyr::filter(id == sbj)
  
  ## the matching subset
  lb_sub <- lb_data %>%
    mutate(id = as.character(id)) %>%
    dplyr::transmute(id = as.character(id),
                     ## more time points for matching
                     ## adding the correlation
                     time = time,
                     diff = lm_bks_target - subject$lm_bks_target) %>%
    ## must remove the self data in training dataset
    dplyr::filter(id != sbj) %>%
    pivot_wider(names_from = "id",
                values_from = "diff") %>%
    remove_rownames() %>%
    column_to_rownames("time")
  
  if (match_methods == "euclidean") {
    matching <<- euclidean_n(Dmatrix = lb_sub,
                             match_num = match_num) %>%
      inner_join(train_data, by = "id")
    cat("\n using euclidean distance \n")
  }
  
  if (match_methods == "mahalanobis") {
    if (!is.null(match_num)) {
      
      matching <<- mahalanobis_n(Dmatrix = lb_sub,
                                 match_num = match_num) %>%
        inner_join(train_data, by = "id")
      cat("\n using mahalanobis distance with matching number \n")}
    
    if (!is.null(match_alpha)) {
      matching <<- mahalanobis_p(Dmatrix = lb_sub,
                                 alpha = match_alpha) %>%
        inner_join(train_data, by = "id")
      cat("\n using mahalanobis distance with F test p value \n")}
  }
  
  if (match_methods == "single") {
    matching <<- singletime_n(Dmatrix = lb_sub,
                              match_time = match_time,
                              match_num = match_num) %>%
      inner_join(train_data, by = "id")
    
    cat("\n using single critical time point matching \n")
  }
  
  if (match_plot == TRUE) {
    matching_plot <- ggplot(matching) +
      geom_line(aes(x = time, y = ht,
                    group = id),
                color = "grey",
                linetype = "dashed") +
      geom_line(data = ind_time,
                aes(x = time, y = ht),
                color = "darkblue",
                size = 1) +
      ggtitle(sbj) +
      xlim(0, 17) +
      ylim(50, 250) +
      theme_bw()
    
    cat("\n plotting matching paired individual trajectories \n")
  } else {
    matching_plot = NULL
  }
  
  
  ## fitting gamlss model for
  plm <- gamlss::gamlss(as.formula(gamlss_formula),
                        sigma.formula = as.formula(gamsigma_formula),
                        # nu.formula = ~cs(time^0.1, df=1),
                        # tau.formula = ~cs(time^0.5, df=1),
                        method = RS(100),
                        data = matching,
                        family = NO)
  
  cat("\n gamlss model fitting is done \n")
  centiles_obs <-  gamlss::centiles.pred(plm,
                                         type = c("centiles"),
                                         xname = "time",
                                         xvalues = c(ind_time$time),
                                         cen = c(5, 10, 25, 50, 75, 90, 95)) %>%
    cbind(actual = ind_time$ht) %>%
    as.data.frame() %>%
    mutate(coverage50 = ifelse(actual >= `C25` & actual <= `C75`, 1, 0),
           coverage80 = ifelse(actual >= `C10` & actual <= `C90`, 1, 0),
           coverage90 = ifelse(actual >= `C5` & actual <= `C95`, 1, 0),
           # mse = (actual - `50`)^2,
           # biassq = bias^2,
           # var = mse - bias^2,
           bias = abs(actual - `C50`))
  # centiles_obs <-
  #   centiles.pred(plm, linetype = "centiles",
  #                 xname = "time",
  #                 xvalues = c(ind_time$time),
  #                 cen = c(5, 10, 25, 50, 75, 90, 95)) %>%
  #   cbind(actual = ind_time$ht) %>%
  #   as.data.frame() %>%
  #   mutate(coverage50 = ifelse(actual >= `25` & actual <= `75`, 1, 0),
  #          coverage80 = ifelse(actual >= `10` & actual <= `90`, 1, 0),
  #          coverage90 = ifelse(actual >= `5` & actual <= `95`, 1, 0),
  #          # mse = (actual - `50`)^2,
  #          # biassq = bias^2,
  #          # var = mse - bias^2,
  #          bias = abs(actual - `50`))
  cat("\n gamlss model prediction for observed time points are done \n")
  centiles_pred <-
    centiles.pred(plm, linetype = c("centiles"),
                  xname = "time",
                  xvalues = c(0:17),
                  cent = c(5, 10, 25, 50, 75, 90, 95),
                  plot = FALSE,
                  legend = T) %>%
    dplyr::select(time = 1,
                  q05 = 2,
                  q10 = 3,
                  q25 = 4,
                  q50 = 5,
                  q75 = 6,
                  q90 = 7,
                  q95 = 8) %>%
    mutate(cfint90 = q95 - q05,
           cfint80 = q90 - q10,
           cfint50 = q75 - q25)
  cat("\n gamlss model prediction for predicted time points are done \n")
  if (predict_plot == TRUE) {
    plm_plot <- plm_ind_plot(quantile = centiles_pred,
                             observation = ind_time,
                             title = unique(ind_time$id))
  } else {
    plm_plot <- NULL
  }
  
  return(list(centiles_observed = centiles_obs,
              centiles_predicted = centiles_pred,
              matching_trajectory = matching_plot,
              predictive_centiles = plm_plot))
}

## 1.4 individual people-like-me matching plot -------------------
plm_ind_plot <- function(quantile,
                         observation,
                         title = NULL) {
  
  plot <- ggplot() +
    geom_line(data = quantile, aes(x = time, y = q05),
              color = "#00A9FF", linetype = "dashed") +
    geom_line(data = quantile, aes(x = time, y = q95),
              color = "#00A9FF", linetype = "dashed") +
    geom_ribbon(data = quantile,
                aes(x = time, ymin = q05, ymax = q95),
                fill = "#00A9FF") +
    geom_line(data = quantile, aes(x = time, y = q10),
              color = "#5FBB68", linetype = "dashed") +
    geom_line(data = quantile, aes(x = time, y = q90),
              color = "#5FBB68", linetype = "dashed") +
    geom_ribbon(data = quantile,
                aes(x = time, ymin = q10, ymax = q90),
                fill = "#5FBB68") +
    geom_line(data = quantile, aes(x = time, y = q25),
              color = "#F9D23C", linetype = "dashed") +
    geom_line(data = quantile, aes(x = time, y = q75),
              color = "#F9D23C", linetype = "dashed") +
    geom_ribbon(data = quantile,
                aes(x = time, ymin = q25, ymax = q75),
                fill = "#F9D23C") +
    geom_line(data = quantile, aes(x = time, y = q50),
              color = "darkblue", linetype = "dashed") +
    geom_point(data = observation, aes(x = time, y = ht),
               color = "black", size = 1) +
    theme_bw() +
    xlab("Time (yr)") +
    ylab("Height (cm)") +
    ggtitle(title) +
    xlim(0, 17) +
    ylim(50, 250)
  
  plot
}




## 0.8 mahalanobis p ------------------
mahalanobis_p <- function(Dmatrix,
                          alpha) {
  def <- nrow(Dmatrix)
  df <- Dmatrix %>%
    ## Mahalanobis distance using the chisq pvalues
    as.matrix() %>%
    t()
  x <- sweep(df, 2L, 0)
  invcov <- MASS::ginv(cov(df))
  value <- rowSums(x %*% invcov * x)
  pvalue <- pchisq(value, df = def, lower.tail = FALSE)
  matching <<- data.frame(diff = value,
                          pvalue = as.numeric(pvalue)) %>%
    arrange(desc(pvalue)) %>%
    rownames_to_column("id") %>%
    as.data.frame() %>%
    dplyr::filter(pvalue >= alpha)
  
  # slice(1:match_num) %>%
  # inner_join(obs_data, by = "id")
  return(matching)
}

## 0.9 mahalanobis_n ---------------
mahalanobis_n <- function(Dmatrix,
                          match_num) {
  def <- nrow(Dmatrix)
  df <- Dmatrix %>%
    as.matrix() %>%
    t()
  x <- sweep(df, 2L, 0)
  invcov <- MASS::ginv(cov(df))
  value <- rowSums(x %*% invcov * x)
  
  matching <<- Dmatrix %>%
    t() %>%
    as.data.frame() %>%
    mutate(value = value) %>%
    arrange(value) %>%
    dplyr::select(diff = 1) %>%
    rownames_to_column("id") %>%
    slice(1:match_num)
  
  return(matching)
}
