## 7.1 meanout -----------------------------------------------------------------
meanout <- function(dataset,
                    term = c("bias", "mse", "coverage50",
                             "coverage80", "coverage90"),
                    ...){

  result <- dataset %>%
    # output_eld_n10 %>%
    # unlist(recursive = FALSE) %>%
    map("centiles_observed")

  if (term != "mse") {
    result <- result %>%
      map(term) %>%
      map(~mean(., na.rm = TRUE)) %>%
      unlist() %>%
      mean()
  } else {
    result <- result %>%
      map(~.$bias ^2) %>%
      map(~mean(., na.rm = TRUE)) %>%
      unlist() %>%
      mean()
  }


  return(result)
}

## 7.2 meanout -----------------------------------------------------------------
meanall <- function(dataset,
                    ...){
  bias <- meanout(dataset, "bias")
  mse <- meanout(dataset, "mse")
  cov50 <- meanout(dataset, "coverage50")
  cov80 <- meanout(dataset, "coverage80")
  cov90 <- meanout(dataset, "coverage90")

  return(list(bias = bias,
              mse = mse,
              cov50 = cov50,
              cov80 = cov80,
              cov90 = cov90))
}

## 7.3 pull out the Rdata into files -------------------------------------------
pullout <- function(location,
                    filename) {
  load(paste0(location, filename))

  ## eld_n test {{{-------------------------------------------------------------
  eld_n <- meanall(test_eld_n10)

  ## mhl_n test {{{-------------------------------------------------------------
  mhl_n <- meanall(test_mhl_n10)

  ## mhl_p test {{{-------------------------------------------------------------
  mhl_p <- meanall(test_mhl_p080)

  ## sgl_n test {{{-------------------------------------------------------------
  sgl_n <- meanall(test_sgl10_n10)

  if (exists("lmm_test")) {
    lmm <- list()
    lmm$bias <-  lmm_test %>%
      transmute(bias = abs(pred - ht)) %>%
      unlist() %>%
      mean()
    lmm$mse <-  lmm_test %>%
      transmute(mse = (pred - ht)^2) %>%
      unlist() %>%
      mean()
    lmm$cov50<- lmm_test %>%
      dplyr::select(coverage50) %>%
      unlist() %>%
      mean()
    lmm$cov80<- lmm_test %>%
      dplyr::select(coverage80) %>%
      unlist() %>%
      mean()
    lmm$cov90<- lmm_test %>%
      dplyr::select(coverage90) %>%
      unlist() %>%
      mean()
  } else {
    lmm = NULL
  }

  return(list(eld_n = eld_n,
              mhl_n = mhl_n,
              mhl_p = mhl_p,
              sgl_n = sgl_n,
              lmm = lmm))
}



## 7.4 pullout the time points and alpha study ------------------------------
pulltime <- function(location, filename) {
  load(paste0(location, filename))

  ## eld_n test {{{-------------------------------------------------------------
  eld_n <- meanall(test_eld_n10)

  ## mhl_n test {{{-------------------------------------------------------------
  mhl_n <- meanall(test_mhl_n10)

  ## sgl_n test {{{-------------------------------------------------------------
  sgl_n <- meanall(test_sgl10_n10)

  ## mhl_p test {{{-------------------------------------------------------------
  mhl_p7 <- meanall(test_mhl_p095)
  mhl_p6 <- meanall(test_mhl_p090)
  ## p080 and p085
  mhl_p5 <- meanall(test_mhl_p085)
  mhl_p4 <- meanall(test_mhl_p080)
  mhl_p3 <- meanall(test_mhl_p075)
  mhl_p2 <- meanall(test_mhl_p070)
  mhl_p1 <- meanall(test_mhl_p060)

  return(list(eld_n = eld_n,
              mhl_n = mhl_n,
              mhl_p1 = mhl_p1,
              mhl_p2 = mhl_p2,
              mhl_p3 = mhl_p3,
              mhl_p4 = mhl_p4,
              mhl_p5 = mhl_p5,
              mhl_p6 = mhl_p6,
              mhl_p7 = mhl_p7,
              sgl_n = sgl_n))
}


