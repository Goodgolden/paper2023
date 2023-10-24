server <- function(input, output) {

  sid <- reactive({sid <- input$individual})
  # match_methods <- reactive(input$methods)
  alpha <- reactive({alpha <- input$alpha})
  num <- reactive({num <- input$num})
  time <- reactive({time <- input$time})

  observeEvent(input$run, {  })

  bks_pred <-
    plmlmm::brokenstick_prediction(
      outcome = "ht",
      time = "time",
      id = "id",
      train_data = train,
      knots = c(5, 10, 12),
      pred_time = c(2, 4, 6, 8, 10, 12, 14, 16),
      choice = "predicted")

  lb_data <-
    plmlmm::linear_brokenstick(
      lm_formula = "`.pred` ~ timef * sex + baseline",
      bks_pred = bks_pred)

#
#   tabsetPanel(
#     tabPanel("Mahalanobis p", plotOutput("mhl_p_plot")),
#     tabPanel("Mahalanobis n", plotOutput("mhl_n_plot")),
#     tabPanel("Euclidean n", plotOutput("eld_n_plot")),
#     tabPanel("single time n", plotOutput("sgl_n_plot")),
#     tabPanel("Summary Table", tableOutput("table")),

  ## mahalanobis p value matching-----------------------------------------------
  output$mhl_p_plot <- renderPlot({

    req(input$individual,
        input$alpha,
        input$num,
        input$time,
        input$run)

    pm_mhl_p <-
      plmlmm::pred_matching(
        lb_data = lb_data,
        lb_test = lb_data,
        test_data = train,
        train_data = train,
        match_methods = "mahalanobis",
        match_alpha = alpha(),
        match_num = NULL,
        gamlss_formula = "ht ~ cs(time, df = 3)",
        gamsigma_formula = "~ cs(time, df = 1)",
        match_plot = TRUE,
        predict_plot = TRUE,
        sbj = sid())

    gridExtra::grid.arrange(pm_mhl_p$matching_trajectory +
                              labs(subtitle = "Mahalandobis distance with critical value 'matching'"),
                            pm_mhl_p$predictive_centiles +
                              labs(subtitle = "Mahalandobis distance with critical value 'prediction'"),
                            nrow = 1)
  })


  ## mahalanobis fixed matching number -------------------------------------
  output$mhl_n_plot <- renderPlot({

    req(input$individual,
        input$alpha,
        input$num,
        input$time,
        input$run)

    pm_mhl_n <-
      plmlmm::pred_matching(
        lb_data = lb_data,
        lb_test = lb_data,
        test_data = train,
        train_data = train,
        match_methods = "mahalanobis",
        match_num = num(),
        match_alpha = NULL,
        gamlss_formula = "ht ~ cs(time, df = 3)",
        gamsigma_formula = "~ cs(time, df = 1)",
        match_plot = TRUE,
        predict_plot = TRUE,
        sbj = sid())

    gridExtra::grid.arrange(pm_mhl_n$matching_trajectory +
                              labs(subtitle = "Mahalandobis distance with matching sample size'matching'"),
                            pm_mhl_n$predictive_centiles +
                              labs(subtitle = "Mahalandobis distance with matching sample size 'prediction'"),
                            nrow = 1)
  })


  ## euclidean fixed matching number -------------------------------------------
  output$eld_n_plot <- renderPlot({

    req(input$individual,
        input$alpha,
        input$num,
        input$time,
        input$run)

    pm_eld_n <-
      plmlmm::pred_matching(
        lb_data = lb_data,
        lb_test = lb_data,
        test_data = train,
        train_data = train,
        match_methods = "euclidean",
        match_num = num(),
        gamlss_formula = "ht ~ cs(time, df = 3)",
        gamsigma_formula = "~ cs(time, df = 1)",
        match_plot = TRUE,
        predict_plot = TRUE,
        sbj = sid())

    gridExtra::grid.arrange(pm_eld_n$matching_trajectory +
                              labs(subtitle = "Euclidean distance with matching sample size 'matching'"),
                            pm_eld_n$predictive_centiles +
                              labs(subtitle = "Euclidean distance with matching sample size 'prediction'"),
                            nrow = 1)
  })



  ## single time point matching number -----------------------------------------
  output$sgl_n_plot <- renderPlot({

    req(input$individual,
        input$alpha,
        input$num,
        input$time,
        input$run)

    pm_sgl_n <-
      plmlmm::pred_matching(
        lb_data = lb_data,
        lb_test = lb_data,
        test_data = train,
        train_data = train,
        match_methods = "single",
        match_num = num(),
        match_time = time(),
        gamlss_formula = "ht ~ cs(time, df = 3)",
        gamsigma_formula = "~ cs(time, df = 1)",
        match_plot = TRUE,
        predict_plot = TRUE,
        sbj = sid())

    gridExtra::grid.arrange(pm_sgl_n$matching_trajectory +
                              labs(subtitle = "Single time with matching sample size 'matching'"),
                            pm_sgl_n$predictive_centiles +
                              labs(subtitle = "Single distance with matching sample size 'prediction'"),
                            nrow = 1)
  })
  }
