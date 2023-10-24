
ui <- fluidPage(
  titlePanel("People like me methods"),

  sidebarPanel(
    sidebarPanel(
      selectInput("individual",
                  "The Patient ID:",
                  choices = unique(all$id),
                  width = "100%"),
      numericInput("alpha",
                   "Critical values for Mahalanobis:",
                   0.95,
                   min = 0, max = 1,
                   step = 0.05),
      numericInput("num",
                   "Matching number for people like me:",
                   10,
                   min = 3, max = 50,
                   step = 1),
      numericInput("time",
                   "Matching time for single time:",
                   12,
                   min = 2, max = 16,
                   step = 2),

      actionButton("run",
                   "Run!",
                   class = "btn-lg btn-success")
      )
  ),

  mainPanel(
    tabsetPanel(
      tabPanel("Mahalanobis p", plotOutput("mhl_p_plot")),
      tabPanel("Mahalanobis n", plotOutput("mhl_n_plot")),
      tabPanel("Euclidean n", plotOutput("eld_n_plot")),
      tabPanel("Single time n", plotOutput("sgl_n_plot")),
      tabPanel("Summary Table", tableOutput("table")),
    )
  )
)
