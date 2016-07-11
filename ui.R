
library(shinyjs)
shinyUI(fixedPage(
  useShinyjs(),
  titlePanel("Intraday trade volume forecast - PAR and NBAR"),br(),
  column(width=3,
    wellPanel(
      helpText("Input any stock ticker available on yahoo finance, e.g. NVO (Novo Nordisk A/S), ^GSPC (S&P500), ^DJI (Dow Jones index)"),
      textInput("symb", "Symbol", "DANSKE.CO"),
      sliderInput("days","Previous number of days",value=2,min=1,max=5),
      #numericInput("days","Previous number of days",value=5,min=1,max=15),
      # dateRangeInput("dates", "Date range",start = "2000-01-01",end = as.character(Sys.Date())),
      # dateRangeInput("plotdates","Plot range",start = "2012-01-01",end = as.character(Sys.Date())),
      actionButton("get", "Get volume data")),
    wellPanel(
      numericInput("p", label = "p (y-lags)", value = 1, min=1, max=20),
      numericInput("q", label = "q (lambda-lags)", value = 1, min=0, max=20),
      checkboxInput("season", label = "Allow negative parameters", value = FALSE),
      actionButton("est", "Estimate"),actionButton("forecast", "Forecast")),
    #wellPanel(
      #numericInput("h", label = "Forecast horizon", value = 24, min=1, max=50)
      # actionButton("forecast","Forecast")
      #),
      verbatimTextOutput("optim_mess")
  
  ),
  column(width=9,
      fluidRow(
        column(7,plotOutput("volume_plot")),
        column(5,plotOutput("qq_plot"))
      ),
      fluidRow(
        # column(4,h4("Information criteria"),tableOutput("info")),
        column(8,
               fluidRow(h4("PAR estimates"),tableOutput("par.par")),
                fluidRow(h4("NBAR estimates"),tableOutput("nbar.par"))
               )
        ))
,title = "Count Time Series")
)