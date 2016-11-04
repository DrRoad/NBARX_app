
library(shinyjs)
shinyUI(fixedPage(
  useShinyjs(),
  titlePanel("Intraday trade volume forecast - PAR and NBAR"),br(),
  tabsetPanel(
    tabPanel("Estimation",
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
          actionButton("est", "Estimate and forecast")),
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
    ),
    tabPanel("Info",
        withMathJax(),
        h4("The app"),
        p("This app is made in R Shiny and the source code can be found on" ,a("github",href="http://github.com/duffau/nbarx_app"),"."),
        h4("The data"),
        p("The trade volume data is queried live through Yahoo!-finance open API. Intraday data is available in two frequencies, 1 min and 5 min intervals.
          The 1 min interval data is only available for the most recent trade day, whereas the 5 min data is available
          for the past 15 trading days. This can be seen when selecting the '1' in the 'Previous day number of days' slider. The data is available from", 
          a("81 exchanges worldwide",href="https://help.yahoo.com/kb/finance/SLN2310.html"), " and arrives, for the most part, in real-time, 
          but for some exchanges, can have up to a 30 minute delay."),
        h4("The models"),
        p("The app uses a Poisson autoregression (PAR) (Rydberg & Shephard (2000)) and a Negative Binomial autoregression (NBAR) (Davis & Liu (2009)) to model and 
          forecast the intraday trade volume of a chosen stock. The Poisson model assumes that the current stock counts, \\(y_t\\), 
          are Poisson distributed, when conditioning on the entire past, \\(\\mathcal{F}_{t-1}\\). The NBAR model assumes a conditional 
          negative binomial distribution. Put symbolically,

          $$\\text{PAR-model:}\\qquad y_t|\\mathcal{F}_{t-1}\\sim\\text{Poisson}\\left(\\lambda_t\\right)$$
          $$\\text{NBAR-model:}\\qquad y_t|\\mathcal{F}_{t-1}\\sim\\text{NegBin}\\left(\\lambda_t,\\nu\\right).$$

          Both model assume the conditional mean of the volume, \\(\\lambda_t\\), follows the recursion,

          $$\\lambda_t=\\omega+\\alpha_1 y_{t-1}+\\cdots+\\alpha_p y_{t-p}+\\beta_1 \\lambda_{t-1}+\\cdots+\\beta_q \\lambda_{t-q}.$$ 

          The default values are \\(p=1\\) and \\(q=1\\) renders the model,

          $$\\lambda_t=\\omega+\\alpha_1 y_{t-1}+\\beta_1 \\lambda_{t-1}$$

          so at each point in time the current mean of the trade counts, is a function of the previous counts 
          (\\(y_{t-1}\\)) and the previous mean (\\(\\lambda_{t-1}\\))."),
        p("The only observed variables are the counts \\(y_t\\), and all other quantities need to be estimated. This is done by recursively 
          calculating the conditional mean, given a set of parameters \\(\\alpha_1,\\ldots\\alpha_p,\\beta_1\\ldots,\\beta_q\\) 
          (and \\(\\nu\\) in the case of NBAR), and using the sequence of means to calculate the likelihood of the observed data. This likelihood is numerically
          maximized with respect to the parameters, producing maximum likelihood estimates."),
        p("The blue and red curves in the time plot represent the \\(\\lambda\\)-recursion with the parameters set to the maximum likelihood estimates, 
          which can be seen as a one-step forecast of \\(\\lambda_t\\)."),
        h4("Live forecast"),
        p("During an open trading day, a multiperiode forecast of the two models conditional mean, is plotted in the time plot figure. They are represented 
          by thick dashed curves. Accompanying them are the 90% confidence bands of the projection. When the exchange is closed no multiperiode forecast is
          plotted. So if you want to see a multiperiod forecast in action, simply choose a stock from an",a("exchange",href="https://help.yahoo.com/kb/finance/SLN2310.html"), 
          "which is open at the given time. It is (a.s.) possible to find an", 
          a("open exchange at any time",href="https://en.wikipedia.org/wiki/List_of_stock_exchange_opening_times"),"."),
        h4("The pseudo-residuals"),
        p("The",a("Q-Q plot",href="https://en.wikipedia.org/wiki/Q%E2%80%93Q_plot"),"is a graphical inspection of the fit of the model.
          It cross-plots the empirical quantiles of the prediction errors, the residuals, with the quantiles of residuals theoretical distribution.
          Since we are interested in comparing two models with different distributional assumptions we make us of the",a("probability integtral transform (PIT)",href="https://en.wikipedia.org/wiki/Probability_integral_transform"),
          "to get the, so called, pseudo-residuals with an induced normal distribution. These normal pseudo-residuals follow a standard normal distribution by condtrunction,
          given the underlying model has the correct distribution. This makes model comparison easy."),
        p("The formal definition of the pseudo-residual, \\(r_t\\) used is,
          $$r_t = \\left(r_t^+ + r_t^-\\right)/2$$ with,$$r_t^+=\\Phi^{-1}\\left(F(y_t,\\theta_t)\\right)$$ and $$r_t^-=\\Phi^{-1}\\left(F(y_t-1,\\theta_t)\\right).$$
          Here \\(F\\) is the cumulative distribution function of the given model, so either the Poisson or the Negative Binomial, and \\(\\theta_t\\) is a vector
          holding the neccesary parameters for the given distribution. So in the case of PAR we have \\( \\theta_t = \\lambda_t\\) and for NBAR \\(\\theta_t=(\\lambda_t,\\nu)\\).
          \\(\\Phi^{-1}\\) is the quantile function of the standard normal distribution. So \\(F\\) transforms the counts from the integers the reals between zero and one, 
          $$F:\\mathbb{N}_0\\rightarrow(0,1)$$
          and the nornmal quantile function transform the interval (0,1) to the whole real line,
          $$\\Phi^{-1}:(0,1)\\rightarrow\\mathbb{R}.$$ If the original conditional distribution of \\(y_t\\) was exactly \\(F\\) then the
          \\(F\\)-transformation would give a uniformely distributed variable, and transforming this uniform variable with the 
          normal quantile function would yield a standard normal distributed variable. So deviations of the pseudo-residuals from the standard
          normal is a sign that the (conditional) distribution of \\(y_t\\)  deviates from the assumed \\(F\\)."),
        h4("References"),
        p("A modelling Framework for the Prices and Times of Trade on the New York Stock Exchange, T. H. Rydbjerg and N. Shephard, Non-linear and Nonstationary Signal Processing, 2000."),
        p("Theory and Inference for a Class of Observation-Driven Models with Application to Time Series of Counts,Richard A. Davis and Heng Liu, ArXiv, 2012.")
    )
  )
,title = "Count Time Series")
)