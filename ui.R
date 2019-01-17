fluidPage(
        titlePanel("Binomial Model and Black - Scholes Model"),
        tabsetPanel(
                tabPanel("Homework (Binomial and Black - Scholes Model)", fluid = TRUE,
                         sidebarPanel(
                                 numericInput('stockprice', "Stock Price", 75),
                                 numericInput('strike', 'Strike Price', 69),
                                 sliderInput("maturity", "Time to Maturity (Years)", value = 1,
                                             min = 0, max = 10, step = 0.1),
                                 sliderInput("volatility", "Volatility (%)", value = 25,
                                             min = 0, max = 100, step = 0.1),
                                 sliderInput("riskfreerate", "Risk free rate (%)", value = 1,
                                             min = 0, max = 50, step = 0.1),
                                 sliderInput("n", "Binomial simulation time steps", value = 50,
                                             min = 1, max = 1000, step = 1),
                                 br()),
                         mainPanel(
                                 h4('Binomial Model'),
                                 tableOutput("price_binomial"),
                                 h4('Black - Scholes Model'), 
                                 tableOutput("price_BS"), br(),
                                 plotlyOutput("plot_price_converge_call"), br(),
                                 plotlyOutput("plot_price_converge_put"), br())
                ),
                
                
                tabPanel("Bonus (Implied Volatility, Payoff of Call and Put)", fluid = TRUE,
                         sidebarPanel(
                                 numericInput('stockprice2', "Stock Price", 75),
                                 numericInput('strike2', 'Strike Price', 69),
                                 numericInput('optionprice2', 'Option Price (For Implied Volatility)', 10),
                                 sliderInput("maturity2", "Time to Maturity (Years)", value = 1,
                                             min = 0, max = 10, step = 0.1),
                                 sliderInput("volatility2", "Volatility (%)", value = 25,
                                             min = 0, max = 100, step = 0.1),
                                 sliderInput("riskfreerate2", "Risk free rate (%)", value = 1,
                                             min = 0, max = 50, step = 0.1),
                                 br()),
                         mainPanel(
                                 h4('Implied Volatility'), 
                                 tableOutput("price_implied"),
                                 h3('Plot of Call and Put Options (Black - Scholes Model)'),
                                 h4("Call Option"),
                                 plotOutput("callPlot_BS"), br(),
                                 h4("Put Option"),
                                 plotOutput("putPlot_BS"), br()
                         )
                )
        )
)