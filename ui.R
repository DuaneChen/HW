library(shiny)

fluidPage(
        titlePanel("Binomial Model and Black - Scholes Model"),
        sidebarLayout(
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
                        br()
                ),
                
                mainPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Overview", value = 1,
                                             h3('Option Data'),
                                             h4('Binomial Model'),
                                             tableOutput("price_binomial"), br(),
                                             h4('Black - Scholes Model'), 
                                             tableOutput("price_BS"), br()),
                                    tabPanel("Binomial Model converge to Black - Scholes Model by time", value = 2,
                                             tableOutput("price_time"), br(),
                                             plotOutput("plot_price_converge_call"), br(),
                                             plotOutput("plot_price_converge_put")),
                                    tabPanel("Plot of Call and Put Options (Black - Scholes Model)", value = 3,
                                             h3('Plot of Call and Put Options (Black - Scholes Model)'),
                                             h4("Call Option"),
                                             plotOutput("callPlot_BS"), br(),
                                             h4("Put Option"),
                                             plotOutput("putPlot_BS"))
                        )
                )
        )
)
