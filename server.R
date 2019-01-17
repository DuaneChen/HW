library(shiny)
library(ggplot2)
library(derivmkts)
library(magrittr)
library(reshape2)
library(plotly)


function(input, output, session) {
        
        # Generate Binomial values
        Binomial <- function(S,K,t,sigma,rf,n) {
                sigma <- sigma/100
                rf <- rf/100
                u <- exp(sigma*sqrt(t/n))
                d <- 1/u
                q <- (exp(rf*t/n)-d)/(u-d)
                call_eu <- binomopt(S, K, sigma, rf, t, 0, n, american=F, putopt=F)
                call_am <- binomopt(S, K, sigma, rf, t, 0, n, american=TRUE, putopt=F)
                put_eu <- binomopt(S, K, sigma, rf, t, 0, n, american=F, putopt=TRUE)
                put_am <- binomopt(S, K, sigma, rf, t, 0, n, american=TRUE, putopt=TRUE)
                res <- list(call_eu=round(call_eu,4),call_am=round(call_am,4),
                            put_eu=round(put_eu,4),put_am=round(put_am,4),
                            u=round(u,4),d=round(d,4),q=round(q,4),N=n)
        }
        
        
        
        # Generate Black-Scholes values
        BS <- function(S,K,t,sigma,rf) {
                sigma <- sigma/100
                rf <- rf/100
                d1 <- (log(S/K) + (rf+0.5*(sigma^2))*t)/(sigma*sqrt(t))
                d2 <- d1 - sigma*sqrt(t)
                bscall <- S*pnorm(d1) - K*exp(-rf*t)*pnorm(d2)
                bsput <- -S*pnorm(-d1) + K*exp(-rf*t)*pnorm(-d2)
                res <- list(bscall=round(bscall,4),bsput=round(bsput,4),d1=round(d1,4),d2=round(d2,4))
        }
        
        
        # Generat Binomial Model converge to BS Model by time
        
        converge<- function(S,K,t,sigma,rf,n){
                sigma <- sigma/100
                rf <- rf/100
                u <- exp(sigma*sqrt(t/n))
                d <- 1/u
                q <- (exp(rf*t/n)-d)/(u-d)
                d1 <- (log(S/K) + (rf+0.5*(sigma^2))*t)/(sigma*sqrt(t))
                d2 <- d1 - sigma*sqrt(t)
                bscall <- S*pnorm(d1) - K*exp(-rf*t)*pnorm(d2)
                bsput <- -S*pnorm(-d1) + K*exp(-rf*t)*pnorm(-d2)
                call_eu <- binomopt(S, K, sigma, rf, t, 0, n, american=F, putopt=F)
                put_eu <- binomopt(S, K, sigma, rf, t, 0, n, american=F, putopt=TRUE)
                res <- data.frame(Call.Price=round(call_eu,4),BS.Call.Price=round(bscall,4),
                                  Put.Price=round(put_eu,4),BS.Put.Price=round(bsput,4))
        }
        
        # implied segma
        
        implied<- function(P,S,K,t,rf){
                P <- input$optionprice2
                S <- input$stockprice2
                K <- input$strike2
                t <- input$maturity2
                rf <- input$riskfreerate2
                
                call_implied <- NA
                call_implied <- bscallimpvol(S,K, rf/100, t, 0, P)
                if(is.numeric(call_implied)){
                        call_implied =call_implied
                }else {call_implied = NA}
                
                put_implied <- NA
                put_implied <- bsputimpvol(S,K, rf/100, t, 0, P)
                if(is.numeric(put_implied)){
                        put_implied = put_implied
                }else {put_implied = NA}
                
                res<- c(call_implied*100,put_implied*100)
        }
        
        #Binomial values
        output$price_binomial <- renderTable({
                #Get inputs
                S <- input$stockprice
                K <- input$strike
                t <- input$maturity
                sigma <- input$volatility
                rf <- input$riskfreerate
                n <- input$n
                res <- Binomial(S,K,t,sigma,rf,n)
                Pricetable_binomial <- data.frame(Call=c(res$call_eu,res$call_am,res$u,res$d,res$q),
                                                  Put=c(res$put_eu,res$put_am,"-","-","-"),
                                                  row.names = c("European", "American",'u','d','q'))
                t(Pricetable_binomial)
        }, rownames = TRUE)
        
        
        #BS option price Table
        output$price_BS <- renderTable({
                #Get inputs
                S <- input$stockprice
                K <- input$strike
                t <- input$maturity
                sigma <- input$volatility
                rf <- input$riskfreerate
                res <- BS(S,K,t,sigma,rf)
                Pricetable_BS <- data.frame(Call=c(res$bscall,res$d1,res$d2),Put=c(res$bsput,"-","-"),row.names = c("Price", "d1","d2"))
                t(Pricetable_BS)
        }, rownames = TRUE)
        
        
        #implied segma Table
        output$price_implied <- renderTable({
                #Get inputs
                P <- input$optionprice
                S <- input$stockprice
                K <- input$strike
                t <- input$maturity
                rf <- input$riskfreerate
                res <- implied(P,S,K,t,rf)
                res <- data.frame(call=c(res[1]),put=c(res[2]),row.names = c("Implied Volatility (%)"))
                t(res)
        }, rownames = TRUE)
        
        
        # Binomial Model converge to BS Model by time
        output$price_time <- renderTable({
                S <- input$stockprice
                K <- input$strike
                t <- input$maturity
                sigma <- input$volatility
                rf <- input$riskfreerate
                n <- c(1,3,5,7,10,100,500,1000)
                res <- data.frame()
                for (i in 1:length(n)) {
                        temp <-converge(S,K,t,sigma,rf,n[i])
                        res <- rbind(res,temp)
                }
                temp <- c("","","","")
                res <- rbind(res,temp)
                colnames(res) <- c("Call Price", "BS Call", "Put Price", "BS Put")
                rownames(res) <- c("1","3","5","7","10","100","500","1000",".")
                t(res)
        }, rownames = TRUE)
        
        # Plot Binomial Model converge to BS Model by time
        
        output$plot_price_converge_call <- renderPlotly({
                S <- input$stockprice
                K <- input$strike
                t <- input$maturity
                sigma <- input$volatility
                rf <- input$riskfreerate
                n <- seq(1,100,1)
                res <- data.frame()
                for (i in 1:length(n)) {
                        temp <-converge(S,K,t,sigma,rf,n[i])
                        res <- rbind(res,temp)
                }
                res <- cbind(n,res)
                colnames(res) <- c("n","Binomial Call", "Black Scholes Call", "Binomial Put", "Black Scholes Put")
                a <- res[,c(1,2,3)]
                colnames(a) <- c("n","BiCall",'BSCall')
                plot_ly(a, x=~n,y=~BiCall, name = "Binomial Call", type = 'scatter', mode = 'lines',line = list(color = 'rgb(22, 96, 167)', width = 2.5))%>%
                        add_trace(x=~n,y=~BSCall, name = "Black Scholes Call", type = 'scatter',line = list(color = 'rgb(205, 12, 24)', width = 2.5, dash = 'dash'))%>%
                        layout(title="Binomial Model converge to Black - Scholes Model by time",
                               xaxis = list(title ="N Period"), yaxis = list(title ="Call Price"),hovermode = 'x')
        })
        
        output$plot_price_converge_put <- renderPlotly({
                S <- input$stockprice
                K <- input$strike
                t <- input$maturity
                sigma <- input$volatility
                rf <- input$riskfreerate
                n <- seq(1,100,1)
                res <- data.frame()
                for (i in 1:length(n)) {
                        temp <-converge(S,K,t,sigma,rf,n[i])
                        res <- rbind(res,temp)
                }
                res <- cbind(n,res)
                colnames(res) <- c("n","Binomial Call", "Black Scholes Call", "Binomial Put", "Black Scholes Put")
                a <- res[,c(1,4,5)]
                colnames(a) <- c("n","BiPut",'BSPut')
                plot_ly(a,x=~n, y=~BiPut, name = "Binomial Put", type = 'scatter', mode = 'lines',line = list(color = 'rgb(22, 96, 167)', width = 2.5))%>%
                        add_trace(x=~n,y=~BSPut, name = "Black Scholes Put", type = 'scatter',line = list(color = 'rgb(205, 12, 24)', width = 2.5, dash = 'dash'))%>%
                        layout(title="Binomial Model converge to Black - Scholes Model by time",
                               xaxis = list(title ="N Period"), yaxis = list(title ="Put Price"),hovermode = 'x')
        })
        
        
        # Plot BS call payoff
        spots <- reactive(seq(0.2*input$stockprice2, 1.8*input$stockprice2, 0.01*input$stockprice2))
        results <- reactive(BS(spots(), input$strike2, input$maturity2, input$volatility2, input$riskfreerate2))
        
        output$callPlot_BS <- renderPlot({
                valueC_BS <- results()$bscall
                payoffC_BS <- pmax(spots() - input$strike2, 0)
                dfC_BS <- data.frame(spots(), valueC_BS, payoffC_BS)
                ggplot(dfC_BS, aes(spots())) +
                        geom_line(aes(y = valueC_BS, color = "Call Value")) +
                        geom_line(aes(y = payoffC_BS, color = "Call Payoff")) +
                        xlab(expression(paste("Underlying Price (", S, ")"))) +
                        ylab("Call Option Payoff and Value") +
                        theme_bw() + theme(legend.title = element_blank(), text = element_text(size=15))
        })
        
        
        
        # Plot BS put payoff
        output$putPlot_BS <- renderPlot({
                valueC_BS <- results()$bsput
                payoffC_BS <- pmax(input$strike2-spots(), 0)
                dfC_BS <- data.frame(spots(), valueC_BS, payoffC_BS)
                ggplot(dfC_BS, aes(spots())) +
                        geom_line(aes(y = valueC_BS, color = "Put Value")) +
                        geom_line(aes(y = payoffC_BS, color = "Put Payoff")) +
                        xlab(expression(paste("Underlying Price (", S, ")"))) +
                        ylab("Put Option Payoff and Value") +
                        theme_bw() + theme(legend.title = element_blank(), text = element_text(size=15))
        })
        
        session$allowReconnect(TRUE) # https://www.jianshu.com/p/24b24274bcc4
        options(shiny.sanitize.errors = FALSE)
}


