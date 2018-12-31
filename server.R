library(shiny)
library(ggplot2)
library(derivmkts)
library(magrittr)

function(input, output, session) {
        
        # # Generate Binomial values
        # Binomial <- function(S,K,t,sigma,rf,n) {
        #         sigma <- sigma/100
        #         rf <- rf/100
        #         call_eu <- CRRBinomialTreeOption(TypeFlag = c("ce"), S, K, t, rf, 0, sigma, n, title = NULL, description = NULL)
        #         call_am <- CRRBinomialTreeOption(TypeFlag = c("ca"), S, K, t, rf, 0, sigma, n, title = NULL, description = NULL)
        #         put_eu <- CRRBinomialTreeOption(TypeFlag = c("pe"), S, K, t, rf, 0, sigma, n, title = NULL, description = NULL)
        #         put_am <- CRRBinomialTreeOption(TypeFlag = c("pa"), S, K, t, rf, 0, sigma, n, title = NULL, description = NULL)
        #         res <- list(call_eu=call_eu@price,call_am=call_am@price,put_eu=put_eu@price,put_am=put_am@price)
        #         # res <- list(call_eu=round(call_eu@price,4),call_am=round(call_am@price,4),put_eu=round(put_eu@price,4),put_am=round(put_am@price,4))
        #         #res <- list(call_eu=round(call_eu@price,4),call_am=round(call_am@price,4),put_eu=round(put_eu@price,4),put_am=round(put_am@price,4))
        # }

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
        
        # Binomial Model converge to BS Model by time
        output$price_time <- renderTable({
                S <- input$stockprice
                K <- input$strike
                t <- input$maturity
                sigma <- input$volatility
                rf <- input$riskfreerate
                n <- c(1,3,5,7,10,100,1000,2000,4000)
                res <- data.frame()
                for (i in 1:length(n)) {
                        temp <-converge(S,K,t,sigma,rf,n[i])
                        res <- rbind(res,temp)
                }
                temp <- c("","","","")
                res <- rbind(res,temp)
                colnames(res) <- c("Call Price", "BS Call", "Put Price", "BS Put")
                rownames(res) <- c("1","3","5","7","10","100","1000","2000","4000",".")
                t(res)
        }, rownames = TRUE)
        
        # Plot Binomial Model converge to BS Model by time
        output$plot_price_time <- renderPlot({
                S <- input$stockprice
                K <- input$strike
                t <- input$maturity
                sigma <- input$volatility
                rf <- input$riskfreerate
                n <- c(1,3,5,7,10,100,1000,2000,4000)
                res <- data.frame()
                for (i in 1:length(n)) {
                        temp <-converge(S,K,t,sigma,rf,n[i])
                        res <- rbind(res,temp)
                }
                temp <- c("","","","")
                res <- rbind(res,temp)
                colnames(res) <- c("Call Price", "BS Call", "Put Price", "BS Put")
                rownames(res) <- c("1","3","5","7","10","100","1000","2000","4000",".")
                res<- t(res)
                
                
        })
        
        
        
        
        # Plot BS call payoff
        spots <- reactive(seq(0.2*input$stockprice, 1.8*input$stockprice, 0.01*input$stockprice))
        results <- reactive(BS(spots(), input$strike, input$maturity, input$volatility, input$riskfreerate))
       
        output$callPlot_BS <- renderPlot({
                valueC_BS <- results()$bscall
                payoffC_BS <- pmax(spots() - input$strike, 0)
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
                payoffC_BS <- pmax(input$strike-spots(), 0)
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


