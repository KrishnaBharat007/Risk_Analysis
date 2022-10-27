#Setup required values----
# install.packages(quantmod)
# install.packages(stringr)
# install.packages(corrplot)
# install.packages(PerformanceAnalytics)
library(quantmod) # get stock prices; useful stock analysis functions
library(stringr) # working with strings
library(corrplot) # for plotting correlation
library(PerformanceAnalytics) # evaluating the performance and  risk  
#   characteristics  of  financial  assets  or  funds
#Loads the company stock using ticker

getSymbols("MSFT",from="2009-10-01",to="2021-10-01",src="yahoo") # Microsoft 
getSymbols("AAPL",from="2009-10-01",to="2021-10-01",src="yahoo") # Apple 
getSymbols("INTC",from="2009-10-01",to="2021-10-01",src="yahoo") #Intel
getSymbols("KO",from="2009-10-01",to="2021-10-01",src="yahoo")  # Coca Cola
getSymbols("WMT",from="2009-10-01",to="2021-10-01",src="yahoo") #Walmart

#Stock returns in log
# Return is log(today_closing_price) - log(prev_day_closing_price) 
MSFT_log_returns<-dailyReturn(MSFT,type='log')
INTC_log_returns<-dailyReturn(INTC,type='log')
APPL_log_returns<-dailyReturn(AAPL,type='log')
KO_log_returns<-dailyReturn(KO,type='log')
WMT_log_returns<-dailyReturn(WMT,type='log')
#Mean of log stock returns 

#Question_1----
MSFT_mean_log<-mean(MSFT_log_returns)
INTC_mean_log<-mean(INTC_log_returns)
APPL_mean_log<-mean(APPL_log_returns)
KO_mean_log<-mean(KO_log_returns)
WMT_mean_log<-mean(WMT_log_returns)
# INTC_mean_log
# APPL_mean_log
# KO_mean_log
# MSFT_mean_log
# WMT_mean_log
#round it to 4 decimal places

mean_log<-c(MSFT_mean_log,APPL_mean_log,KO_mean_log,INTC_mean_log,WMT_mean_log)
mean_log<-round(mean_log,4)
mean_log
#standard deviation of log stock returns

#Question_2----
MSFT_sd_log<-sd(MSFT_log_returns)
INTC_sd_log<-sd(INTC_log_returns)
APPL_sd_log<-sd(APPL_log_returns)
KO_sd_log<-sd(KO_log_returns)
WMT_sd_Log<-sd(WMT_log_returns)
# MSFT_sd_log
# APPL_sd_log
# KO_sd_log
# INTC_sd_log
# WMT_sd_Log
#round it to 4 decimal places 

sd_log<-c(MSFT_sd_log,APPL_sd_log,KO_sd_log,INTC_sd_log,WMT_sd_Log)
sd_log<-round(sd_log,4)
sd_log

#Question_3----
#create data frame

graphic1<-data.frame(mean_log,sd_log)
rownames(graphic1)<-c("MSFT","APPL","KO","INTC","WMT")
colnames(graphic1)<-c("Mean_Log_Return", "Sd_Log_Return")
#View(graphic1)
plot(graphic1,type="p", xlab = 'Mean Log Return', 
     ylab = 'SD Log Return', pch=row.names(graphic1))

#Question_4----
chartSeries(MSFT,bar.type = "ohlc",theme="white",subset="2020",multi.col = T)
addBBands(n=20,sd=2)
chartSeries(AAPL,bar.type = "ohlc",theme="white",subset="2020",multi.col = T)
addBBands(n=20,sd=2)
chartSeries(KO,bar.type = "ohlc",theme="white",subset="2020",multi.col = T)
addBBands(n=20,sd=2)
chartSeries(INTC,bar.type = "ohlc",theme="white",subset="2020",multi.col = T)
addBBands(n=20,sd=2)
chartSeries(WMT,bar.type = "ohlc",theme="white",subset="2020",multi.col = T)
addBBands(n=20,sd=2)

#Question_5----
#check correlation of different companies
data2<-cbind(diff(log(Cl(MSFT))),diff(log(Cl(AAPL))),diff(log(Cl(KO))),
             diff(log(Cl(INTC))),diff(log(Cl(WMT))))
#View(data2)
#library(corrplot)
corrplot(cor(na.omit(data2)),method = 'pie')

#Question_6----
#Monte Carlo: Rooted in past performance is not an indicator of future results. 
#Price fluctuations can not be predicted with accuracy

mu<-MSFT_mean_log
sig<-MSFT_sd_log

#one year 252 trading days, simulate for 4 years 
T<-252*4

#Monte Carlo simulation: incredibly useful forecasting tool to predict outcomes 
#of events with many random variables
set.seed(123456)
N<-300
mc_matrix<-matrix(nrow=T,ncol=N)

for(j in 1:ncol(mc_matrix)){
  #most recent closing price is given by:
  #as.numeric(MSFT[(dim(MSFT))[1],4])
  mc_matrix[1,j]<-as.numeric(MSFT[(dim(MSFT))[1],4])
  for(i in 2:nrow(mc_matrix)){
#generate random daily exponent increase rate using MSFT's mean, sd, log returns
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}

#Plot the matrix for each simulation
matplot(mc_matrix, type = 'l', xlab="Days", ylab="Price")

#max(mc_matrix) = 2949.983
#min(mc_matrix) = 130.3289
#View(mc_matrix)

#Question_7----
# Portfolio variances (AAPL, WMT)
#data12<-cbind(diff(log(Cl(AAPL))), diff(log(Cl(WMT))))
data12 <- cbind(APPL_log_returns, WMT_log_returns)
#View(data2)
#library(corrplot)
cov12 <- cov(na.omit(data12))

# combination 1
w1 <- 0.5 # AAPL
w2 <- 0.5 # WMT
stdev1 <- APPL_sd_log
stdev2 <- WMT_sd_Log
PV_50_50 <- ((w1 **2) * (stdev1 **2) + (w2 **2) * (stdev2 **2) + 2 * w1 * w2 * cov12)[1]
# 0.0002705487      

# combination 2
w1 <- 0.25 # AAPL
w2 <- 0.75 # WMT
PV_25_75 <- ((w1 **2) * (stdev1 **2) + (w2 **2) * (stdev2 **2) + 2 * w1 * w2 * cov12)[1]
# 0.0002158672      

# combination 3
w1 <- 0.75 # AAPL
w2 <- 0.25 # WMT
PV_75_25 <- ((w1 **2) * (stdev1 **2) + (w2 **2) * (stdev2 **2) + 2 * w1 * w2 * cov12)[1]
# 0.0003033575 

# combination 4
w1 <- 0.8 # AAPL
w2 <- 0.2 # WMT
PV_80_20 <- ((w1 **2) * (stdev1 **2) + (w2 **2) * (stdev2 **2) + 2 * w1 * w2 * cov12)[1]
# 0.0003072946 

portfolio_combinations <- c(PV_50_50, PV_25_75, PV_75_25, PV_80_20)
names(portfolio_combinations) <- c('50/50', '25% APPL, 75% WMT', '75% APPL, 25% WMT', '80% APPL, 20% WMT')
portfolio_combinations

Sharpe_Ratio_0802 <- (0.8*APPL_mean_log+0.2*WMT_mean_log)/PV_80_20
Sharpe_Ratio_075025 <- (0.75*APPL_mean_log+0.25*WMT_mean_log)/PV_75_25
Sharpe_Ratio_0505 <- (0.5*APPL_mean_log+0.5*WMT_mean_log)/PV_50_50
Sharpe_Ratio_025075 <- (0.25*APPL_mean_log+0.75*WMT_mean_log)/PV_25_75
PV_100A <-  ((1 **2) * (stdev1 **2) + (0 **2) * (stdev2 **2) + 2 * 1 * 0 * cov12)[1]
Sharpe_Ratio_100A <- (1*APPL_mean_log+0*WMT_mean_log)/PV_100A
Sharpe_Ratios <- (rbind(Sharpe_Ratio_100A, Sharpe_Ratio_0802, Sharpe_Ratio_075025, Sharpe_Ratio_0505, Sharpe_Ratio_025075))
plot(Sharpe_Ratios)
Sharpe_Ratios # Ratios disregarding no-risk returns
max(Sharpe_Ratios)

#Portfolio wise simulation (AAPL + WMT) 80/20:
N<-300
T<-252
mu<-0.8*APPL_mean_log+0.2*WMT_mean_log
sig<-sqrt(PV_80_20)
set.seed(123456)
mc_matrix<-matrix(nrow=T,ncol=N)
for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-10000
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}
matplot(mc_matrix, type = 'l', xlab="Days", ylab="Price")

mean(mc_matrix[252, ]) # mean (our expectation) 12 885
# min value of the portfolio in the end of the year
min(mc_matrix[252, ]) # 5 661
# max value of the portfolio in the end of the year
max(mc_matrix[252, ]) # 23 496

#Stock wise simulation
#Monte Carlo Simulation (10000 times) to predict stock price after 1 year.
#Apple : Closing Price as of 30-09-2021 is 141.5
mu<-APPL_mean_log
sig<-APPL_sd_log
T<-252
set.seed(123456)
N<-10000
appl_initial <- as.numeric(AAPL[(dim(AAPL))[1],4])
mc_matrix<-matrix(nrow=T,ncol=N)
for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-appl_initial
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}
matplot(mc_matrix, type = 'l', xlab="Days", ylab="Price")
max_apple <- max(mc_matrix)
min_apple <- min(mc_matrix)
avg_apple <- mean(mc_matrix)

#Walmart : Closing Price as of 30-09-2021 is 139.38
mu<-WMT_mean_log
sig<-WMT_sd_Log
T<-252
set.seed(123456)
N<-10000
wmt_initial <- as.numeric(WMT[(dim(WMT))[1],4])
mc_matrix<-matrix(nrow=T,ncol=N)
for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-wmt_initial
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}
matplot(mc_matrix, type = 'l', xlab="Days", ylab="Price")
max_wmt <- max(mc_matrix)
min_wmt <- min(mc_matrix)
avg_wmt <- mean(mc_matrix)

#Calculate Profit or loss based on Monte Carlo Simulations.
#Total Portfolio investment is 10000 dollars
portfolio_investment <- 10000

calculate_profit_or_loss_percent <- function(i,f) {
  return(round((f/i),4))
}

#Test_Strategy_for_80_20_Investment
#Made 80-20 percent of investment. i.e., $7500 in Apple and $2500 in Walmart
per_appl <- 0.80
per_wmt <- 0.20
#When both Apple and Walmart gave max stock price at the end of first year, in simulation.
final_portfolio_value_max <- 
  (per_appl*portfolio_investment*calculate_profit_or_loss_percent(appl_initial,max_apple))+
  (per_wmt*portfolio_investment*calculate_profit_or_loss_percent(wmt_initial,max_wmt))
profit <- final_portfolio_value_max - portfolio_investment
profit_percentage <- round((profit/portfolio_investment)*100,2)
print('When both Apple and Walmart has reached max stock price as per Monte Carlo Simulations,')
print(paste('Portfolio Profit in dollars is:',profit))
print(paste('which is ',profit_percentage,' percent profit'))

# When both Apple and Walmart gave min Stock price at the end of first year, in simulation.
final_portfolio_value_min <- 
  (per_appl*portfolio_investment*calculate_profit_or_loss_percent(appl_initial,min_apple))+
  (per_wmt*portfolio_investment*calculate_profit_or_loss_percent(wmt_initial,min_wmt))
loss <- final_portfolio_value_min - portfolio_investment
loss_percentage <- round((loss/portfolio_investment)*100,2)
print('When both Apple and Walmart has reached min stock price as per Monte Carlo Simulations,')
print(paste('Portfolio loss in dollars is:',abs(loss)))
print(paste('which is ',abs(loss_percentage),' percent loss'))

#When both Apple and Walmart gave mean stock price at the end of first year, in simulation.
final_portfolio_value_max <- 
  (per_appl*portfolio_investment*calculate_profit_or_loss_percent(appl_initial,avg_apple))+
  (per_wmt*portfolio_investment*calculate_profit_or_loss_percent(wmt_initial,avg_wmt))
profit <- final_portfolio_value_max - portfolio_investment
profit_percentage <- round((profit/portfolio_investment)*100,2)
print('When both Apple and Walmart has reached max stock price as per Monte Carlo Simulations,')
print(paste('Portfolio Profit in dollars is:',profit))
print(paste('which is ',profit_percentage,' percent profit'))