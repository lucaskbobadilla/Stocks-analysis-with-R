# stocks buy and sell decision


####### packages #########
library(quantmod)
library(xts)
library(rvest)
library(tidyverse)
library(stringr)
library(forcats)
library(lubridate)
library(plotly)
library(dplyr)
library(PerformanceAnalytics)
library(prophet)
library(forecast)
library(tseries)
library(timeSeries)
library(fGarch)
##### get symbols #######

symbols <- c('DIS','V', 'SPY','APHA',"BTC-USD",'MA', 
             'NVDA', "NKE", 'SNE', 'VOO','AAPL','BAC', 'HD',
             'WMT', 'MSFT', 'TSLA','JNJ', 'VT')

for ( symbol in symbols){
  getSymbols(symbol,from="2015-03-01",to="2021-03-22")
}

# correlation

library(PerformanceAnalytics)
data<-cbind(diff(log(Cl(DIS))),diff(log(Cl(V))),diff(log(Cl(SPY))),
            diff(log(Cl(APHA))), diff(log(Cl(NKE))), diff(log(Cl(JNJ))), diff(log(Cl(AAPL))),
            diff(log(Cl(SNE))), diff(log(Cl(AAPL))), diff(log(Cl(HD))), diff(log(Cl(MSFT))))
chart.Correlation(data)

####### get log returns #######
APHA_log_returns <- APHA %>%
  Ad()%>%
  dailyReturn(type='log')

V_log_returns <- V %>%
  Ad()%>%
  dailyReturn(type='log')

AMZN_log_returns <- AMZN %>%
  Ad()%>%
  dailyReturn(type='log')

GOOGL_log_returns <- GOOGL %>%
  Ad()%>%
  dailyReturn(type='log')

AAPL_log_returns <- AAPL %>%
  Ad()%>%
  dailyReturn(type='log')

FB_log_returns <- FB %>%
  Ad()%>%
  dailyReturn(type='log')

TSLA_log_returns <- TSLA %>%
  Ad()%>%
  dailyReturn(type='log')

#### plot price chart ########
`BTC-USD`  %>% 
  Ad() %>%
  chartSeries()
### plot candle chart ####

candleChart(`BTC-USD`, TA=NULL, subset = '2021')
candleChart(`BTC-USD`, TA=c(addMACD(),addVo()), subset = '2021')

######## plot bollinger chart #######
`BTC-USD` %>% 
  chartSeries(TA='addBBands();addVo();addMACD()',subset='2021')

####  build a risk/reward ratio #####
# risk = calculate the std dev from the daily returns
# reward = calculate the mean of the daily returns
# plot risk vs reward

#create data frame

#Mean of log stock returns 
AMZN_mean_log<-mean(AMZN_log_returns)
FB_mean_log<-mean(FB_log_returns)
TSLA_mean_log<-mean(TSLA_log_returns)
AAPL_mean_log<-mean(AAPL_log_returns)
GOOGL_mean_log<-mean(GOOGL_log_returns)

#round it to 4 decimal places
mean_log<-c(AMZN_mean_log,FB_mean_log,TSLA_mean_log,AAPL_mean_log,GOOGL_mean_log)
mean_log<-round(mean_log,4)

#standard deviation of log stock returns
AMZN_sd_log<-sd(AMZN_log_returns)
FB_sd_log<-sd(FB_log_returns)
TSLA_sd_log<-sd(TSLA_log_returns)
AAPL_sd_log<-sd(AAPL_log_returns)
GOOGL_sd_Log<-sd(GOOGL_log_returns)

#round it to 4 decimal places 
sd_log<-c(AMZN_sd_log,FB_sd_log,TSLA_sd_log,AAPL_sd_log,GOOGL_sd_Log)
sd_log<-round(sd_log,4)

# join data
graphic1<-data.frame(rbind(c("AMZN",AMZN_mean_log,AMZN_sd_log),c("FB",FB_mean_log,FB_sd_log),c("TSLA",TSLA_mean_log,TSLA_sd_log),c("AAPL",AAPL_mean_log,AAPL_sd_log),c("GOOGL",GOOGL_mean_log,GOOGL_sd_Log)),stringsAsFactors = FALSE)


graphic1<-data.frame(mean_log,sd_log)
rownames(graphic1)<-c("AMZN","FB","TSLA","AAPL","GOOGL")
colnames(graphic1)<-c("Mean_Log_Return", "Sd_Log_Return")

# Data frame contains the 4 companies with each company's average log return and standard deviation.

xlab<-list(title="Reward")
ylab<-list(title="Risk")

plot_ly(x=graphic1[,1],y=graphic1[,2],text=rownames(graphic1),type='scatter',mode="markers",marker=list(color=c("black","blue","red","grey","green")))%>%layout(title="Risk v Reward",xaxis=xlab,yaxis=ylab)



#### Average stock daily return ######

probs<-c(0.005,0.025,0.25,0.5,0.75,0.975,0.995)

AMZN_dist<-AMZN_log_returns%>%quantile(probs=probs,na.rm=TRUE)
AMZN_mean<-mean(AMZN_log_returns,na.rm=TRUE)
AMZN_sd<-sd(AMZN_log_returns,na.rm=TRUE)

AMZN_mean%>%exp() # 1.001271

FB_dist<-FB_log_returns%>%quantile(probs=probs,na.rm=TRUE)
FB_mean<-mean(FB_log_returns,na.rm=TRUE)
FB_sd<-sd(FB_log_returns,na.rm=TRUE)

FB_mean%>%exp() # 1.000963

TSLA_dist<-TSLA_log_returns%>%quantile(probs=probs,na.rm=TRUE)
TSLA_mean<-mean(TSLA_log_returns,na.rm=TRUE)
TSLA_sd<-sd(TSLA_log_returns,na.rm=TRUE)

TSLA_mean%>%exp() # 1.001244

AAPL_dist<-AAPL_log_returns%>%quantile(probs=probs,na.rm=TRUE)
AAPL_mean<-mean(AAPL_log_returns,na.rm=TRUE)
AAPL_sd<-sd(AAPL_log_returns,na.rm=TRUE)

AAPL_mean%>%exp() # 1.001057

GOOGL_dist<-GOOGL_log_returns%>%quantile(probs=probs,na.rm=TRUE)
GOOGL_mean<-mean(GOOGL_log_returns,na.rm=TRUE)
GOOGL_sd<-sd(GOOGL_log_returns,na.rm=TRUE)

GOOGL_mean%>%exp() # 1.000651


# A popular investing principle is to diversify your investments: 
# do not put all your eggs in one basket. When purchasing stocks you should try to purchase stocks
# that share a small correlation because you want to maximize the total rate of return.

data<-cbind(diff(log(Cl(AMZN))),diff(log(Cl(GOOGL))),diff(log(Cl(AAPL))),diff(log(Cl(FB))))

chart.Correlation(data)

# price prediction
# random walk theory and monte carlo method.
  # The random walk theory is suited for a stock’s price prediction because
  # it is rooted in the believe that past performance is not an indicator 
  # of future results and price fluctuations can not be predicted with accuracy.

# simulation per year - a fiscal stock year have 252 days of trading

mu <- mean(V_log_returns, na.rm = TRUE) # mean of log returns
sig <- sd(V_log_returns, na.rm = TRUE) # sd of log returns
testsim <- rep(NA,1000)
#simulate for the next four years
price<-rep(NA,252)

#most recent price
price[1]<-as.numeric(V$V.Adjusted[length(V$V.Adjusted),])

#start simulating prices

for(i in 2:length(testsim)){
  price[i]<-price[i-1]*exp(rnorm(1,mu,sig))
}

random_data<-cbind(price,1:(252*4))
colnames(random_data)<-c("Price","Day")
random_data<-as.data.frame(random_data)
random_data%>%
  ggplot(aes(Day,Price)) + 
  geom_line() + 
  labs(title="Visa price simulation for 4 years") + 
  theme_bw()


# monte carlo simulation

N<-500
mc_matrix<-matrix(nrow=252*4,ncol=N)
mc_matrix[1,1]<-as.numeric(V$V.Adjusted[length(V$V.Adjusted),])
for(j in 1:ncol(mc_matrix)){
  mc_matrix[1,j]<-as.numeric(V$V.Adjusted[length(V$V.Adjusted),])
  for(i in 2:nrow(mc_matrix)){
    mc_matrix[i,j]<-mc_matrix[i-1,j]*exp(rnorm(1,mu,sig))
  }
}

name<-str_c("Sim ",seq(1,500))
name<-c("Day",name)
final_mat<-cbind(1:(252*4),mc_matrix)
final_mat<-as_tibble(final_mat)
colnames(final_mat)<-name
dim(final_mat) #1008 501
final_mat %>% 
  gather("Simulation","Price",2:501) %>%
  ggplot(aes(x=Day,y=Price,Group=Simulation)) + 
  geom_line(alpha=0.2) + 
  labs(title="Visa Stock (V): 500 Monte Carlo Simulations")+ 
  theme_bw()


probs<-c(0.005,0.025,0.25,0.5,0.75,0.975,0.995)
final_mat[500,-1]%>%as.numeric()%>%quantile(probs=probs)



