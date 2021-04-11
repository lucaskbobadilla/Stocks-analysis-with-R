### Stocks prediction

####### packages #########
library(quantmod)
library(lubridate)
library(tidyverse)
library(fpp2)
library(astsa)
library(plotly)

## Original plot theme
my_theme = theme(panel.grid = element_line(color = '#e6e6e6'),
                 panel.background = element_rect(fill = 'white'),
                 plot.title = element_text(hjust = .5, size = 28, colour = '#ffa500'),
                 text = element_text(family = 'Georgia'),
                 axis.text = element_text(size = 10),
                 axis.title = element_text(size = 18, family = 'Georgia', face = 'bold'),
                 axis.line = element_line(colour = '#737373', size = 1),
                 strip.background = element_rect(colour = "black", fill = "white"),
                 strip.text = element_text(face = 'bold'))

##### get symbol #######
getSymbols("SPY",from="2020-04-01",to="2021-3-21")

#### plot price chart ########
SPY %>% 
  Ad() %>%
  chartSeries(subset = c('2020', '2021'))

######## plot bollinger chart #######
SPY  %>% 
  chartSeries(TA='addBBands();addVo();addMACD()',subset='2021')

# convert data to tibble
stock_df <- data.frame(SPY)
colnames(stock_df) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
stock_df <- stock_df %>% 
  rownames_to_column("Date") %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))




bit_ts <- stock_df %>% 
  arrange(Date) %>%
  select(Close) %>%
  as.matrix() %>%
  ts()

#plot scatterplots
gglagplot(bit_ts, do.lines = F) + my_theme +
  scale_color_continuous(low = "#b37400", high = "#ffc04d", breaks = c(1, 366, 731), 
                         labels = c('2019','2020', '2021'))

#ACF and PACF

ggAcf(bit_ts, lag.max = 200) + my_theme + labs(title = 'ACF' , y = 'Correlation')
  
ggPacf(bit_ts, lag.max = 200) + my_theme + labs(title = 'PACF', y = '')

# The ACF and PACF suggest this data is taking a Random Walk
  #so a single difference should be applied, and the two plots should be reviewed again.

ggAcf(diff(bit_ts), lag.max = 200) + my_theme + labs(title = 'ACF' , y = 'Correlation') 

ggPacf(diff(bit_ts), lag.max = 200) + my_theme + labs(title = 'PACF', y = '')

# apply a boxcox transformation

BoxCox.lambda(bit_ts)

# prior transformation
ggplotly(stock_df %>%
           ggplot(aes(Date, Close)) + geom_line(col = '#ffa500') + 
           labs(title = 'Stock - prior transformation', x = '') + my_theme)

# transformed 
ggplotly(stock_df %>%
           mutate(Price = BoxCox(stock_df$Close, lambda = BoxCox.lambda(stock_df$Close))) %>%
           ggplot(aes(Date, Price)) + geom_line(col = '#ffa500') + 
           labs(title = 'Stock - Transformed', x = '')  + my_theme)

## Original Price
stock_df[-1,] %>%
  mutate(Price = diff(stock_df$Close)) %>%
  ggplot(aes(Date, Price)) + geom_line(col = '#ffa500') + my_theme + 
  labs(x = '', title = 'Original Price', y = 'Difference')

## Transformed Price
stock_df[-1,] %>%
  mutate(Price = diff(BoxCox(stock_df$Close, lambda = BoxCox.lambda(stock_df$Close)))) %>%
  ggplot(aes(Date, Price)) + geom_line(col = '#ffa500') + my_theme + 
  labs(x = '', title = 'Transformed Price', y = '')

bit_ts_tran = BoxCox(bit_ts, lambda = BoxCox.lambda(bit_ts))

ggAcf(diff(bit_ts_tran), lag.max = 200) + my_theme + labs(title = 'ACF' , y = 'Correlation') 

ggPacf(diff(bit_ts_tran), lag.max = 200) + my_theme + labs(title = 'PACF', y = '')

# fit model

#untransformed
auto.arima(bit_ts) 
checkresiduals(auto.arima(bit_ts))

#transformed
auto.arima(bit_ts_tran) 
checkresiduals(auto.arima(bit_ts_tran))

# summary model

summary(Arima(bit_ts_tran, order = c(2,2,3)))

# standard deviation

err = residuals(Arima(bit_ts_tran, order = c(2,2,3)))
cat('Standard Deviation = ', sd(err, na.rm = T))

cat('Mean =', mean(err, na.rm = T))

# invert boxcox
invers_BoxCox = function(ts_data, lambda){
  original_ts = (ts_data * lambda + 1) ** (1/lambda)
  return(original_ts)
}

invers_BoxCox(sd(err, na.rm = T), BoxCox.lambda(bit_ts))


# create function for prediction

fit_model = function(bitcoin_data, h){
  bitcoin_df = bitcoin_data 
  
  time_series = bitcoin_df %>%
    select(Close) %>%
    ts()
  
  predictions = time_series %>%
    BoxCox(lambda = BoxCox.lambda(time_series)) %>% 
    auto.arima() %>%
    forecast(h)
  
  forecast_df = cbind(data.frame(predictions[4]), 
                      data.frame(predictions[5]), 
                      data.frame(predictions[6]))
  
  the_forecast = invers_BoxCox(forecast_df, lambda = BoxCox.lambda(time_series))
  
  the_forecast = the_forecast %>%
    mutate(Date = tail(bitcoin_df$Date, h) + h) %>%
    as_tibble()
  
  return(the_forecast)
}

#prediction for the next 100 days
fit_model(stock_df, 100) %>%
           ggplot(aes(x = Date, y = mean)) + geom_line(col = '#ff2500') +
           geom_ribbon(aes(ymin = lower.80., ymax = upper.80.), alpha = .3, fill = '#ffc04c') +
           geom_ribbon(aes(ymin = lower.95., ymax = upper.95.), alpha = .3, fill = '#ffe4b2') +
           geom_line(data = stock_df, aes(Date, Close)) +
           geom_line(data = filter(stock_df, Date >= as.Date('2015-01-01')), aes(Date, Close), col = '#ffa500') +
           my_theme +
           labs(title = 'Prediction of 100 Days', y = 'Price', x = '')

