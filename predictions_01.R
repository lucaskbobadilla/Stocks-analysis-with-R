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
getSymbols("SPY",from="2015-04-01",to="2021-3-21")


# convert data to tibble
stock_df <- data.frame(SPY)
colnames(stock_df) <- c("Open", "High", "Low", "Close", "Volume", "Adjusted")
stock_df <- stock_df %>% 
  rownames_to_column("Date") %>% 
  as_tibble() %>% 
  mutate(Date = as.Date(Date))

# get close values
stock_ts <- stock_df %>% 
  arrange(Date) %>%
  select(Close) %>%
  as.matrix() %>%
  ts()

# check autocorrelation

#plot scatterplots
gglagplot(stock_ts, do.lines = F) + my_theme +
  scale_color_continuous(low = "#b37400", high = "#ffc04d")

#ACF and PACF

ggAcf(stock_ts, lag.max = 200) + my_theme + labs(title = 'ACF' , y = 'Correlation')

ggPacf(stock_ts, lag.max = 200) + my_theme + labs(title = 'PACF', y = '')

# log transform

stock_log_returns <- stock_df %>% 
  mutate(High = log(High),
         Low = log(Low),
         Close = log(Close))

stock_ts_log <-  stock_log_returns %>% 
  arrange(Date) %>%
  select(Close) %>%
  as.matrix() %>%
  ts()


# difference logged data

### difference logged data
stock_diff <- diff(stock_ts_log, lag = 1)

stock_diff <- na.locf(stock_ts_log, na.rm = TRUE,
                     fromLast = TRUE)

plot(stock_ts_log)

# adf test
adf <- adf.test(bx_stock_ts, alternative = c("stationary", "explosive"), 
                k = 0)
adf
