### Stocks prediction ####
## Lucas Kopecky Bobadilla

#### Packages ####

#data cleanup and visualization 
library(tidyverse)
library(data.table)
library(plotly)
library(DT)
library(lubridate)
#API and json
library(httr)
library(jsonlite)
library(config)
library(RcppSimdJson)
#Web Scraping
library(rvest)

#Data
library(bea.R)
library(devtools)
library(gtrendsR)

#Text Analysis
library(tidytext)
library(wordcloud)
library(RColorBrewer)

#Forecasting
library(quantmod)
library(forecast)
library(tseries)
library(fpp2)
library(astsa)

### Choose a symbol ####

symbol <- 'TSLA'

# get data of symbol ####
getSymbols(symbol,from="2015-04-01")
stock <- TSLA
##### plot trends ########



#### plot price chart 
stock  %>% 
  Ad() %>%
  chartSeries(subset = '2021')
######## plot bollinger chart
stock %>% 
  chartSeries(TA='addBBands();addVo();addMACD()',subset='2021')

close_price <- stock[,4]
### tidy data
stock_df <- data.frame(TSLA)
colnames(stock_df) <- c('open', 'high', 'low', 'close', 'volume','adjusted')
stock_df <- stock_df %>% 
  rownames_to_column("date") %>% 
  as_tibble() %>% 
  mutate(date = as_date(date))


## google trends ####

trends <- gtrends(keyword = symbol, geo = "US", onlyInterest = TRUE)
trends <- trends$interest_over_time %>%
  as_tibble() %>%
  select(c(date, hits, keyword))
trends$date <- as_date(ceiling_date(trends$date, unit = "weeks", change_on_boundary = NULL,
                                    week_start = getOption("lubridate.week.start", 1)))
trends %>%  
  plot_ly(x=~date, y=~hits, mode = 'lines', name = "Google Search Trends") %>%
  layout(title = paste0("Interest over Time: ",symbol), yaxis = list(title = "hits"))

# interest vs price

trends %>%
  left_join(stock_df, by = "date") %>%
  select(one_of(c("date", "hits", "close"))) %>%
  drop_na() %>%
  ggplot(aes(hits, close)) + geom_point(color="blue") + geom_smooth(model=lm, color = "black") +
  labs(title =paste0(symbol,": Relationship between Hits and Close Stock Price"))

# get news info ####
config <- config::get()
##get company name using web-scraping
url_overview = paste0("https://www.marketwatch.com/investing/stock/",symbol,"/profile")
var_overview = read_html(url_overview)
company <-  var_overview %>% 
  html_nodes('div h1') %>% # need to go and find it on the page
  html_text() %>%
  as.character()

#news API Query
google_api = "" # place key here
url_news = paste0("https://newsapi.org/v2/everything?q=",
                  str_replace_all(company,pattern = " Inc.", replacement = ""),
                  "&from=",today()-ddays(27), #last 30 days
                  "&sortBy=relevance&pageSize=100&language=en&apiKey=",google_api,config$news_apikey)

#API json to datatable
results <- GET(url = url_news)
news <- content(results, "text")
news %<>%
  fromJSON(flatten = TRUE) %>% #flatten
  as.data.frame() %>% #make dataframe
  select(c(articles.title, articles.description, articles.content, articles.publishedAt))
datatable(news)

# word cloud ####

news_words <- news %>%
  select(c("articles.title","articles.description", "articles.content", "articles.publishedAt")) %>%
  unnest_tokens(word, articles.description) %>%
  filter(!word %in% append(stop_words$word, values = "chars"), str_detect(word, "^[a-z']+$"))
news_words$date = as_date(news_words$articles.publishedAt)

words_only <- news_words %>%
  count(word, sort =TRUE)

set.seed(1)
wordcloud(words = words_only$word, freq = words_only$n, scale=c(5,.5), max.words=50, colors=brewer.pal(8, "Dark2"))

# sentiment analysis ####

afinn <- get_sentiments("afinn")

sentiment_summary <- news_words %>%
  left_join(afinn) %>%
  filter(!is.na(value)) %>%
  group_by(articles.title, date) %>%
  summarise(score = mean(value)) %>%
  mutate(sentiment = ifelse(score>0, "positive","negative")) 

datatable(sentiment_summary)

# plot sentiment analysis

ggplot(sentiment_summary, aes(date, score)) + geom_bar(stat = "identity", aes(fill=sentiment))  + 
  ggtitle(paste0(symbol, ": News Sentiment Over Time")) 

### ARIMA stocks prediction ####
close_price <- stock[,4]
# Conduct ADF test for dataset
print(adf.test(close_price))

#ACF and PACF

ggAcf(close_price, lag.max = 200) + labs(title = 'ACF' , y = 'Correlation')

ggPacf(close_price, lag.max = 200) + labs(title = 'PACF', y = '')

# transform plot
## Transformed Price
stock_df[-1,] %>%
  mutate(Price = diff(BoxCox(stock_df$close, lambda = BoxCox.lambda(stock_df$close)))) %>%
  ggplot(aes(date, Price)) + geom_line(col = '#ffa500') + 
  labs(x = '', title = 'Transformed Price', y = '')


#We apply auto arima to the dataset 
modelfit <- auto.arima(close_price, lambda = "auto")
checkresiduals(auto.arima(close_price, lambda = "auto"))


#Dataset forecasting for the next 30 days
h <- 30
price_forecast <- forecast(modelfit, h=h)
plot(price_forecast)

# nice plot
forecast_df = cbind(data.frame(price_forecast[4]), 
                    data.frame(price_forecast[5]), 
                    data.frame(price_forecast[6]))
forecast_df %>%  
  as_tibble() %>% 
  mutate(date = tail(stock_df$date, h) + h) %>%
  ggplot(aes(x = date, y = mean)) + geom_line(col = '#ff2500') +
  geom_ribbon(aes(ymin = lower.80., ymax = upper.80.), alpha = .3, fill = '#ffc04c') +
  geom_ribbon(aes(ymin = lower.95., ymax = upper.95.), alpha = .3, fill = '#ffe4b2') +
  geom_line(data = stock_df, aes(date, close)) +
  geom_line(data = filter(stock_df, date >= as.Date('2015-01-01')), aes(date, close), col = 'black') +
  labs(title = paste0('Prediction of ',h,' Days'), y = 'Price', x = '') +
  theme_light()
