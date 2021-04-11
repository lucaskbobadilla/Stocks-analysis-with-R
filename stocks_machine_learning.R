## Machine learning stock prediction

#get symbol
getSymbols("V",from="2015-03-01",to="2021-03-22")

head(V)

stock <- data.frame(V) %>% 
  rownames_to_column("Date")

as_tibble(stock)
col_names <- c("Date", "Open", "High", "Low", "Close", "Volume", "Adjusted")

colnames(stock) <- col_names
stock <- as_tibble(stock)

data <- stock %>% 
  select(Open:Volume)

#shifting n rows up of a given variable
shift <- function(x, n) {
  c(x[-(seq(n))], rep(NA, n))
}
data$shifted <- shift(data$Close, 1)
tail(data)

#remove NA observations
data <- na.omit(data)
write.csv(data, "data.csv")

#Installing the package
install.packages("h2o")
#loading the library 
library(h2o)

#Initializing the Virtual Machine using all the threads (-1) and 16gb of memory
h2o.init(nthreads = -1, max_mem_size = "16g")

h2o.importFile("data.csv")
h2o.describe(data)

