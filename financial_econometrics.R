
# TASK 1

data <- read.csv("FIN 36182-Industry_Portfolios.CSV")
head(data)
sum(is.na(data))
summary(data)
anyDuplicated(data)

## 1) Report the arithmetic mean of the returns for each of the five industries over the entire sample
mean_returns <- colMeans(data[, -1])  
mean_returns
