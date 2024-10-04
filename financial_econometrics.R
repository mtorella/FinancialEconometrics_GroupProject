
# TASK 1

data <- read.csv("FIN 36182-Industry_Portfolios.CSV")
head(data)
sum(is.na(data))
summary(data)
anyDuplicated(data)

## 1) Report the arithmetic mean of the returns for each of the five industries over the entire sample
mean_returns <- colMeans(data[, -1])  
mean_returns

## 2) Report the standard deviation of the returns for each of the five industries over the entire sample
std_returns <- apply(data[, -1], 2, sd)
std_returns

## 3) Report the Sharpe ratio of each industry
sharpe_ratio <- mean_returns/std_returns
sharpe_ratio

results_table <- data.frame(
  Mean_Returns = mean_returns,
  Std_Returns = std_returns,
  Sharpe_Ratio = sharpe_ratio
 )
results_table

## 4) Is there evidence that technology stocks have better risk-adjusted returns?
### The Sharpe ratio of HiTec (0.1794) is not the highest; Health (0.1947) and Consumer (0.1919) have slightly better risk-adjusted returns.
### Therefore, technology stocks do not have the best risk-adjusted returns based on the Sharpe ratio. 
### Health and Consumer stocks show better performance in terms of risk-adjusted returns in this sample.

## 5) Provide a table (5×5) with the sample correlation between the returns of the five industries. Comment briefly.
correlation_matrix <- cor(data[, -1])
correlation_matrix
### The correlation matrix shows how returns from one industry are related to returns from another:
### High Correlations: Many pairs of industries show strong positive correlations (close to 1). For example:
#### Consumer (Cnsmer) and Manufacturing (Manuf): 0.867; Manufacturing and Other: 0.892 --> These high correlations suggest that 
#### returns in these industries tend to move in the same direction. This may indicate that they are influenced 
#### by similar economic factors or market conditions.
### Moderate Correlations: Health (Hlth) generally has lower correlations with other industries, such as with Technology (HiTec) 
#### at 0.707 and Manufacturing at 0.743. This indicates that while Health is still positively correlated with the others, it may 
#### react differently to market conditions compared to sectors like Manufacturing or Consumer.
### Diversification Insights: High correlations (above 0.7) indicate that these industries may not provide significant 
#### diversification benefits. For example, investing in Consumer and Manufacturing stocks may not reduce portfolio 
#### risk much since their returns are highly correlated (0.867).On the other hand, the relatively lower correlation 
#### of Health with other industries (0.707 to 0.773) suggests it might offer slightly better diversification benefits 
#### compared to, say, the Consumer-Manufacturing pair.
### The industries in this dataset are highly correlated, with most correlations above 0.7. This indicates that their returns 
#### move in similar directions, likely influenced by common market factors.The highest correlation is between Manufacturing 
#### and Other industries (0.892), suggesting very little diversification potential between these two. The Health sector appears 
#### to offer slightly better diversification, with lower correlations with other industries, such as with Technology (0.707) and 
#### Manufacturing (0.743), but overall the correlations are still fairly high.














