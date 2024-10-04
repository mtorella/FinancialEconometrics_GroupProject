
# TASK 1

data <- read.csv("FIN 36182-Industry_Portfolios.CSV")
head(data)
sum(is.na(data))
summary(data)
anyDuplicated(data)
data$Year <- substr(data$Date, 1, 4)

## 1) Report the arithmetic mean of the returns for each of the five industries over the entire sample
yearly_means <- aggregate(cbind(Cnsmr, Manuf, HiTec, Hlth, Other) ~ Year, data, mean)
yearly_means ## yearly means
mean_returns <- colMeans(data[, c("Cnsmr", "Manuf", "HiTec", "Hlth", "Other")]) ## whole sample mean returns
mean_returns ## mean returns

## 2) Report the standard deviation of the returns for each of the five industries over the entire sample
std_returns <- apply(data[, c("Cnsmr", "Manuf", "HiTec", "Hlth", "Other")], 2, sd) ## whole sample standard deviation of returns
std_returns
yearly_std <- aggregate(cbind(Cnsmr, Manuf, HiTec, Hlth, Other) ~ Year, data, sd)
yearly_std

## 3) Report the Sharpe ratio of each industry
sharpe_ratio <- mean_returns/std_returns ## whole sample Sharpe ratio
sharpe_ratio
yearly_sharpe_ratios <- (yearly_means[, -1]) / yearly_std[, -1] ## yearly Sharpe ratios
yearly_sharpe_ratios <- cbind(Year = yearly_means$Year, yearly_sharpe_ratios)
yearly_sharpe_ratios

### Create a table of results in the whole sample
results_table1 <- data.frame(
  Mean_Returns = mean_returns,
  Std_Returns = std_returns,
  Sharpe_Ratio = sharpe_ratio
 )
results_table1

## 4) Is there evidence that technology stocks have better risk-adjusted returns?
### The Sharpe ratio of HiTec (0.1794) is not the highest; Health (0.1947) and Consumer (0.1919) have slightly better risk-adjusted returns.
### Therefore, technology stocks do not have the best risk-adjusted returns based on the Sharpe ratio. 
### Health and Consumer stocks show better performance in terms of risk-adjusted returns in this sample.

## 5) Provide a table (5×5) with the sample correlation between the returns of the five industries. Comment briefly.
correlation_matrix <- cor(data[, c("Cnsmr", "Manuf", "HiTec", "Hlth", "Other")])
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

## 6) Construct a time series of the simple, non-cumulative returns of a portfolio where capital is allocated equally 
## across the first four industries (excluding Other). Report the arithmetic mean, standard deviation and Sharpe.
## Comment briefly on the gains achieved by this diversified portfolio.
### whole sample portfolio returns
# Escludiamo la colonna "Date" e "Other", prendendo solo le prime quattro industrie
portfolio_data <- data[, c("Cnsmr", "Manuf", "HiTec", "Hlth")]
# Calcolare i rendimenti giornalieri del portafoglio con allocazione equa
portfolio_returns <- rowMeans(portfolio_data)
portfolio_returns
# Calcolare la media aritmetica dei rendimenti del portafoglio
mean_portfolio_return <- mean(portfolio_returns)
mean_portfolio_return
# Calcolare la deviazione standard dei rendimenti del portafoglio
std_portfolio_return <- sd(portfolio_returns)
std_portfolio_return
# Calcolare il rapporto di Sharpe (senza tasso privo di rischio)
sharpe_portfolio <- mean_portfolio_return / std_portfolio_return
sharpe_portfolio
# Creare una tabella dei risultati
results_table2 <- data.frame(
  Mean_Return = mean_portfolio_return,
  Std_Dev = std_portfolio_return,
  Sharpe_Ratio = sharpe_portfolio
)
results_table2

### yearly portfolio returns
data$Portfolio_return <- rowMeans(data[, c("Cnsmr", "Manuf", "HiTec", "Hlth")])
yearly_portfolio_means <- aggregate(Portfolio_return ~ Year, data, mean)
yearly_portfolio_std <- aggregate(Portfolio_return ~ Year, data, sd)
yearly_sharpe_ratios <- yearly_portfolio_means$Portfolio_return / yearly_portfolio_std$Portfolio_return

yearly_results <- data.frame(
  Year = yearly_portfolio_means$Year,
  Mean_Return = yearly_portfolio_means$Portfolio_return,
  Std_Dev = yearly_portfolio_std$Portfolio_return,
  Sharpe_Ratio = yearly_sharpe_ratios
)
yearly_results

### Plot the time series of portfolio performance
plot(yearly_results$Year, yearly_results$Mean_Return, 
     type = 'l', 
     xlab = "Year", 
     ylab = "Values", 
     col  = "blue", 
     ylim = range(c(yearly_results$Mean_Return, yearly_results$Std_Dev, yearly_results$Sharpe_Ratio)),  # Adjust the y-axis limits to accommodate all data
     main = "Time Series of Portfolio Performance")
lines(yearly_results$Year, yearly_results$Std_Dev, 
      col = "red")
lines(yearly_results$Year, yearly_results$Sharpe_Ratio, 
      col = "green")
legend("topright", 
       legend = c("Mean Return", "Std Dev", "Sharpe Ratio"), 
       col = c("blue", "red", "green"), 
       lty = 1)

### Plot returns in the portfolio
plot(data$Year, data$Portfolio_return, 
     type = 'l', 
     xlab = "Year", 
     ylab = "Portfolio Return", 
     col  = "blue", 
     main = "Time Series of Portfolio Returns")








