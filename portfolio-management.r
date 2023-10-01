library(quantmod)
library(funModeling)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(fBasics)
library(reshape)
library(PerformanceAnalytics)
library(IntroCompFinR)
library(xts)
library(timetk)
library(writexl)
library(readxl)
library(fPortfolio)
library(timeSeries)

data <- read_excel("D:/download/Portfolio Management - ky1nam4/DataVCBFBCF-PORTFOLIO-DATA.xlsx", 
                                      sheet = "NAVPS")
VCBFVN100 <- data[,c(1:3)]
VCBFVN100$Date <- as.Date(VCBFVN100$Date, format = "%d/%m/%Y")
# Order the dataframe by the "Date" column in descending order
VCBFVN100 <- VCBFVN100 %>% arrange(desc(Date))

# Remove duplicate rows based on the "Date" and "VN100" columns, keeping the first occurrence
VCBFVN100 <- VCBFVN100 %>% distinct(Date, .keep_all = TRUE)
# Reorder the dataframe by the "Date" column in ascending order
VCBFVN100 <- VCBFVN100 %>% arrange(Date)
View(VCBFVN100)
structure(VCBFVN100)

Port <- read_excel("D:/download/Portfolio Management - ky1nam4/DataVCBFBCF-PORTFOLIO-DATA.xlsx", 
                                     sheet = "PORTFOLIO")
Port$Date <- as.Date(Port$Date, format = "%d/%m/%Y")
structure(Port)
# Options de hien thi dau phay thap phan thay vi 10e5
options(scipen=100)
options(digits=2)

# Tao bien dang xts tuong ung voi tung ma co phieu
stocks <- colnames(Port)[2:ncol(Port)]
for (stock in stocks) {
  assign(stock, xts(Port[[stock]], order.by=Port$Date))
}

# Tao bien return cho tung ma co phieu
for (stock in stocks) {
  stock_xts <-  get(stock)
  returns <-  (diff(log(stock_xts)))[-1,] 
  assign(paste0(stock, "return"), returns)
}

# Hop lai thanh mot dataframe chua return
returnport <- get(paste0(stocks[1], "return"))
for(stock in stocks[-1]) {
  return_xts <- get(paste0(stock, "return"))
  returnport <- merge(returnport, return_xts)
}
colnames(returnport) <- colnames(Port[-1]) # Doi ten cot lai cho dep

###################################### Lam tuong tu voi VCBF-BCF va VN100
stocks <- colnames(VCBFVN100)[2:ncol(VCBFVN100)]
for (stock in stocks) {
  assign(stock, xts(VCBFVN100[[stock]], order.by=VCBFVN100$Date))
}

# Tao bien return cho tung ma co phieu
for (stock in stocks) {
  stock_xts <-  get(stock)
  returns <- (diff(log(stock_xts)))[-1,] 
  assign(paste0(stock, "return"), returns)
}

# Hop BCF va VN100 thanh mot dataframe
returnbcfvn100 <- get(paste0(stocks[1], "return"))
for(stock in stocks[-1]) {
  return_xts <-  get(paste0(stock, "return"))
  returnbcfvn100 <- merge(returnbcfvn100, return_xts)
}
colnames(returnbcfvn100) <- colnames(VCBFVN100[-1]) # Doi ten cot lai cho dep


###################################################################
###################################################################
# Ve do thi cumulative sum
# Dataframe cua return port va return bcfvn100
returnportdf <- data.frame(date=index(returnport), coredata(returnport))
returnbcfvn100df <- data.frame(date=index(returnbcfvn100), coredata(returnbcfvn100))
# VE CUMSUM cua PORT va BCFVN100
cumsumportdf <- cumsum(returnportdf[-1])
cumsumportdf$date <- returnportdf$date
melted_cumsumportdf <- melt(cumsumportdf, id="date")
ggplot(data = melted_cumsumportdf , aes(x = date, y = value, color = variable)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Cumulative Sum of Return ",
       x = "Date",
       y = "Return",
       color = "Stock Symbol") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        title = element_text(size=25, face='bold'),
        legend.text = element_text(size=15),
        legend.position = "bottom", legend.key.size=unit(1.5, "cm"))

cumsumbcfvn100df <- cumsum(returnbcfvn100df[-1])
cumsumbcfvn100df$date <- returnbcfvn100df$date
melted_cumsumbcfvn100df <- melt(cumsumbcfvn100df, id="date")
ggplot(data = melted_cumsumbcfvn100df , aes(x = date, y = value, color = variable)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Cumulative Sum of Return ",
       x = "Date",
       y = "Return",
       color = "Stock Symbol") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        title = element_text(size=25, face='bold'),
        legend.text = element_text(size=15),
        legend.position = "bottom", legend.key.size=unit(1.5, "cm"))


## Correlation heatmap of stocks' adjusted price
corr_mat <- round(cor(returnportdf[-1]),2) 
melted_corr_mat <- melt(corr_mat, as.is=TRUE)
ggplot(data = melted_corr_mat, aes(x=X1, y=X2, fill=value)) + 
  geom_tile() +
  geom_text(aes(X1, X2, label = value), size = 5) +
  scale_fill_gradient2(low = "#d30efb", high = "#2999eb",
                       limit = c(-1,1), name="Correlation") +
  labs(tilte = "Correlation Map") +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_blank(),
        title = element_text(size=15, face='bold'))



###################################################################
###################################################################
# PORTFOLIO CONSTRUCTION

## Equally Weighted Portfolio
ewSpec <- portfolioSpec()
nAssets <- ncol(final_data)
setWeights(ewSpec) <- rep(1/nAssets, times = nAssets)

ewPortfolio <- feasiblePortfolio(
  data = final_data,
  spec = ewSpec,
  constraints = "LongOnly")

print(ewPortfolio)

### Visualize EWP
col <- seqPalette(ncol(final_data), "RdPu")
weightsPie(ewPortfolio, box = FALSE, col = col)
mtext(text = "EWP Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)

weightedReturnsPie(ewPortfolio, box = FALSE, col = col)
mtext(text = "EWP Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)

covRiskBudgetsPie(ewPortfolio, box = FALSE, col = col)
mtext(text = "EWP Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)

weights <- 100 * as.vector(getWeights(ewPortfolio))
weightedReturns <- weights * getMean(ewPortfolio)
covRiskBudgets <- getCovRiskBudgets(ewPortfolio)
names <- colnames(final_data)

barplot(height = weightedReturns, names.arg = names, horiz = TRUE, las = 1, col = col, )
title(main = "EWP Portfolio Weighted Returns", xlab = "Weighted Returns %")


# Calculate the global minimum variance portfolio
ret.mat <- as.matrix(returnport)
ret.mat
final_data <- as.timeSeries(100*ret.mat)
View(final_data)
minVarSpec <- portfolioSpec()
minVarPortfolio <- minvariancePortfolio(
  data = final_data,
  spec = minVarSpec,
  constraints = "LongOnly")
print(minVarPortfolio)


# weight,return, risk pie chart
par(mfrow=c(1,1))
col <- seqPalette(ncol(final_data), "RdPu")
weightsPie(minVarPortfolio, box = FALSE, col = col)
mtext(text = "Minimum Variance MV Portfolio", side = 3,
      line = 1.5, font = 2, cex = 0.7, adj = 0)

weights <- 100 * as.vector(getWeights(minVarPortfolio))
weightedReturns <- weights * getMean(minVarPortfolio)
covRiskBudgets <- getCovRiskBudgets(minVarPortfolio)
names <- colnames(final_data)

barplot(height = weightedReturns, names.arg = names, horiz = TRUE, las = 1, col = col, )
title(main = "GMV Portfolio Weighted Returns", xlab = "Weighted Returns %")

covRiskBudgetsPie(minVarPortfolio, box = FALSE, col = col)
mtext(text = "Global Minimum Variance MV Portfolio", side = 3,
      line = 1.5, font = 2, cex = 0.7, adj = 0)

# tangency portfolio
tgSpec <- portfolioSpec()
setRiskFreeRate(tgSpec) <- 0.02741/252

tgPortfolio <- tangencyPortfolio(
  data = final_data,
  spec = tgSpec,
  constraints = "LongOnly")
print(tgPortfolio)

r.free <- getRiskFreeRate(tgPortfolio)
tangency.port.ret <- getPortfolio(tgPortfolio)$targetReturn["mean"]
risk <- getPortfolio(tgPortfolio)$targetRisk["Cov"]
sharpe.ratio <- (tangency.port.ret-r.free)/risk
sharpe.ratio

# weight,return, risk pie chart of tangency
col <- seqPalette(ncol(final_data), "RdPu")
weightsPie(tgPortfolio, box = FALSE, col = col)
mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)

weights <- 100 * as.vector(getWeights(tgPortfolio))
weightedReturns <- weights * getMean(tgPortfolio)
covRiskBudgets <- getCovRiskBudgets(tgPortfolio)
names <- colnames(final_data)

barplot(height = weightedReturns, names.arg = names, horiz = TRUE, las = 1, col = col, )
title(main = "Tangent Portfolio Weighted Returns", xlab = "Weighted Returns %")

covRiskBudgetsPie(tgPortfolio, box = FALSE, col = col)
mtext(text = "Tangency MV Portfolio", side = 3, line = 1.5,
      font = 2, cex = 0.7, adj = 0)
print(ewPortfolio)

###
# efficient frontier
frontierSpec <- portfolioSpec()

setNFrontierPoints(frontierSpec) <- 200
setRiskFreeRate(frontierSpec) <- 0.02741/252
longFrontier <- portfolioFrontier(final_data, frontierSpec)
tailoredFrontierPlot(object = longFrontier,
                     risk = "Sigma",
                     mText = "MV Portfolio - LongOnly Constraints",
                     twoAssets = FALSE,
                     sharpeRatio = T,
                     xlab = "sadas")


###################################################################
# PERFORMANCE ANALYSIS
tangency_weights <- as.numeric(getPortfolio(tgPortfolio)$weights)
returns.matrix <- returnport %*% as.vector(tangency_weights)
returns.portfolio <- xts(returns.matrix, order.by=index(returnport))
View(returns.portfolio)

plot(returns.portfolio); title(main="Portfolio Daily Return (%)", sub="")
port_grow <- Return.portfolio(returnport, weights=tangency_weights,
                              wealth.index = TRUE)

View(port_grow)
plot(port_grow); title(main="Growth of 1 USD Invested in Portfolio")


BCF_grow <- Return.portfolio(BCFreturn, weights = NULL, wealth.index = TRUE)
VN100_grow <- Return.portfolio(VN100return, weights = NULL, wealth.index = TRUE)
benchmark <- merge(BCF_grow, VN100_grow, port_grow)
colnames(benchmark) <- c("BCF", "VN100", "Portfolio")
benchmark <- na.omit(benchmark)
View(benchmark)
View(benchmarkdf)
benchmarkdf <- data.frame(benchmark)
benchmarkdf$date <- index(benchmark)
meltedbenchmarkdf <- melt(benchmarkdf, id="date")
ggplot(meltedbenchmarkdf, aes(x=date,y=value,colour=variable,group=variable)) + geom_line() + 
  ylab("Amount ($)") + xlab("Date") + 
  ggtitle("Comparision Growth between 1 USD Invested in Portfolio vs. BCF vs. VN100")+
  theme_minimal() +
  theme(axis.text.x=element_text(size=20),axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        title = element_text(size=20, face='bold'),
        legend.text = element_text(size=20))

# Cumsum

return_portbcfvn100 <- merge(returnbcfvn100, returns.portfolio)
colnames(return_portbcfvn100) <- c("BCF", "VN100", "Portfolio")
return_portbcfvn100 <- na.omit(return_portbcfvn100)
returncumsum_portbcfvn100 <- cumsum(return_portbcfvn100)

returncumsum_portbcfvn100df <- data.frame(returncumsum_portbcfvn100)
returncumsum_portbcfvn100df$date <- index(returncumsum_portbcfvn100)
melted_returncumsum_portbcfvn100df <- melt(returncumsum_portbcfvn100df, id="date")
ggplot(data = melted_returncumsum_portbcfvn100df, aes(x = date, y = value, color = variable)) +
  geom_line() +
  theme_minimal() +
  labs(title = "Cumulative Sum of Return ",
       x = "Date",
       y = "Return",
       color = "Stock Symbol") +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x=element_text(size=10),
        axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        title = element_text(size=25, face='bold'),
        legend.text = element_text(size=15),
        legend.position = "bottom", legend.key.size=unit(1.5, "cm"))

# Key Performance Indicators Calculations
excess_BCF_returns <- BCFreturn - r.free
excess_VN100_returns <- VN100return - r.free
excess_portfolio_returns <- returns.portfolio - r.free

temp_df <- na.omit(merge(excess_BCF_returns, excess_VN100_returns, excess_portfolio_returns))
excess_portfolio <- temp_df$excess_portfolio_returns
excess_BCF <- temp_df$excess_BCF_returns
excess_VN100 <- temp_df$excess_VN100_returns

# Calculate Beta
beta_portfolio <- cov(excess_portfolio, excess_VN100) / var(excess_VN100)
beta_BCF <- cov(excess_BCF, excess_VN100) / var(excess_VN100)

# Calculate Alpha
alpha_portfolio <- mean(excess_portfolio) - beta_portfolio * mean(excess_VN100)
alpha_BCF <- mean(excess_BCF) - beta_BCF * mean(excess_VN100)

# Treynor Ratio
treynor_port <- mean(excess_portfolio) / beta_portfolio
treynor_BCF <- mean(excess_BCF) / beta_BCF
treynor_VN100 <- mean(excess_VN100) / cov(excess_BCF, excess_VN100) * var(excess_VN100)


# Maximum Drawdown
portfolio_cumulative_returns <- cumprod(1 + return_portbcfvn100$Portfolio)
drawdowns <- 1 - portfolio_cumulative_returns / cummax(portfolio_cumulative_returns)
max_drawdown_portfolio <- max(drawdowns)

BCF_cumulative_returns <- cumprod(1 + return_portbcfvn100$BCF)
drawdowns <- 1 - BCF_cumulative_returns / cummax(BCF_cumulative_returns)
max_drawdown_BCF <- max(drawdowns)

VN100_cumulative_returns <- cumprod(1 + return_portbcfvn100$VN100)
drawdowns <- 1 - VN100_cumulative_returns / cummax(VN100_cumulative_returns)
max_drawdown_VN100<- max(drawdowns)

max_drawdown_portfolio
max_drawdown_BCF
max_drawdown_VN100


# Visualize the Maxixum Drawdown
drawdowns1 <- Drawdowns(return_portbcfvn100$Portfolio)
drawdowns2 <- Drawdowns(return_portbcfvn100$BCF)
drawdowns3 <- Drawdowns(return_portbcfvn100$VN100)
drawdowns <- merge(drawdowns1, drawdowns2, drawdowns3) 
index(drawdowns) <- as.Date(index(drawdowns))
drawdowns_df <- data.frame(Date = index(drawdowns), Drawdown = coredata(drawdowns))
colnames(drawdowns_df) <- c('Date', 'Portfolio', 'BCF', 'VN100')
melted_drawdowns_df <- na.omit(melt(drawdowns_df, id='Date'))
y_axis_limits <- c(-1,0)
ggplot(melted_drawdowns_df, aes(x = Date, y = value, color = variable)) +
  geom_line() +
  labs(title = "Maximum Drawdown Chart", x = "Date", y = "Drawdown") +
  coord_cartesian(ylim = y_axis_limits) +
  theme_minimal() +
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
        axis.text.x=element_text(size=20),axis.text.y=element_text(size=20),
        axis.title.x=element_text(size=20),axis.title.y=element_text(size=20),
        title = element_text(size=25, face='bold'),
        legend.text = element_text(size=20))

# Report
beta_BCF
beta_portfolio
alpha_BCF
alpha_portfolio
treynor_port 
treynor_BCF
treynor_VN100
mean(excess_VN100)