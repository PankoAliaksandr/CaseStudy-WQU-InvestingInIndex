#Case Study Task
# 1) Download data for last 2 years for the NASDAQ Composite
# 2) Calculate Daily returns of NASDAQ composite for the last 1 year
# 3) Graphically represent the stock prices as a line plot
# 4) Are any obvious trends visible in movement of NASDAQ prices for the period under study?
# 5) Bucket the daily return values into bins and plot a histogram
# 5.1) Calculate mean, median and standard deviation of
#    Daily return values and plot them on the same graph mentioned in step IV
# 6) How is the daily returns of NASDAQ distributed? Does it follow a normal distribution?

# 7) Analyze the measures of central tendency calculated and offer opinion on the overall risks and
#    possible rewards associated with investing in the NASDAQ index for the period under study.

library(quantmod)
library(xts)
library(PerformanceAnalytics)

# Step 1. Create Data Frame and download the data

getSymbols('^IXIC', from = "2015-04-22", to = "2017-04-21", src='yahoo')

#------------------------------------------------------------------------------
# Step 2.  Calculate Daily returns of NASDAQ composite for the last 1 year
#------------------------------------------------------------------------------

# Create a new 1 year Data Frame
last_year_DF <- window(IXIC, start = "2016-04-22", to = "2017-04-21")

#Calculate returns
return = Return.calculate(last_year_DF$IXIC.Adjusted, method = "discrete") 

#------------------------------------------------------------------------------
# Step 3. Index line plot
#------------------------------------------------------------------------------

plot(x = last_year_DF$IXIC.Adjusted, main = "Nasdaq index", xlab = "date", ylab = "index value")

#------------------------------------------------------------------------------
# Step 4. Add trend line 
#------------------------------------------------------------------------------

x <- 1:252 # 1 year contains 252 trading days
y <- as.vector(last_year_DF$IXIC.Adjusted)

mydf <- data.frame(x = x, y = y)
plot(y ~ x, data = mydf,pch = 19, cex = .5)
model <- lm(y ~ x, data = mydf)
abline(model, col = "red", lwd = 3)
summary(model)

# Conclusion: there is obvious upwards trend

#------------------------------------------------------------------------------
# Step 5. Histogram with mean, median and st. deviation 
#------------------------------------------------------------------------------

# Create Intervals
plot(return, main = "Nasdaq index daily returns", xlab = "date", ylab = "daily return")

#Based on the plot it is good idea to use 0.01 intervals
breaks = seq(-0.05,0.03,by = 0.01 )

# Mean of daily return
mean(return, na.rm = TRUE)

# Median of daily return
median(return, na.rm = TRUE)

# Standard deviation of daily return
StdDev(return, na.rm = TRUE)

# Plot a histogram (daily returns, mean, median, st. dev)
colors <- c("red","green","blue","yellow","cyan")
hist(x = return, col = colors, breaks = breaks,
     main = "Nasdaq index daily returns", xlab = "date", ylab = "daily return")

#Add mean
abline(v = mean(return, na.rm = TRUE),
      col = "black",
      lwd = 3)

#Add median
abline(v = median(return, na.rm = TRUE),
         col = "pink",
         lwd = 3)

#Add st. deviation
abline(v = sd(return, na.rm = TRUE),
       col = "magenta",
       lwd = 3)

#Add legend
legend_colors <- c("black", "pink", "magenta")
legend_parameters <- c("Mean", "Median", "Standard Deviation")

legend(x = "topleft", legend_parameters, 
       col = legend_colors,
       lwd = c(2, 2, 2))

#------------------------------------------------------------------------------
# Step 6. Check whether daily returns follow a normal distribution 
#------------------------------------------------------------------------------

shapiro.test(as.vector(return))

#Conclusion: Based on Shapiro test, returns do not follow a normal distribution

#Compare with S&P500

getSymbols('^GSPC', from = "2015-04-22", to = "2017-04-21", src='yahoo')
last_year_DF_sp500 <- window(GSPC, start = "2016-04-22", to = "2017-04-21")
return_sp500 = Return.calculate(last_year_DF_sp500$GSPC.Adjusted, method = "discrete")
st_dev = StdDev(return_sp500, na.rm = TRUE)
annualized_stdev = st_dev * sqrt(252)
annualized_return = (2348.69 - 2091.58) / 2091.58
annualized_return
annualized_stdev

