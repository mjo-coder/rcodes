
library(haven)  
library(dplyr)  
library(tidyr)  
library(quantmod)  


data <- read_sas("/Users/majomarcia/Downloads/dret1519.sas7bdat")
str(data)
# Define positions 
positions <- data.frame(
  stock = c("IBM", "MSFT_long", "MSFT_short", "GM"),
  shares = c(1.212e6, 3.444e6, -1.872e6, -0.568e6),
  stringsAsFactors = FALSE
)
#Define Cash
cash <- 5.380e6

#Prices of stocks based on specific date
ibm_price <- data %>% filter(PERMNO == 12490 & DATE == "2018-10-12") %>% select(PRC) %>% pull()
msft_price <- data %>% filter(PERMNO == 10107 & DATE == "2018-10-12") %>% select(PRC) %>% pull()
gm_price <- data %>% filter(PERMNO == 12369 & DATE == "2018-10-12") %>% select(PRC) %>% pull()

#add prices to datafrsame
positions$price <- c(ibm_price, msft_price, msft_price, gm_price)

# Value of each position
positions$value <- positions$shares * positions$price

# Total portfolio value
total_value <- sum(positions$value) + cash
print(total_value)

# Weights of each asset
positions$weight <- positions$value / total_value
print(positions)

# Daily returns
returns <- data %>% 
  filter(DATE >= "2015-01-01" & DATE <= "2018-10-12") %>%
  select(DATE, PERMNO, RET)


returns_wide <- returns %>%
  spread(key = PERMNO, value = RET)


colnames(returns_wide)[colnames(returns_wide) == "12490"] <- "IBM"
colnames(returns_wide)[colnames(returns_wide) == "10107"] <- "MSFT"
colnames(returns_wide)[colnames(returns_wide) == "12369"] <- "GM"

# Calculate portfolio returns
returns_wide <- returns_wide %>%
  mutate(portfolio_return = (positions$weight[1] * IBM + 
                               (positions$weight[2] - positions$weight[3]) * MSFT + 
                               positions$weight[4] * GM))

# Sort returns and find the 5%
var_hist <- quantile(returns_wide$portfolio_return, 0.05, na.rm = TRUE)
var_hist_value <- -var_hist * total_value
print(var_hist_value)

# Calculate the covariance matrix
cov_matrix <- cov(returns_wide[c("IBM", "MSFT", "GM")], use = "pairwise.complete.obs")


# Calculate the portfolio variance
weights <- c(positions$weight[1], positions$weight[2] - positions$weight[3], -positions$weight[4])  
portfolio_variance <- t(weights) %*% cov_matrix %*% weights
portfolio_sd <- sqrt(portfolio_variance)

# VaR using Variance-Covariance Method
z_value <- qnorm(0.05)  # 5% VaR
var_vcv <- -z_value * portfolio_sd * total_value
print(var_vcv)
#VaR using Historical simulation Method
 historical_var <- quantile(returns_wide$portfolio_return, 0.05, na.rm = TRUE) * total_value
 print(historical_var)

 #RESULTS
 cat("Total Portfolio Value: $", total_value, "\n")
 print("Weights of each asset:")
 print(positions)
 cat("VaR at 5% (Variance-Covariance): $", var_vcv, "\n")
 cat("VaR at 5% (Historical Simulation Method): $", historical_var, "\n")

 