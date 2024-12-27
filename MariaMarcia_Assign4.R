library(dplyr)
library(tidyr)
library(ggplot2)
library(lubridate)

#------------PART A 
mret <- read_sas("/Users/majomarcia/Downloads/mret7023.sas7bdat")
mret <- as.data.frame(mret)

# Calculate market capitalization
mret <- mret %>%
  mutate(mcap = PRC * SHROUT)

mret$DATE <- as.Date(mret$DATE, format="%Y-%m-%d")
#Grouping by month and year 
# Then, extract the firm wiht the highest mcap for each month

largest_firm_per_month <- mret %>%
  mutate(year = format(DATE, "%Y"), month = format(DATE, "%m")) %>%
  group_by(year, month) %>%
  slice(which.max(mcap)) %>%
  ungroup() %>%
  select(DATE, PERMNO, COMNAM, mcap)
#Ranking

largest_firm_per_month <- largest_firm_per_month %>%
  arrange(DATE) %>%
  mutate(rank = cumsum(lag(PERMNO, default = first(PERMNO)) !=PERMNO)) %>%
  
  group_by(PERMNO) %>%
  
  
 
  summarise(duration = n()) %>%
  arrange(desc(duration))

#Longest duration company 
longest_streak_company <- largest_firm_per_month[1,]
  
cat(
    "\nCompany with PERMNO ", longest_streak_company$PERMNO, 
    "\nheld the #1 spot for \n", longest_streak_company$duration, "months.\n")

#---------PART B 

mret <- mret %>%
  mutate(mcap = PRC * SHROUT)

#date format
mret$DATE <- as.Date(mret$DATE, format="%Y-%m-%d")

# calculate value-weighted returns
calculate_portfolio_returns <- function(data, n) {
  data %>%
    group_by(DATE) %>%
   
    slice_max(order_by = mcap, n = n) %>%
    # Calculate value-weighted returns
    summarize(portfolio_return = sum(RET * mcap) / sum(mcap),
              .groups = 'drop')
}

# Create portfolios 
# store their returns
portfolio_returns <- lapply(seq(10, 100, by = 10), function(n) {
  calculate_portfolio_returns(mret, n) %>%
    mutate(portfolio_size = n)
})

# In a single data frame store the combination of portfolio returns  
portfolio_returns_df <- bind_rows(portfolio_returns)


vwretd_data <- mret %>%
  select(DATE, VWRETD) %>%
  distinct()

correlations <- portfolio_returns_df %>%
  left_join(vwretd_data, by = "DATE") %>%
  group_by(portfolio_size) %>%
  summarize(correlation = cor(portfolio_return, VWRETD, use = "complete.obs"),
            .groups = 'drop')

#histogram
ggplot(correlations, aes(x = correlation)) +
  geom_histogram(binwidth = 0.05, color = "skyblue", fill = "lightblue") +
  labs(title = "  Correlations of The Returns with VWRETD",
       x = "Correlation Coefficient",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(color = "darkblue", size = 14))

print(correlations)

