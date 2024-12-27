library(haven)
library(dplyr)  
library(ggplot2)




data <- read_sas("/Users/majomarcia/Downloads/compst7023.sas7bdat")
data <- as.data.frame(data)

head(data)

data$SIC2 <- substr(as.character(data$SIC), 1, 2)

calculate_IC <- function(df) {

  df <- df %>%
    arrange(desc(SLS)) %>%
    mutate(cumulative_sales = cumsum(SLS),  
           total_sales = sum(SLS)) %>%  
    filter(row_number() <= 5)  
  
  
  IC <- sum(df$SLS) / df$total_sales
  
  return(IC)
}


IC_data <- data %>%
  group_by(SIC2, year) %>%
  do(data.frame(IC = calculate_IC(.))) %>%
  ungroup()

average_IC <- IC_data %>%
  group_by(year) %>%
  summarize(avg_IC = mean(IC))


ggplot(average_IC, aes(x = year, y = avg_IC)) +
  geom_line() +
  labs(title = "Average Industry Concentration (IC) Over Time", 
       x = "Year", 
       y = "Average IC") +
  theme_minimal()


latest_year <- max(IC_data$year)


last_10_years <- latest_year - 10
recent_IC_data <- IC_data %>%
  filter(year >= last_10_years)


industry_IC_10years <- recent_IC_data %>%
  group_by(SIC2) %>%
  summarize(avg_IC_10years = mean(IC)) %>%
  arrange(avg_IC_10years)  


top_3_competitive_industries <- head(industry_IC_10years, 3)
print(top_3_competitive_industries)

