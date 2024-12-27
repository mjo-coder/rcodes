# Load required libraries
library(dplyr)
library(ggplot2)
library(haven)
library(readxl)

data <- read_sas("/Users/majomarcia/Downloads/compst7023.sas7bdat")
data <- as.data.frame(data)
head(data)

# Create SIC2 variable
data$SIC2 <- substr(data$SIC, 1, 2)

# Calculate IC for each SIC2 and YEAR
ic_data <- data %>%
  group_by(SIC2, year) %>%
  arrange(desc(SLS)) %>%
  mutate(
    top5_sales = sum(head(SLS, 5)),
    total_sales = sum(SLS),
    IC = if(n() < 5) 1 else top5_sales / total_sales
  ) %>%
  summarise(IC = first(IC)) %>%
  ungroup()

# A) Calculate and plot average IC each year
avg_ic_by_year <- ic_data %>%
  group_by(year) %>%
  summarise(avg_IC = mean(IC, na.rm = TRUE))

# Plot average IC over time
ggplot(avg_ic_by_year, aes(x = year, y = avg_IC)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Average Industry Concentration Over Time",
       x = "Year",
       y = "Average IC")

# B) Identify 3 most competitive industries over last 10 years
last_10_years <- max(ic_data$year) - 9

most_competitive <- ic_data %>%
  filter(year >= last_10_years) %>%
  group_by(SIC2) %>%
  summarise(avg_IC = mean(IC, na.rm = TRUE)) %>%
  arrange(avg_IC) %>%
  head(3)

# Read SIC code definitions
sic_definitions <- read_excel("/Users/majomarcia/Downloads/SIC2.xls")

print(names(sic_definitions))
print(head(sic_definitions))

# Combine all columns except the first into a single definition column
sic_definitions <- sic_definitions %>%
  mutate(Definition = apply(.[,-1], 1, paste, collapse = " ")) %>%
  select(SIC2 = 1, Definition)





#was SIC2, avg_IC, Definition
# Join with SIC definitions
most_competitive_with_def <- most_competitive %>%
  left_join(sic_definitions, by = "SIC2") 
  



# Print results
print("The 3 most competitive companies over the last 10 years:")
print(most_competitive_with_def)

# If you want to see which industries might be missing definitions
missing_definitions <- most_competitive %>%
  anti_join(sic_definitions, by = "SIC2")

print("Industries missing definitions:")
print(missing_definitions)

# Write the results to a CSV file
write.csv(most_competitive_with_def, "/Users/majomarcia/Downloads/most_competitive_industries.csv", row.names = FALSE)

# If you also want to write industries missing definitions to a separate CSV file
write.csv(missing_definitions, "/Users/majomarcia/Downloads/missing_definitions.csv", row.names = FALSE)

