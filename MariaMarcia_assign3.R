
library(dplyr)
library(haven)
library(lubridate)  


mret <- read_sas("/Users/majomarcia/Downloads/mret7023.sas7bdat")
mret <- as.data.frame(mret)

# Define function to process the data for each CUSIP
process_cusip <- function(data) {
  data <- data %>%
    arrange(DATE) %>%
    mutate(
      # Replace NA values in DIVAMT with 0
     DIVAMT = ifelse(is.na(DIVAMT), 0, DIVAMT),
      
      # Create a new column for adjusted price
     adjustedPrice = ifelse(abs(PRC) < 5000, PRC, NA),
      
      # Calculate end of the month
     end_Mo = floor_date(DATE, "month") + days(days_in_month(DATE) - 1),
      
      # Lagged price
      adjustedPrice2 = lag(adjustedPrice),
      
      # Calculate monthly return
      Calculation = ifelse(!is.na(adjustedPrice), (adjustedPrice - adjustedPrice2 + DIVAMT) / adjustedPrice2, NA),
      #Ensures the Mean return of leap or common year is positive
      PositiveReturn = ifelse(Calculation < 0, abs(Calculation), Calculation)
       
    )
  
  return(data)
}


# Apply the function to each group of CUSIP
mret <- mret %>%
  group_by(CUSIP) %>%
  do(process_cusip(.)) %>%
  ungroup()  # Ungroup to finalize the processing

# Remove rows with NA values in Calculation
mret <- mret %>%
  filter(!is.na(Calculation))

# Print the first few rows to check the results
print(head(mret))

# Calculate and print the correlation
correlation <- with(mret, cor(Calculation, RET, use = "complete.obs"))
print(paste("Correlation with RET:", correlation))
#Why is the Correlation not 1?
#Several Factors can contribute to this, one of them being the market conditions some market behaviors 
#cant be represented on a return calculation and can affect the correlation of RET. 


#PART B
#CALCULATION TO DETERMINE IF THE YEAR IS A LEAP YEAR OR NOT 

# Create a new column indicating the year and if it is a leap year
mret <- mret %>%
  mutate(
    Year = year(DATE),
    Month = month(DATE),
    IsLeapYear = leap_year(DATE)
  )

# Filter the dataset for February
february_data <- mret %>%
  filter(Month == 2)

# Summarize the average return by leap year status
february_summary <- february_data %>%
  group_by(IsLeapYear) %>%
  summarise(
    MeanReturn = mean(PositiveReturn, na.rm = TRUE),
    .groups = 'drop'
  )

print(february_summary)
#We can observe from the Mean Returns that whether its a leap year or common year has a important impact
