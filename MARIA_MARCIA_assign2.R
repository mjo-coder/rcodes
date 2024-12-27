library(dplyr) 
library(haven)
library(ggplot2)
library(tidyr)
library(broom)
## Read in data 
wvs <- read_sas("/Users/majomarcia/Desktop/FIN_R /wvs_dataset.sas7bdat")
as.data.frame(wvs)

View(wvs)

wvs1 <- 
  wvs %>% 
  mutate(trust = if_else(A165 == 1,1,0), trust = replace(trust, A165 < 1, NA), 
         inc=X047CS, inc = replace(inc, inc < 1, NA), 
         
         
         inc = if_else(X047CS >= 840001 & X047CS <= 840010, X047CS-840000 ,inc), 
         inc = if_else(X047CS >= 840011 & X047CS <= 840020, X047CS-840010 ,inc),
         inc = if_else(X047CS >= 840041 & X047CS <= 840050, X047CS-840040 ,inc), 
         inc = if_else(X047CS >= 840051 & X047CS <= 840060, X047CS-840050 ,inc), 
         hinc = if_else(inc >= 8,1,0),
# 1= HAPPY 0=NOT HAPPY
         happiness = A008,
         happiness = replace(happiness, happiness < 0, NA),
         
         happiness = if_else(happiness == 1 | happiness ==2,1,0),
  # 1=Voting 0= Not Voting       
         vote = E179,
         vote = replace(vote, vote < 0, NA),
         vote = if_else(vote == 5 | vote == 6,1,0),
# 1= important 0= Not in the list
         cqualities = A030, 
         cqualities = replace(cqualities, cqualities < 0, NA),
         cqualities = if_else(cqualities == 1, 1,0),
# 1= important 0= Not in the list
         csavings = A038, 
         csavings = replace(csavings, csavings < 0, NA),
         csavings = if_else(csavings == 1, 1,0),
# 1=important 0= Not Important
         goodpay = C011, 
         goodpay = replace(goodpay, goodpay < 0, NA),
         goodpay = if_else(goodpay == 1, 1,0), 
# 1=important 0= Not Important
         littlepressure = C012, 
         littlepressure = replace(littlepressure, littlepressure < 0, NA),
         littlepressure = if_else(littlepressure == 1, 1,0), 
# 1=important 0= Not Important
         jobsecurity= C013, 
         jobsecurity = replace(jobsecurity, jobsecurity < 0, NA),
         jobsecurity = if_else(jobsecurity == 1, 1,0), 
# 1=important 0= Not Important
         respectedjob= C014, 
         respectedjob = replace(respectedjob, respectedjob < 0, NA),
         respectedjob = if_else(respectedjob == 1, 1,0), 
# 1=important 0= Not Important
         goodhours= C015, 
         goodhours = replace(goodhours, goodhours < 0, NA),
         goodhours = if_else(goodhours == 1, 1,0), 
# 1=important 0= Not Important
         initiativejob= C016, 
         initiativejob = replace(initiativejob, initiativejob < 0, NA),
         initiativejob = if_else(initiativejob == 1, 1,0), 
# 1=important 0= Not Important
         holidays= C017, 
         holidays = replace(holidays, holidays < 0, NA),
         holidays = if_else(holidays == 1, 1,0), 
# 1=important 0= Not Important
        achievement= C018, 
        achievement = replace(achievement, achievement < 0, NA),
        achievement = if_else(achievement == 1, 1,0), 
# 1=important 0= Not Important
        responsiblejob= C019, 
        responsiblejob = replace(responsiblejob, responsiblejob < 0, NA),
        responsiblejob = if_else(responsiblejob == 1, 1,0), 
# 1=important 0= Not Important
        interestingjob= C020, 
        interestingjob = replace(interestingjob, interestingjob < 0, NA),
        interestingjob = if_else(interestingjob == 1, 1,0), 
# 1=important 0= Not Important
        oneabilities= C021, 
        oneabilities = replace(oneabilities, oneabilities < 0, NA),
        oneabilities = if_else(oneabilities == 1, 1,0), 
# 1= cautious  0= not cautious
        lifechanges = E045, 
        lifechanges = replace(lifechanges, lifechanges < 0, NA), 
        lifechanges = if_else(lifechanges >= 1 & lifechanges <= 5, 1,0),

# 1 =competition good  0=competition bad
        competition = E039,
        competition = replace(competition, competition < 0, NA), 
        competition = if_else(competition >= 1 & competition <=5, 1, 0),
# 1 = Male  0=Female
        sex = X001, 
        sex = replace(sex, sex < 1, NA), 
        sex = if_else(sex == 1, 1, 0),
#Age 
        age = X003, 
        age = replace (age, age < 1, NA), 
        age = ifelse(!is.na(X003), X003,NA), 
# Marital Status 
# 2 =Married /Living together, 1= divorced/separate, 0=single/never married

        maritalstatus = X007, 
        maritalstatus = replace(maritalstatus, maritalstatus < 0, NA),
        maritalstatus = if_else(maritalstatus == 1 | maritalstatus == 2, 2, maritalstatus),
        maritalstatus = if_else(maritalstatus == 3 | maritalstatus == 4 | maritalstatus == 5 | maritalstatus ==7 | maritalstatus ==8, 1, maritalstatus),
        maritalstatus = if_else(maritalstatus ==6, 0,maritalstatus),
#highest Educational level
       hedulevel = X025, 
       hedulevel = replace(hedulevel, hedulevel < 0, NA), 
       hedulevel = if_else(hedulevel < 6, 0, hedulevel),
       hedulevel= if_else(hedulevel == 6 | hedulevel == 7, 1, hedulevel), 
       hedulevel = if_else (hedulevel ==8 , 2, hedulevel), 
 #employment 
      employed = X028, 
      employed = replace(employed, employed < 0, NA),
      employed = if_else(employed == 1 | employed == 2 | employed ==3, 1 ,0),

    
      )  %>%

 

    select(year, S003, trust, X047CS, inc, hinc, A008, happiness, E179, vote, A030,cqualities, A038, csavings, C011,goodpay, C012, littlepressure, C013, jobsecurity, C014, respectedjob, C015, goodhours, C016, initiativejob, C017, holidays, C018, achievement, C019, responsiblejob, C020, interestingjob, C021, oneabilities, E045, lifechanges, E039, competition, X001, sex, X007,maritalstatus, age, X025, hedulevel, X028, employed)  
#Assignment 2 STARTS HERE
df_summary <- wvs1 %>%
  group_by(sex) %>%
  #summarize(mean_value = mean(competition, na.rm = TRUE))
  summarize(across(where(is.numeric), ~mean(.x, na.rm =TRUE)))
#prints the dataset 
print(df_summary) 
df_long <- df_summary %>%
  pivot_longer(
    cols = -sex,
    names_to = "variable",
    values_to = "average"
  )

print(df_long)
#Write the averages to a csv file
write.csv(df_long, file = "averages.csv", row.names = FALSE)
ggplot(df_long, aes(x = sex, y = average, fill = as.factor(sex))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("1" = "steelblue" , "0" = "pink")) +
  labs(
    x = "Variable",
    y = "Average Value",
    fill = "Sex",
    title = "Average Values of Numeric Variables by Sex Male = Blue and Female = Pink"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))





#RSQUARED

vars <- c("hinc", "happiness", "vote", "cqualities", "csavings",
                      "goodpay", "littlepressure", "jobsecurity", "respectedjob",
                      "goodhours", "initiativejob", "holidays", "achievement",
                      "responsiblejob", "interestingjob", "oneabilities", "lifechanges",
                      "competition", "sex", "maritalstatus", "age", "hedulevel", "employed")

# Initialize lists where results will be stored 
r_squared_list <- list()
results_list <- list()

# regressions 
#extraction R-squared values
for (var in vars) {
  formula <- as.formula(paste("inc ~", var))
  model <- lm(formula, data = wvs1)
  r_squared_list[[var]] <- glance(model)$r.squared
}

# Combines R-squared results
r_squared_df <- tibble(variable = names(r_squared_list), r_squared = unlist(r_squared_list))

# Ranking vars based on their RSquared Values
r_squared_df <- r_squared_df %>%
  arrange(desc(r_squared))

print(r_squared_df)
#write the ranking of the variables based on r-squared to a csv file
write.csv(r_squared_df, file = "variable_rsquared.csv", row.names = FALSE)

# Select the top 3 
top_vars <- r_squared_df %>%
  slice_head(n = 3) %>%
  pull(variable)

print(top_vars)

# multiple regression model with top 3 
formula <- as.formula(paste("inc ~", paste(top_vars, collapse = " + ")))
multi_model <- lm(formula, data = wvs1)


summary(multi_model)

# Plot R-squared values for the BARPLOT
ggplot(r_squared_df, aes(x = reorder(variable, r_squared), y = r_squared)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(
    x = "Variable",
    y = "R-squared",
    title = "R-squared Values of Regressions for Each Predictor"
  ) +
  theme_minimal()

#View(wvs1)

#summary(wvs1) 

