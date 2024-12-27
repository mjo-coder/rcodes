
#Maria Jose Marcia
library(dplyr)
library(zoo)
library(lubridate)
library(haven)
library(caret)
library(pROC)
library(nnet)
library(rpart)  


data <- read_sas("/Users/majomarcia/Downloads/mret7023.sas7bdat")
data <- as.data.frame(data)


data <- data %>% mutate(CRASH = ifelse(RET < -0.08, 1, 0))


date_column_name <- "DATE"  
data[[date_column_name]] <- as.Date(data[[date_column_name]], format = "%Y-%m-%d") 

# Filter for last 5 years
data <- data %>% filter(!!sym(date_column_name) >= Sys.Date() - years(5))

# Arrange data and create lagged variables
data <- data %>%
  arrange(DATE) %>%
  mutate(
    Lag1_Return = lag(RET, 1),
    Lag2_Return = lag(RET, 2),
    Lag3_Return = lag(RET, 3),
    Abs_Return = abs(RET),
    Lag1_Abs_Return = lag(Abs_Return, 1),
    Lag2_Abs_Return = lag(Abs_Return, 2),
    Lag3_Abs_Return = lag(Abs_Return, 3),
    Lag4_Abs_Return = lag(Abs_Return, 4),
    Lag5_Abs_Return = lag(Abs_Return, 5),
    
    Lag1_Market_Cap = lag(data$PRC * data$SHROUT, 1),  
    Lag1_Turnover = lag(data$VOL / data$SHROUT, 1)
  ) %>%
  na.omit()

# Check class distribution
if (sum(data$CRASH == 1) == 0 || sum(data$CRASH == 0) == 0) {
  stop("Data does not contain both classes after filtering. Please check your dataset.")
}

# sampling for training and test sets
set.seed(123)
train_index <- createDataPartition(data$CRASH, p = 0.8, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]


if (min(table(train_data$CRASH)) == 0) {
  stop("Training data does not contain both classes. Adjust the sampling.")
}

# Convert CRASH -> factor
train_data$CRASH <- factor(train_data$CRASH, levels = c(0, 1))
test_data$CRASH <- factor(test_data$CRASH, levels = c(0, 1))

# Logistic Regression Model
model <- glm(CRASH ~ Lag1_Return + Lag2_Return + Lag3_Return + 
               Lag1_Abs_Return + Lag2_Abs_Return + Lag3_Abs_Return + 
               Lag4_Abs_Return + Lag5_Abs_Return +
               Lag1_Market_Cap + 
               Lag1_Turnover, 
             data = train_data, 
             family = binomial)

# Predictions and AUC for Logistic Regression
predictions <- predict(model, newdata = test_data, type = "response")
roc_curve <- roc(test_data$CRASH, predictions)
auc_value <- auc(roc_curve)
print(paste("AUC with Logistic Regression:", auc_value))

# Neural Network Model
nn_model <- nnet(CRASH ~ Lag1_Return + Lag2_Return + Lag3_Return + 
                   Lag1_Abs_Return + Lag2_Abs_Return + Lag3_Abs_Return + 
                   Lag4_Abs_Return + Lag5_Abs_Return + Lag1_Market_Cap + 
                   Lag1_Turnover, 
                 data = train_data, 
                 size = 5, 
                 decay = 0.01, 
                 maxit = 200)

# Make predictions with Neural Network
nn_probabilities <- predict(nn_model, newdata = test_data, type = "raw")
roc_nn <- roc(test_data$CRASH, nn_probabilities)
auc_nn <- auc(roc_nn)
print(paste("AUC for Neural Network:", auc_nn))





