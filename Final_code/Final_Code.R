rm(list = ls())

# Load R libraries
library(dplyr) # %>% 
library(ggplot2) # ggplot
library(tidyverse) # rownames_to_column
library(kableExtra) # kbl
library(broom) # tidy
library(gridExtra) # grid.arrange
library(scales) # comma
library(e1071) # skewness
library(ggpubr) # stat_compare_means & ggarrange
library(fastDummies) # dummy_cols
library(caret) # createDataPartition
library(car) # vif
library(rpart) # rpart
library(Metrics) # rmse

#### One.Read Data ####
setwd("/Users/15792/Desktop/HW/625/Final Project") # Set Working Directory
data <- read.csv("INF002v4.csv", header = T) # Read data

## 1. Convert to numeric variables
clean_and_convert_to_numeric <- function(x) {
  x_cleaned <- gsub(",", "", x)  # Remove thousandths comma
  as.numeric(x_cleaned)  # Convert to numeric variables
}
data <- data %>% mutate(Total.Charges = clean_and_convert_to_numeric(Total.Charges),
                        Total.Costs = clean_and_convert_to_numeric(Total.Costs),
                        Length.of.Stay = as.numeric(Length.of.Stay))

# 2. Convert to categorical variable
data <- data %>% mutate(across(where(is.character), as.factor))

# 3. Variables Desciption table
data_str <- capture.output(str(data))  # Captures the output of str()

var_info <- lapply(data_str[-1], function(line) { # Parsing output to extract variable names and types
  parts <- strsplit(line, ":")  # split by colon
  name <- trimws(parts[[1]][1]) 
  type <- trimws(parts[[1]][2])
  list(name = name, type = type)
})
var_info_df <- do.call(rbind, lapply(var_info, as.data.frame))
var_info_df$type <- gsub("^(Factor w/ \\d+ levels).*", "\\1", var_info_df$type)  
var_info_df$type <- gsub("^(Factor w/ \\d+ level).*", "\\1", var_info_df$type)  
var_info_df$type <- gsub("^(num).*", "\\1", var_info_df$type)  
var_info_df$type <- gsub("^(int).*", "\\1", var_info_df$type)  
var_info_df$name <- gsub("^\\$\\s*", "", var_info_df$name)

var_info_df <- var_info_df %>% 
  mutate(Role = ifelse(name == "Length.of.Stay", "Target", "Feature"))

kbl(var_info_df, caption = "the Description of Variables", booktabs = T) %>% 
  kable_styling(latex_options = "striped", full_width = FALSE, font_size = 7)

#### Two.Exploratory Data Analysis ####

# 1. Data preprocessing
## 1.1 Missing data
data <- data %>% 
  mutate_all(~na_if(trimws(.), "")) %>% # Replace blank values with NA
  mutate(Ethnicity = na_if(Ethnicity, "Unknown")) %>% 
  mutate(Gender = na_if(Gender, "U")) %>% 
  mutate(Total.Charges = clean_and_convert_to_numeric(Total.Charges),
         Total.Costs = clean_and_convert_to_numeric(Total.Costs),
         Length.of.Stay = as.numeric(Length.of.Stay)) %>% 
  mutate(across(where(is.character), as.factor))

missing_values_df <- data.frame(
  Variable = names(sapply(data, function(x) sum(is.na(x)))),
  Missing_ratio = round(sapply(data, function(x) sum(is.na(x))) / nrow(data),5),
  Missing_cnt = sapply(data, function(x) sum(is.na(x)))
)
missing_values_df %>% 
  filter(Missing_ratio > 0) %>% 
  ggplot(aes(x = reorder(Variable, -Missing_ratio), y = Missing_ratio)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = scales::percent(Missing_ratio)), hjust = 0.5, size = 3) +  
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "VariableS", y = "Proportion of Missing Values") +
  theme_minimal()
sum(is.na(data))
MissingDropVar <- c("Payment.Typology.3", "Payment.Typology.2") # Remove variables with more than 40% missing values
data <- data %>%  select(-one_of(MissingDropVar))

sum(is.na(data))

# Mode filling 
mode_fill_1 <- names(sort(table(data$Hospital.Service.Area), decreasing = TRUE))[1]
data$Hospital.Service.Area[is.na(data$Hospital.Service.Area)] <- mode_fill_1

mode_fill_2 <- names(sort(table(data$APR.Severity.of.Illness.Description), decreasing = TRUE))[1]
data$APR.Severity.of.Illness.Description[is.na(data$APR.Severity.of.Illness.Description)] <- mode_fill_2

mode_fill_3 <- names(sort(table(data$APR.Risk.of.Mortality), decreasing = TRUE))[1]
data$APR.Risk.of.Mortality[is.na(data$APR.Risk.of.Mortality)] <- mode_fill_3

mode_fill_4 <- names(sort(table(data$Ethnicity), decreasing = TRUE))[1]
data$Ethnicity[is.na(data$Ethnicity)] <- mode_fill_4

mode_fill_5 <- names(sort(table(data$Gender), decreasing = TRUE))[1]
data$Gender[is.na(data$Gender)] <- mode_fill_5

sum(is.na(data)) # Check if there are any missing values left

##### Three.Explore the data and report 3 notable findings. ####
# 1. Distribution of Demographics in terms of Length of Stay
p1 <- ggplot(data, aes(x = Hospital.Service.Area, y = log(Length.of.Stay), fill = Hospital.Service.Area)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) +  
  labs(x = "Hospital.Service.Area", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

p2 <- ggplot(data, aes(x = Age.Group, y = log(Length.of.Stay), fill = Age.Group)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) + # Calculate p-value
  labs(x = "Age.Group", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

p3 <- ggplot(data, aes(x = Gender, y = log(Length.of.Stay), fill = Gender)) +
  geom_boxplot() +
  stat_compare_means(method = "wilcox.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) +  
  labs(x = "Gender", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

p4 <- ggplot(data, aes(x = Race, y = log(Length.of.Stay), fill = Race)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) +  
  labs(x = "Race", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

p5 <- ggplot(data, aes(x = Ethnicity, y = log(Length.of.Stay), fill = Ethnicity)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) +  
  labs(x = "Ethnicity", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

ggarrange(p1, ggarrange(p2, p3, ncol = 2), ggarrange(p4, p5, ncol = 2), ncol = 1)

# 2. Distribution of Admission Information in terms of Length of Stay
q1 <- ggplot(data, aes(x = Type.of.Admission, y = log(Length.of.Stay), fill = Type.of.Admission)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) +  
  labs(x = "Type.of.Admission", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

q2 <- ggplot(data, aes(x = Emergency.Department.Indicator, y = log(Length.of.Stay), fill = Emergency.Department.Indicator)) +
  geom_boxplot() +
  stat_compare_means(method = "kruskal.test", label = "p.format", size = 3, label.y = 4, hjust = -0.5) +  
  labs(x = "Emergency.Department.Indicator", y = "log(Length of Stay)") +
  theme_minimal() +
  theme(legend.position = "none")

ggarrange(q1, q2, ncol = 2)


# 3. Linear relationship between Charges and Costs
model_nonlog <- lm(Total.Charges ~ Total.Costs, data = data)
model_nonlog_summary <- tidy(model_nonlog)
intercept <- round(model_nonlog_summary$estimate[1], 2)  # intercept
slope <- round(model_nonlog_summary$estimate[2], 2)      # slope
p_value <- round(model_nonlog_summary$p.value[2], 2)     # p-value
adjusted_r_squared <- round(summary(model_nonlog)$adj.r.squared,4) # R squared

r1 <- ggplot(data = data, aes(x = Total.Costs, y = Total.Charges)) +
  geom_point(col = 'blue') + 
  geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +
  annotate("text", x = min(data$Total.Costs), y = max(data$Total.Charges) * 0.8, 
           label = paste0("y = ", slope, "x + ", intercept), 
           hjust = 0, size = 4, color = "black") +
  annotate("text", x = min(data$Total.Costs), y = max(data$Total.Charges) * 0.75, 
           label = paste0("P value = ", p_value,";","R squared = ", adjusted_r_squared),
           hjust = 0, size = 3, color = "red") + 
  labs(title = "Scatter plot",
       x = "Total Costs",
       y = "Total Charges") +
  theme_minimal() + scale_x_continuous(labels = comma) + scale_y_continuous(labels = comma) 

model_log <- lm(log(Total.Charges) ~ log(Total.Costs), data = data)
model_log_summary <- tidy(model_log)
intercept <- round(model_log_summary$estimate[1], 2)  # intercept
slope <- round(model_log_summary$estimate[2], 2)      # slope
p_value <- round(model_log_summary$p.value[2], 2)     # p-value
adjusted_r_squared <- round(summary(model_log)$adj.r.squared,4) # R squared

r2 <- ggplot(data = data, aes(x = log(Total.Costs), y = log(Total.Charges))) +
  geom_point(col = 'blue') + 
  geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1)) +
  annotate("text", x = min(log(data$Total.Costs)), y = max(log(data$Total.Charges)) * 0.78, 
           label = paste0("y = ", slope, "x + ", intercept), 
           hjust = 0, size = 4, color = "black") +
  annotate("text", x = min(log(data$Total.Costs)), y = max(log(data$Total.Charges)) * 0.755, 
           label = paste0("P value = ", p_value,";","R squared = ", adjusted_r_squared),
           hjust = 0, size = 3, color = "red") + 
  labs(title = "Scatter plot-log transformation",
       x = "Total Costs",
       y = "Total Charges") +
  theme_minimal() 


log_charges <- log(data$Total.Charges)
r3 <- ggplot(data, aes(x = log_charges)) +
  geom_density(fill = "blue", alpha = 0.5) +
  stat_density(aes(y = ..density..), geom = "line") +
  # Use the maximum y-value of the density plot to label the skewness value.
  annotate("text", x = mean(log_charges), 
           y = max(density(log_charges)$y),  
           label = paste("Skewness:", round(skewness(log_charges), 3)), 
           size = 3, color = "red") +
  labs(title = "Density Plot of Log(Total Charges) ",
       x = "Log(Total Charges)",
       y = "Density") +
  theme_minimal()

log_costs <- log(data$Total.Costs)
r4 <- ggplot(data, aes(x = log_costs)) +
  geom_density(fill = "blue", alpha = 0.5) +
  stat_density(aes(y = ..density..), geom = "line") +
  annotate("text", x = mean(log_costs), 
           y = max(density(log_costs)$y),  
           label = paste("Skewness:", round(skewness(log_costs), 3)), 
           size = 3, color = "red") +
  labs(title = "Density Plot of Log(Total Costs) ",
       x = "Log(Total Charges)",
       y = "Density") +
  theme_minimal()

grid.arrange(r1,r2,r3,r4, ncol = 2)





#### Four.Modeling  ####
# 1. Feature engineering
## 1.1 Deleting variables with only one value
dropVar <- c("CCSR.Diagnosis.Code", "CCSR.Diagnosis.Description")
data <- data %>% select(-one_of(dropVar))
## 1.2 One hot encoding the categorical variables
dataDummies <- data %>% 
  select_if(is.factor) %>% 
  dummy_cols(remove_first_dummy = TRUE, remove_selected_columns = TRUE) 
## 1.3 Logarithmic transformation
dataNum <- data %>%  
  select_if(is.numeric) %>% 
  mutate(across(everything(), log))

# 2. Dataset segmentation
## Combined data
modelData <- cbind(dataNum, dataDummies)
print(paste0("Number of independent variables after feature engineering is:",dim(modelData)[[2]]-1))
# Setting the random seed
set.seed(123) 
# Partition the dataset
train_index <- createDataPartition(modelData$Length.of.Stay, p = 0.7, list = FALSE) 
# Create training and test sets
modelData <- modelData %>%
  rename_with(~ gsub(" ", "_", .)) %>%
  rename_with(~ gsub("-", "_", .)) %>%
  rename_with(~ gsub("/", "_", .)) %>%
  rename_with(~ gsub("'", "_", .)) %>%
  rename_with(~ gsub("\\(", "", .)) %>%
  rename_with(~ gsub("\\)", "", .)) %>%
  rename_with(~ gsub(">", "_", .)) %>%
  rename_with(~ gsub(",", "_", .))

train_data <- modelData[train_index, ] 
test_data <- modelData[-train_index, ]

#### Model 1. Linear Regression ####
lm.model <- glm(Length.of.Stay~., data = train_data)
stepwise_model <- step(lm.model, direction = "backward")

vif_values <- vif(stepwise_model)
stepwise_vars <- names(vif_values)
stepwise_vars <- gsub("`", "", stepwise_vars)
print(paste0("Number of independent variables after stepwise regression is:",length(stepwise_vars)))
high_vif_vars <- names(vif_values[vif_values > 10])
remaining_vars <- setdiff(stepwise_vars, high_vif_vars)
print(paste0("Number of independent variables after stepwise regression and VIF is:",length(remaining_vars)))

train_data_new <- train_data %>% 
  select(c("Length.of.Stay", intersect(remaining_vars, names(train_data))))

test_data_new <- test_data %>% 
  select(c("Length.of.Stay", intersect(remaining_vars, names(test_data))))


lm.model.updated <- glm(Length.of.Stay~., data = train_data_new)

importance <- varImp(lm.model.updated, scale = FALSE)
importance$Variable <- rownames(importance)

# Mapping the importance of variables
ggplot(importance, aes(x = reorder(Variable, Overall), y = Overall)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Variable Importance", x = "Variables", y = "Importance Score") +
  theme_minimal()

# Predict and calculate RMSE
predict_lm <- predict(lm.model.updated, newdata = test_data_new[,-1])
predict_lm <- exp(predict_lm)
rmse_lm <- rmse(predict_lm, test_data_new$Length.of.Stay)
print(paste("RMSE for Linear Regression: ", rmse_lm))

#### Model 2. Classification and Regression Trees ####
library(rpart)
library(rpart.plot)
set.seed(1234)
rpart_model <- rpart(Length.of.Stay ~., data = train_data, method = "anova")
rpart.plot(rpart_model, box.palette = c("pink", "gray"))
num_terminal_nodes <- sum(rpart_model$frame$var == "<leaf>")
print(paste0("The number of terminal nodes is:",num_terminal_nodes))

predict_rpart <- predict(rpart_model, newdata =  test_data[,-1])
predict_rpart <- exp(predict_rpart)
rmse_rpart <- rmse(predict_rpart, test_data$Length.of.Stay)
print(paste("RMSE for CART: ", rmse_rpart))


#### Model 3. Random Forest ##### 
library(randomForest)
set.seed(123)  # Set random seed for reproducibility

# Train Random Forest model
rf_model <- randomForest(Length.of.Stay ~ ., data = train_data, ntree = 500, mtry = sqrt(ncol(train_data)-1))
# Print the Random Forest model summary
print(rf_model)
# Evaluate the importance of variables
rf_importance <- importance(rf_model)
rf_importance_df <- data.frame(Variable = rownames(rf_importance), Importance = rf_importance[, 1])
# Plot variable importance
ggplot(rf_importance_df, aes(x = reorder(Variable, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "orange") +
  coord_flip() +
  labs(title = "Random Forest - Variable Importance", x = "Variables", y = "Importance Score") +
  theme_minimal()

# Predict on test set
predict_rf <- predict(rf_model, newdata = test_data[,-1])

# Transform the predictions back (log scale)
predict_rf <- exp(predict_rf)

# Calculate RMSE for Random Forest model
rmse_rf <- rmse(predict_rf, test_data$Length.of.Stay)
print(paste("RMSE for Random Forest: ", rmse_rf))


#### Cross-Validation for Linear Regression, CART, Random Forest Model ####

# 5-fold cv
train_control <- trainControl(method = "cv", number = 5, savePredictions = "all")

# 1. Linear Regression with Cross-validation
set.seed(123)
cv_lm <- train(Length.of.Stay ~ ., data = train_data_new, method = "lm", trControl = train_control)
print(cv_lm)
rmse_lm_cv <- cv_lm$results$RMSE
print(paste("RMSE for Linear Regression with CV: ", cv_lm$results$RMSE))

# 2.2 CART with Cross-validation
set.seed(123)
cv_rpart <- train(Length.of.Stay ~ ., data = train_data_new, method = "rpart", trControl = train_control)
print(cv_rpart)
rmse_rpart_cv <- cv_rpart$results$RMSE[1]
print(paste("RMSE for CART with CV: ", rmse_rpart_cv))

# 2.3 Random Forest with Cross-validation
set.seed(123)
cv_rf <- train(Length.of.Stay ~ ., data = train_data_new, method = "rf", trControl = train_control, ntree = 100)
print(cv_rf)
rmse_rf_cv <- cv_rf$results$RMSE[2]
print(paste("RMSE for Random Forest with CV: ", rmse_rf_cv))

#### Five.RMSE Results for the three models  ####
results_rmse <- data.frame(
  Model = c("Linear Regression", "CART", "Random Forest"),
  RMSE_No_CV = c(rmse_lm, rmse_rpart, rmse_rf),
  RMSE_With_CV = c(rmse_lm_cv, rmse_rpart_cv, rmse_rf_cv))

kbl(results_rmse, caption = "RMSE for the three models", booktabs = True) %>% 
  kable_styling(latex_options = "striped", full_width = FALSE, font_size = 7) 

