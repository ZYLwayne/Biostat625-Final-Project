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
