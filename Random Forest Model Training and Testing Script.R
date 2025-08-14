# MODEL TRAINING FUNCTION -----------------------------------------------
train_random_forest_model <- function(train_data) {
  ctrl <- trainControl(
    method = "cv",
    number = 5,
    classProbs = TRUE,
    summaryFunction = multiClassSummary,
    allowParallel = TRUE
  )
  
  model <- train(
    x = select(train_data, -Isolate, -country),
    y = train_data$country,
    method = "ranger",
    importance = "permutation",
    num.trees = 500,
    metric = "Accuracy",
    trControl = ctrl
  )
  
  model
}

# MODEL TESTING FUNCTION ------------------------------------------------
predict_random_forest <- function(model, test_data) {
  predicted_class <- predict(model, test_data)
  predicted_probabilities <- predict(model, test_data, type = "prob")
  
  list(
    predicted_class = predicted_class,
    predicted_probabilities = predicted_probabilities
  )
}