#' K Nearest Neighbors Cross-Validation
#'
#' This function will predict output `body_mass_g` using covariates
#'   `bill_length_mm`, `bill_depth_mm`, and `flipper_length_mm`. All the data
#'   is in the dataframe penguins
#'
#' @param k Number of folds
#'
#' @return A numeric with the cross-validation error
#'
#' @example
#' my_rf_cv(k = 5)
#'
#' @export
my_rf_cv <- function(k) {
  penguins <- tidyr::drop_na(my_penguins)
  # Split data into k folds randomly
  fold <- sample(rep(1:k, length = nrow(penguins)), replace = TRUE)
  # Generate complete data
  data <- penguins %>% select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm)
  data$split = fold
  # Empty rep to store preiction results
  prediction_result <- rep(NA, nrow(data))
  for (i in 1:k) {
    data_train <- data %>% filter(split != i)
    data_test <- data %>% filter(split == i)
    #Train our model and record predictions and errors
    forest_model <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + flipper_length_mm, data = data_train, ntree = 100)
    prediction_result[fold == i] <- predict(forest_model, data_test[, -1])
  }
  mse_err <- c()
  for (i in 1:k) {
    mse_err[i] <- mse(prediction_result[i], data$body_mass_g[i])
  }

  return(mean(mse_err))
}
