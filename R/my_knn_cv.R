#' K Nearest Neighbors Cross-Validation
#'
#' This functions will predict output class by using input data and gives out
#' cross-validation error
#'
#' @param train Input data frame
#' @param cl Input data frame.
#' @param k_nn Integer representing the number of neighbors
#' @param k_cv Integer representing the number of folds
#'
#' @return a list with objects, containing \code{class} representing a vector of
#'   the predicted class $\hat{Y}_{i}$ for all observations, and \code{cv_err} which
#'   is a numeric with the cross-validation misclassification error
#'
#' @example
#' k_1 <- my_knn_cv(penguins[,3:6], penguins$species, 1, 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {

  #Split data in k_cv parts, randomly
  fold <- sample(rep(1:k_cv, length = nrow(penguins)))
  x <- penguins %>% select(bill_length_mm, bill_depth_mm, flipper_length_mm, body_mass_g)
  new_data <- data.frame("x" = x, "y" = cl, "split" = fold)

  # Empty matrix to store predictions, 2 columns for 2 models
  pred_mat <- matrix(NA)
  error_vec = rep(NA, k_cv)

  for(i in 1:k_cv) {
    data_train <- new_data %>% filter(split != i)
    data_test <- new_data %>% filter(split == i)

    cl_train <- as.factor(data_train$y)
    cl_test <- as.numeric(data_test$y)
    predictation <- knn(data_train[,1:4], data_test[,1:4], cl_train, k_nn)
    error_vec[i] <- mean(data_test$y != predictation)

  }

  class <- knn(train = new_data[,1:4], test = new_data[,1:4], cl, k = k_nn)
  cv_err <- mean(error_vec)
  return (list("class" = class, "cv_err" = cv_err))
}
