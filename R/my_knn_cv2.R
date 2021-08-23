#' Output class predictor
#' 
#' This function predicts the species of penguins based on the input data.
#' 
#' @param train The data frame inputted.
#' @param cl The true class of the training data.
#' @param k_nn Numeric input representing the number of neighbors.
#' @param k_cv Numeric input representing the number of folds. 
#' 
#' @return A list with a vector of predicted classes and a numeric value with
#'   the predicted misclassification rate. 
#' 
#' @examples 
#'  my_knn_cv(data_use, data_clean$species, k_nn = 1, k_cv = 5)
#'  
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  
  data_frame <- data.frame("train" = train, "cl" = cl, "folds" = fold)
  
  for (i in 1:k_cv) {
    data_train <- data_frame %>% filter(folds != i)
    data_test <- data_frame %>% filter(folds == i)
    data_train_fil <- data_train %>% select(-folds, -cl)
    data_test_fil <- data_test %>% select(-folds, -cl)
    cl <- data_train %>% pull(cl)
    
    temp <- knn(data_train_fil, data_test_fil, cl, k_nn)
    
    misclass <- mean(cl != temp)
    
  }
  
  cv_err <- mean(misclass)
  
  
  output <- list("class" = class, "cv_err" = cv_err)
  
  return(output)
}
