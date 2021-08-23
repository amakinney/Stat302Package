#' Penguin body mass predictor
#' 
#' This function predicts the penguins body mass based on physical trait inputs.
#' 
#' @param k Numeric input representing the number of folds.
#' 
#' @return Numeric output representing the cv error.
#' 
#' @examples 
#'  my_rf_cv(5)
#'  
#' @export
my_rf_cv <- function(k) {
  
  folds <- sample(rep(1:k, length = nrow(data_cleaned)))
  
  for (i in 1:k) {
    
    data_training <- data_cleaned %>% filter(folds != i)
    data_testing <- data_cleaned %>% filter(folds == i)
    
    modele <- randomForest(body_mass_g ~ bill_length_mm + bill_depth_mm + 
                             flipper_length_mm, data = data_training, ntree = 100)
    
    predic <- predict(modele, data_testing[, -1])
    
    MSE <- (predic - data_cleaned$body_mass_g[i])^2
  }
  
  avg_MSE <- sum(MSE) / k
  
  return(avg_MSE)
  
}