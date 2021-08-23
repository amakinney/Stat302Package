#' T-test Function
#' 
#' This function runs a single or double tailed t test on provided data. 
#' 
#' @param x Numeric vector the data is saved into. 
#' @param alternative Character string equal to "two.sided", "less", or 
#'   "greater".
#' @param mu Numeric input that xhat will be compared to. 
#' @return Returns a list of the t-value, degrees of freedom, alternative value,
#'   and p-value. 
#'   
#' @examples 
#' my_t.test(x, "two.sided", 0)
#'   
#' @export
my_t.test <- function(x, alternative = c("two.sided", "less", "greater"), mu) {
  
  xhat <- mean(x)
  
  n <- length(x)
  
  std_er <- sd(x) / sqrt(n)
  
  t_val <- (xhat - mu) / std_er
  
  deg_fr <- n - 1
  
  alternative_val <- alternative
  
  if(alternative == "two.sided") {
    p_value <- 2 * (pt(abs(t_val), deg_fr, lower.tail = FALSE))
  } else if(alternative == "less") {
    p_value <- pt(t_val, deg_fr, lower.tail = TRUE)
  } else if(alternative == "greater") {
    p_value <- pt(t_val, deg_fr, lower.tail = FALSE)
  } else {
    "Error: Alternative must be two sided, greater, or less."
  }
  
  final_list <- list("test_stat" = t_val,
                     "df" = deg_fr,
                     "alternative" = alternative_val,
                     "p_val" = p_value)
  
  return(final_list)
}