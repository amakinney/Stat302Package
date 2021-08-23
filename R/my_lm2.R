#' Linear model fucntion
#'
#' This function performs linear regression on given data and models.
#'
#' @param form the input formula.
#' @param data The inputted dataset.
#'
#' @keywords inference
#'
#' @return A tibble containing the estimate, standard error, t value, and p
#'     value for the intercept.
#'
#' @examples
#' my_lm(form, my_gapminder)
#'
#' @export
my_lm <- function(form, data) {

  X <- model.matrix(form, data)

  frame_object <- model.frame(form, data)

  Y <- model.response(frame_object)

  tx <- t(X)

  beta <- solve(tx %*% X) %*% tx %*% Y

  df <- nrow(X) - ncol(X)

  sig_sq <- sum((Y-(X %*% beta))^2 / df)

  se <- abs(diag(sqrt(sig_sq * solve(tx %*% X))))

  t_stat <- beta / se

  p_val <- 2 * (pt(abs(t_stat), df, lower.tail = FALSE))

  results <- (data.frame("Estimate" = beta,
                         "Std. Error" = se,
                         "t value" = t_stat,
                         "Pr(>|t|)" = p_val))

  rn <- rownames(beta)

  row.names(results) <- rn

  final_results <- as_tibble(results)

  return(final_results)
}
