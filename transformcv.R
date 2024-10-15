#' @param estimator Function. Has 1 argument (some data) and fits a model. 
#' @param predictor Function. Has 2 args (the fitted model, the_newdata) and
#'   produces predictions
#' @param error_fun Function. Has one arg: the test data, with fits added.
#' @param kfolds Integer. The number of folds.
kfold_cv <- function(data, estimator, predictor, error_fun, kfolds = 5, lambda) {
  n <- nrow(data)
  fold.labels <- sample(rep(1:kfolds, length.out = n))
  errors <- double(kfolds)
  for (fold in 1:kfolds) {
    test.rows <- fold.labels == fold
    train <- data[!test.rows, ]
    test <- data[test.rows, ]
    current_model <- estimator(train, lambda)
    test$.preds <- predictor(current_model, test)
    errors[fold] <- error_fun(test)
  }
  mean(errors)
}

# cross validation for choosing lambda for transformation
# (horribly designed)
transform_cv <- function(data, lambda = 1:50, kfolds = 5) {
  tr <- function(d, lambda) {
    log(lambda + d)
  }
  
  estimator <- function(data, lambda) {
    data_tr <- data %>%
      mutate(across(`rate-12`:`rate+6`, ~ tr(., lambda)))
    lm(`rate+6` ~ `rate+0` + `rate-6` + `rate-12`, data = data_tr)
  }
  
  error_fun <- function(testData){
    mean(abs(testData$`rate+6` - testData$.preds))
  }
  
  n <- length(lambda)
  CV <- vector(length=n)
  for (i in 1:n) {
    CV[i] = kfold_cv(rates, estimator, predict, error_fun, kfolds = kfolds, lambda = lambda[i])
  }
  
  CV
}