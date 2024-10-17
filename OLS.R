# load data
source("transformcv.R")
canadarates <- read.csv(file = "canadarates.csv")
colnames(canadarates) <- c("date", "prime", "overnight")
canadarates <- tail(canadarates, -26)
dates <- as.Date(canadarates$date, format = "%d %B %Y")
canadarates$date <- dates

# fits an OLS model that predicts interest rates t months into the future and plots the predictions
#' @param t should be divisible by 6
OLS_t <- function(t, rate = canadarates$overnight, date = canadarates$date) { #
  #fit the model that predicts t months into future
  n = length(rate)
  date = date[(t+1):(n-t)]
  `rate+0` = rate[(t+1):(n-t)]
  `rate+t` = rate[(2*t+1):n]
  rates <- cbind.data.frame(date, `rate+t`, `rate+0`)
  # rate-t, ... rate-(t-6), ... rate-6
  for (i in 1:(t/6)) {
    rates <- cbind.data.frame(rates, rate[(1 + 6*(i-1)):(n - 2*t+6*(i-1))])
  }
  colnames(rates) <- c('date', 'rate+t', 'rate+0', paste("rate-", seq(t, 6, by=-6), sep=""))
  
  n <- length(rates$date)
  rates_test <- rates[(n-t+1):n,]
  rates <- rates[(1:(n-t)),]
  
  # use cross validation to select lambda
  lambda = seq(0, 20, by = 0.5)
  cv <- transform_cv(data = rates, lambda, kfolds = 10, t = t)
  lambda = lambda[which(cv == min(cv))]
  
  # transform data
  tr <- function(d) {
    log(lambda + d)
  }
    # train data
  rates_tr <- rates %>%
    mutate(across(`rate+t`:`rate-6`, log))
    # test data
  rates_test_tr <- rates_test %>%
    mutate(across(`rate+t`:`rate-6`, log))
  
  # fit model
  model <- lm(`rate+t` ~. -date, data = rates_tr)
  
  # plot fitted model + future predictions + actual values
  pred = data.frame(date = rates_test$date, 
                    predictions = exp(predict(model, rates_test_tr)))
  fit = data.frame(date = rates_tr$date, fits = (exp(model$fitted.values)))
  
  ggplot() +
    geom_point(data = rates, mapping = aes(x = date, y = `rate+0`)) +
    geom_point(data = rates_test, mapping = aes(x = date, y = `rate+0`), colour = "gray") +
    geom_line(data = pred, mapping = aes(x = date, y = predictions), colour = "purple") +
    geom_line(data = fit, mapping = aes(x = date, y = fits), colour = "blue") +
    labs(title = paste("Predicting", t/12, "years into the future")) +
    theme_bw()
} #