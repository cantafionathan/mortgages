# load data
source("transformcv.R")
canadarates <- read.csv(file = "canadarates.csv")
colnames(canadarates) <- c("date", "prime", "overnight")
canadarates <- tail(canadarates, -26)
dates <- as.Date(canadarates$date, format = "%d %B %Y")
canadarates$date <- dates

###--------------------------------------------------------------------------###
###-------------------------------CODE BLOCK 1-------------------------------###

# fits an OLS model that predicts interest rates t months into the future
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
  
  # use cross validation to select lambda
  lambda = seq(0, 20, by = 0.5)
  cv <- transform_cv(data = rates, lambda, kfolds = 10, t = t)
  lambda = lambda[which(cv == min(cv))]
  
  # transform data
  tr <- function(d) {
    log(lambda + d)
  }
  rates_tr <- rates %>%
    mutate(across(`rate+t`:`rate-6`, tr))
  
  # fit model
  model <- lm(`rate+t` ~. -date, data = rates_tr)
  
  # plot fitted model + future predictions + actual values
  someRates = tail(rates_tr, t)
  someDates = seq.Date(from = someRates$date[1], by ="month", length.out = 2*t)
  
  pred = data.frame(date = someDates, predictions = exp(predict(model, someRates)) - lambda,
                    row.names = NULL)
  ggplot() +
    geom_point(data = rates, mapping = aes(x = date, y = `rate+0`)) +
    geom_line(data = pred, mapping = aes(x = date, y = predictions), colour = "purple") +
    geom_line(data = data.frame(date = rates_tr$date, pred = (exp(model$fitted.values)-lambda)), mapping = aes(x = date, y = pred), colour = "blue") +
    labs(title = paste("Predicting", t/12, "years into the future")) +
    theme_bw()
} #