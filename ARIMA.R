# What follows is trying to use Box-Jenkins (ARIMA models) to predict interest rates
# this is a bad approach. It's not clear that interest rates should have a seasonal effect,
# and even if they do, it would likely be ove a year: 
#                       and a weekly frequency is too long a period over a year.


# canadarates <- read.csv(file = "canadarates")
# colnames(canadarates) <- c("date", "prime", "overnight")
# canadarates <- tail(canadarates, -26)
# dates <- base::as.Date(canadarates$date, format = "%d %B %Y")
# canadarates$date <- dates

## Predicting interest rates with Box-Jenkins

# This leads us to the Box-Jenkins Method. 

#Essentially we look at some characteristics of the data, and try identify a decent model. We then perform parameter estimation for this model and finally evaluate the performance of the model. We can repeat this process as necessary to get a model that predicts well.

# We will use the `zoo` package to handle the time series data. 

# It's clear from looking at the plot of the rate that the data isn't stationary. And the autocorrelation plot agrees, as it decays very slowly. We can try log-differencing the data to see if that helps and indeed it does.

usrates <- read.csv(file = "MORTGAGE30US.csv")
colnames(usrates) <- c("date", "rate")
usrates$date <- base::as.Date(usrates$date, format = "%Y-%m-%d")

# fix data so it is evenly spaced
# most of the data is every 7 days, but some of it is a difference of 5, 6, 8, or 9. 
# If one of those days is Oct-19 and the previous observation is on Oct-14, 
# I would change it to Oct-21 and assume that the average mortgage is the same 
# on both of these days. There are so few of these days in the data that I don't 
# think it will affect the analysis
n = length(usrates$date)
for (i in 2:n) {
  usrates$date[i] = usrates$date[i - 1] + 7
}

library(zoo)
rawrates <- zoo(usrates$rate, order.by = usrates$date)

n = length(rawrates)

rawACF <- acf(rawrates, plot = FALSE, lag.max = 60)
rawACFplot <- ggplot(data = with(rawACF, data.frame(lag, acf)), 
                     mapping = aes(x = lag, y = acf)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  geom_hline(aes(yintercept = 1.96/sqrt(n)), linetype = "dashed", colour = "blue") +
  geom_hline(aes(yintercept = -1.96/sqrt(n)), linetype = "dashed", colour = "blue") +
  labs(title = "Raw ACF plot") +
  theme_bw()
rawPACF <- pacf(rawrates, plot = FALSE, lag.max = 60)
rawPACFplot <- ggplot(data = with(rawPACF, data.frame(lag, acf)), 
                      mapping = aes(x = lag, y = acf)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_hline(aes(yintercept = 1.96/sqrt(n)), linetype = "dashed", colour = "blue") +
  geom_hline(aes(yintercept = -1.96/sqrt(n)), linetype = "dashed", colour = "blue") +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  labs(title = "Raw PACF plot") +
  theme_bw()

t <- 6 # how far ahead to do we want to predict
rates_test <- window(rawrates, start = usrates$date[2794 - t + 1], end = usrates$date[2794])
rates_train <- window(rawrates, start = usrates$date[1], end = usrates$date[2794 - t])

s <- 1 # difference period
rates_diff <- diff(log(rates_train), lag = s)
n = length(rates)

ACF <- acf(rates_diff, plot = FALSE, lag.max = 100)
ACFplot <- ggplot(data = with(ACF, data.frame(lag, acf)), 
                  mapping = aes(x = lag, y = acf)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  geom_hline(aes(yintercept = 1.96/sqrt(n)), linetype = "dashed", colour = "blue") +
  geom_hline(aes(yintercept = -1.96/sqrt(n)), linetype = "dashed", colour = "blue") +
  labs(title = "Differenced ACF plot") +
  theme_bw()
PACF <- pacf(rates_diff, plot = FALSE, lag.max = 300)
PACFplot <- ggplot(data = with(PACF, data.frame(lag, acf)), 
                   mapping = aes(x = lag, y = acf)) + 
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  geom_hline(aes(yintercept = 1.96/sqrt(n)), linetype = "dashed", colour = "blue") +
  geom_hline(aes(yintercept = -1.96/sqrt(n)), linetype = "dashed", colour = "blue") +
  labs(title = "Differenced PACF plot") +
  theme_bw()

cowplot::plot_grid(rawACFplot, rawPACFplot, nrow=2)
cowplot::plot_grid(ACFplot, PACFplot, nrow=2) #MA1?


# From these plots, the ACF has significant spikes up to lag 4, and the PACF seems to trail off without a clearly discernible pattern. Therefore I would suggest an $\text{ARIMA}(p, 1, q=4)$ model. But checking other values of $q$ could also be useful. We can try different values of $p$ and $q$ and select a model based on AIC.

auto.arima(rates_train)

# So it seems like an $\text{ARIMA}(4, 1, 4)$ model will be appropriate

# A disadvantage of this method is that there isn't really a programatic way of predicting future rates given some history. We need to redo the entire analysis.

model <- arima(rates_train, order = c(2, 0, 2))

predrates <- predict(model, t)
lb = predrates$pred - 1.96*predrates$se
ub = predrates$pred + 1.96*predrates$se

predictions = data.frame(date = usrates$date[(2794-t+1):2794], pred = predrates$pred)

ggplot() +
  geom_point(data = usrates[(2794-t):2794,], mapping = aes(x = date, y = rate), colour = "black") +
  geom_line(data = predictions, mapping = aes(x = date, y = pred), colour = "purple") +
  labs(title = "ARIMA predictions") +
  theme_bw()
