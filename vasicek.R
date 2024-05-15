library(ggplot2)

# returns a series of interest rates simulated according to a Vasicek Model
# dr = alpha*(theta - r)*dt + sigma*dW
# r is interest rate
# alpha is speed of reversion to the mean
# theta is long-term mean
# sigma is volatility
# dW is random noise/shock term
dt <- 1
vasicek <- function(alpha, theta, sigma, steps) {
  dt <- 1 # time-step
  
  r <- vector(length=steps)
  r[1] = theta # initial interest rate
  
  for (i in 2:steps) {
    dW <- rnorm(1, mean=0, sd=sqrt(dt)) # generate random noise
    
    r[i] <- r[i-1] + alpha*(theta-r[i-1])*dt + sigma*dW # calculate next step
  }
  
  return(r)
}

#------------------------------------------------------------------------------#
# Set model parameters

# load data
rates <- read.csv(file = "mortgages/canadarates.csv")
colnames(rates) <- c("date", "prime", "overnight")
rates <- tail(rates, -26)

r = rates$overnight/100
n = length(r)

# predicting last n-1 rates based on the first n-1 to fit Vasicek Model

y = r[-1] 
x <- r[-n]

X <- matrix(c(rep(1, n-1), x), ncol=2)
B <- solve(t(X)%*%X)%*%t(X)%*%y

e <- y - X%*%B # for calculating standard error

# r[t] ~ a*r[t-1] + b + ep
# a = (1 - alpha*dt)
# b = alpha*theta*dt
# ep = sigma*dW

b <- B[1]
a <- B[2]
MSE <- (t(e)%*%e)[1,1]/(n-3)

#Vasicek parameters
alpha = (1 - a)/dt # Long-term mean or equilibrium interest rate
theta = b/(1 - a) # Speed of mean reversion
sigma <- sqrt(MSE)/sqrt(dt) # Volatility

# Set number of simulation steps
steps <- 300

# Perform Vasicek Monte Carlo simulation
vasicek_rates <- vasicek(alpha, theta, sqrt(sigma), steps)

# Create time vector
time <- 1:steps

# Plot simulated interest rates
plot_data <- data.frame(Time = time, Rate = vasicek_rates)
ggplot(plot_data, aes(x = Time, y = Rate)) +
  geom_line(color = ifelse(vasicek_rates < 0, "red", "blue")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = theta, linetype = "dashed") +
  xlab("Time") +
  ylab("Interest Rate") +
  ggtitle("Vasicek Interest Rate Monte Carlo Simulation") +
  theme_bw()