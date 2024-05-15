library(ggplot2)
library(reshape2)

# returns a series of interest rates simulated according to a CIR Model
# dr = alpha*(theta - r)*dt + sigma*sqrt(r)*dW
# r is interest rate
# alpha is speed of reversion to the mean
# theta is long-term mean
# sigma is volatility
# dW is random noise/shock term
dt <- 1 # time step
cir <- function(alpha, theta, sigma, steps) {
  dt <- 1 # time step
  
  r <- vector(length=steps)
  r[1] <- theta # initial interest rate
  
  
  
  for (i in 2:steps) {
    dW <- rnorm(1, mean=0, sd=1) # generate random noise
    
    r[i] <- abs(r[i-1] + alpha*(theta - r[i-1])*dt + sigma*sqrt(r[i-1]*dt)*dW)
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

# predicting last n-1 rates based on the first n-1 to fit CIR Model

x <- r[-n]
y = r[-1]/sqrt(x)

X <- matrix(c(x/sqrt(x), rep(1, n-1)/sqrt(x)), ncol=2)
B <- solve(t(X)%*%X)%*%t(X)%*%y

e <- y - X%*%B # for calculating standard error

# r[t]/sqrt(r[t-1]) ~ a*r[t-1]/sqrt(r[t-1]) + b*(1/sqrt(r[t-1])) + ep
# a = (1 - alpha*dt)
# b = alpha*theta*dt
# ep = sigma*dW

a <- B[1]
b <- B[2]
MSE <- (t(e)%*%e)[1,1]/(n-3)




# initial parameters from least squares regression
alpha = (1 - a)/dt # Long-term mean or equilibrium interest rate
theta = b/(1 - a) # Speed of mean reversion
sigma <- sqrt(MSE)/sqrt(dt) # Volatility





# Now refine the value of parameters with ML estimation
# function to calculate the ln Likelihood for CIR Model
lnLhelper <- function(param, data, dt) {
  n = length(data)
  dataF <- data[-1] # data from 2:n
  dataL <- data[-n] # data from 1:n-1
  
  # parameter values
  alpha <- param[1]
  theta <- param[2]
  sigma <- param[3]
  
  # ## calculate likelihood (Bessel)
  c = 2*alpha/(sigma^2*(1 - exp(-alpha*dt)))
  q = 2*alpha*theta/sigma^2 - 1
  u = c*exp(-alpha*dt)*dataL
  v = c*dataF
  z = 2*sqrt(u*v)
  bf = besselI(z, q, T) # scaled modified bessel function of the first kind

  lnL = -(n-1)*log(c) + sum(u + v - 0.5*q*log(v/u) - log(bf) - z)
  
  # calculate likelihood (Chi-Sq)
  # c = 2*alpha/(sigma^2*(1 - exp(-alpha*dt)))
  # q = 2*alpha*theta/sigma^2 - 1
  # u = c*exp(-alpha*dt)*dataL
  # v = c*dataF
  # s = 2*c*dataF
  # nc = 2*u # non-centrality
  # df = 2*q + 2 # degrees of freedom
  # 
  # gpdf = dchisq(s, df = df, ncp = nc)
  # ppdf = 2*c*gpdf
  # lnL = sum(-log(ppdf))
   
  return(lnL)
}

# returns the ln Likelihood function for the CIR Model
# enter the data and time step inside the function definition
logLikelihood <- function(param) {
  result <- lnLhelper(param=param, data=r, dt=1)
  return(result)
}



# optimize using optim()
# for example call optim(c(alpha, theta, sigma), logLikelihood)
# results for Canada overnight rate from January 1960 - March 2024 have been saved in cirParameters.csv

parameter_data <- read.csv(file="mortgages/cirParameters.csv", row.names = 1)






# Simulate the rates
# Set number of simulation steps
steps <- 300

param <- parameter_data$mleBessel

# Perform CIR Monte Carlo simulation with standard normal shocks
cir_rates <- cir(param[1], param[2], param[3], steps)

# Create time vector
time <- 1:steps

# Create data frame to store trajectories
trajectories <- as.data.frame(c(time))

# Choose how many simulated trajectories to generate 
N <- 5

# Run loop to generate N different simulated trajectories

for (i in 1:N) {
  trajectories <- cbind(trajectories, cir(param[1], param[2], param[3], steps))
}
colnames(trajectories) <- c('time', paste("sim", 1:N, sep = ""))
trajectories <- melt(trajectories, id.vars = 'time', variable.name = 'sim')


ggplot(data = trajectories, aes(time, value)) +
  geom_line(aes(colour = sim)) +
  geom_hline(yintercept = param[2], linetype = "dashed") +
  xlab("Time") +
  ylab("Interest Rate") +
  ggtitle("CIR Interest Rate Monte Carlo Simulation") +
  theme_bw()


