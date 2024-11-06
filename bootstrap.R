source("difference.r")
library(tibble)
B = 2

# load data
usrates <- read.csv(file = "MORTGAGE30US.csv")
colnames(usrates) <- c("date", "rate")
usrates$date <- base::as.Date(usrates$date, format = "%Y-%m-%d")
r = usrates$rate/100
rates <- data.frame(date = usrates$date, rate = r)

# cir function
cir <- function(alpha, theta, sigma, steps, start = 0) {
  dt <- 1 # time step
  
  r <- vector(length=steps)
  r[1] <- start # initial interest rate
  # r[1] <- 5 # could choose any initial rate we want
  
  
  for (i in 2:steps) {
    dW <- rnorm(1, mean=0, sd=1) # generate random noise
    
    r[i] <- abs(r[i-1] + alpha*(theta - r[i-1])*dt +      # modified euler
                  sigma*sqrt(r[i-1])*dt*dW) 
  }
  
  return(r)
}

step = 0.05
max_ratio = 1
min_ratio = 0.6
beta = 0.05
alpha = 0.02

ratios <- seq(min_ratio, max_ratio, step)
boot_mean = matrix(nrow = B, ncol = length(ratios))
colnames(boot_mean) <- ratios

boot_prop = matrix(nrow = B, ncol = length(ratios))
colnames(boot_prop) <- ratios

for (b in 1:B) {
  #### simulate trajectories
  years = 25
  steps <- years*52.25 # each step should be thought of as 1 week so this is 25 years
  
  # choose which parameters to use
  param <- c(0.0006201719, 0.0720520244, 0.0039161539)
  
  # Create data frame to store trajectories
  trajectories <- tibble(date = rates$date[1:steps])
  
  # Choose how many simulated trajectories to generate 
  N <- 10000
  
  # Run loop to generate N different simulated trajectories
  for (i in 1:N) {
    trajectories <- cbind(trajectories, cir(alpha = param[1], 
                                            theta = param[2], 
                                            sigma = param[3], 
                                            steps, start = param[2]))
  }
  colnames(trajectories) <- c('date', paste("trajectory", 1:N, sep = "")) # rename columns
  trajectories$Month <- format(trajectories$date,format="%m")
  trajectories$Year <- format(trajectories$date,format="%Y")
  trajectories <- aggregate(. ~ Month + Year, trajectories, mean)
  trajectories$date <- as.Date(paste(trajectories$Year, "-", trajectories$Month, "-01", sep =""))
  trajectories <- trajectories[,-(1:2)]
  trajectories <- trajectories[(1:(years*12)),]
  
  #### compare renting to mortgaging
  step = 0.05
  max_ratio = 1
  min_ratio = 0.6
  
  results <- difference(rates = trajectories[,2], 
                        step = step,
                        alpha = alpha,
                        beta = beta,
                        max_ratio = max_ratio, 
                        min_ratio = min_ratio)
  for (i in 2:N) {
    results[nrow(results) + 1, ] = difference(rates = trajectories[,i+1], 
                                              step = step,
                                              alpha = alpha,
                                              beta = beta,
                                              max_ratio = max_ratio, 
                                              min_ratio = min_ratio)
  }
  
  count_mat <- results > 0
  count_mat[,1] <- as.numeric(count_mat[,1])
  count <- colSums(count_mat)
  names(count) <- NULL
  boot_prop[b,] <- count/N
  
  means <- colMeans(results)
  names(means) <- NULL
  boot_mean[b,] <- means
}
