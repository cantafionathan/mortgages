# load data
rates <- read.csv(file = "mortgages/canadarates.csv")
colnames(rates) <- c("date", "prime", "overnight")
rates <- tail(rates, -26)

r = rates$overnight/100
n = length(r)
dt <- 1

# predicting last n-1 rates based on the first n-1 to fit Vasicek Model

y = r[-1] 
x <- r[-n]

X <- matrix(c(rep(1, n-1), r[-n]), ncol=2)
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
alpha = (1 - a)/dt
theta = b/(1 - a)
sigma <- sqrt(MSE)/sqrt(dt)