# log-likelihood in terms of modified bessel function of the first kind
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

# returns the monthly payment required to pay off the mortgage in the specified time (given in years)
# interest is annualized and downpayment is as a percentage, assumes monthly interest and payments
payment <- function(principal=500000, rate=0.05, downpayment=0.05, time = 25) {
  return((rate/12)*((1+rate/12)^(12*time))*(principal-downpayment*principal)/((1+rate/12)^(12*time)-1))
}

# returns a vector with the monthly payments required to pay off the mortgage in the specified time
# rates is a vector of annualized interest rates for each month in a sequence 
# downpayment is given as a percentage
variable_payment <- function(principal=500000, downpayment=0.05, time=25, rates) {
  n = 12*time # time in months
  payments = vector(length=n)
  amount_owed = principal - downpayment*principal # initial amount owed
  for (i in 1:n) {
    # calculate payment for month i so that mortgage is payed off in n-i+1 many months
    payment = (rates[i]/12)*((1+rates[i]/12)^(n-i+1))*(amount_owed)/((1+rates[i]/12)^(n-i+1)-1)
    # records payment on month i
    payments[i] = payment
    # updates new amount owed on month i
    amount_owed = amount_owed - payment + amount_owed*rates[i]/12
  }
  return(payments)
}
