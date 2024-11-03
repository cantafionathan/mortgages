source("mortgagescripts.R")
# principal = initial value of property
# time = number of years for mortgage
# rates = interest rate over time
# beta = annual appreciation of investment (i.e. stocks/bonds)
# alpha = annual appreciation of property value
# lease_term = length of the lease term (i.e. how often should price of rent be updated)
# step = step between ratios we test
#
difference <- function(principal = 500000, time = 25, rates, beta = 0.05,
                       alpha = 0.02, lease_term = 12, step=0.1, max_ratio=1.2,
                       min_ratio=0.5) {
  stopifnot(length(rates) == time*12)
  n = time*12 # number of months
  r = 1 + beta/12 # monthly appreciation of investment
  s = 1 + alpha/12 # monthly appreciation of real estate
  
  # calculate the mortgage payments over time
  var_payment = variable_payment(principal = principal, rates = rates, time=time)
  # total cost of mortgage = sum of all the payments
  mortgage_cost = sum(var_payment)
  
  # get adjusted value of home over 25 years based on 2% appreciation per year
  principal = rep(principal, n)
  for (i in 1:n) {
    principal[i] = principal[i]*s^(i-1)
  }
  
  # calculate the baseline rent for every lease term
  # by finding what the fixed rate mortgage would be
  # for adjusted property value
  baseline_rent = payment(principal = principal, rate = rates, time = time)
  baseline_rent = baseline_rent[c(TRUE,rep(FALSE,lease_term - 1))]
  rep_baseline <- c()
  for(w in 1:length(baseline_rent)) {
    rep_baseline <- rbind(rep_baseline, rep(baseline_rent[w], lease_term))
  }
  
  
  m = (max_ratio - min_ratio)/step + 1 # m = number of different rents we test
  result = vector(length = m)
  
  for (j in 1:m) {
    rental_payment = rep_baseline*(min_ratio+step*(j-1)) # calculate monthly rent for each month
    
    # calculate how much money is made by investing what we would have payed towards the mortgage
    investment_income = 0
    for (k in 1:n) {
      investment_income = investment_income*r + var_payment[k] - rental_payment[k]
    }
    
    # calculate the cost of the rental by taking sum of rent payments
    # and subtracting profits from investment
    rental_cost = sum(rental_payment) - investment_income
    
    result[j] = mortgage_cost - rental_cost
  }
  rent_ratios <- seq(min_ratio, max_ratio, step)
  result <- data.frame(as.list(result))
  colnames(result) <- rent_ratios
  return(result)
}