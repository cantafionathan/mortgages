# principal = initial value of property
# time = number of years for mortgage
# rates = interest rate over time
# investment_appreciation = annual appreciation of investment (i.e. stocks/bonds)
# property_appreciation = annual appreciation of property value
# lease_term = length of the lease term (i.e. how often should price of rent be updated)
# m = number of increments
difference <- function(principal = 500000, time = 25, rates, investment_appreciation = 0.06, property_appreciation = 0.02, lease_term = 12, m = 5) {
  
  result = vector(length=m)
  
  n = time*12 # number of months
  r = 1 + investment_appreciation/12 # monthly appreciation of investment
  s = 1 + property_appreciation/12 # monthly appreciation of real estate
  
  var_payment = variable_payment(principal = principal, rates = rates, time=time) # calculate the mortgage payments over time
  mortgage_cost = sum(var_payment) # total cost of mortgage = sum of all the payments
  
  principal = rep(principal, n)
  for (i in 1:n) {
    principal[i] = principal[i]*s^(i-1) # get adjusted value of home over 25 years based on 2% appreciation per year
  }
  
  baseline_rent = payment(principal = principal, rate = rates, time = time) # calculate the baseline rent for every lease term
  baseline_rent = baseline_rent[c(TRUE,rep(FALSE,lease_term - 1))]          # by finding what the fixed rate mortgage would be for adjusted
  rep_baseline <- c()                                                       # property value
  for(w in 1:length(baseline_rent)) {
    rep_baseline <- rbind(rep_baseline, rep(baseline_rent[w], lease_term))
  }
  
  increment = 1000/(m-1) # increment between each rent we test
  
  for (j in 1:m) {
    rental_payment = rep_baseline - increment*(j-1) # calculate monthly rent for each month

    investment_income = 0 # calculate how much money is made by investing what we would have payed towards the mortgage
    for (k in 1:n) {
      investment_income = investment_income*r + var_payment[k] - rental_payment[k]
    }
    rental_cost = sum(rental_payment) - investment_income # calculate the cost of the rental by taking sum of rent payments and
                                                          # subtracting profits from investment
    result[j] = mortgage_cost - rental_cost
  }
  return(result)
}