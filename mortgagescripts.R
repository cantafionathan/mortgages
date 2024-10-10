# For every function, all interest rates are annualized, and all payments are assumed to be monthly
# In fact in Canada, interest can be compounded separately from when it is payed. For example, it can be
# compounded semi-annually with monthly payments. At the end of the day, these differences are somewhat 
# superficial, at least that's the assumption of this code is
###--------------------------------------------------------------------------------------------------------------###
# returns the amount of time to pay off and the total cost of a mortgage 
# interest rate is annualized and downpayment is given as a percentage
time <- function(principal=500000, rate=0.05, downpayment=0.05, payment) {
  amount_owed = principal - downpayment*principal
  months = 1
  while (amount_owed > payment) {
    amount_owed = amount_owed - payment + amount_owed*rate/12
    months = months + 1
  }
  print(paste("Years:", months/12))
  print(paste("Cost:", months*payment + downpayment*principal))
}
###--------------------------------------------------------------------------------------------------------------###
# returns the monthly payment required to pay off the mortgage in the specified time (given in years)
# interest rate is annualized and downpayment is given as a percentage
# (note: this equation is not so bad to derive with some basic linear algebra to solve the recurrence relation)
payment <- function(principal=500000, rate=0.05, downpayment=0.05, time) {
  return((rate/12)*((1+rate/12)^(12*time))*(principal-downpayment*principal)/((1+rate/12)^(12*time)-1))
}
###--------------------------------------------------------------------------------------------------------------###
# returns a vector containing the monthly payments required to pay off the mortgage in the specified time (in years)
# rates is a vector of annualized interest rates for each month in a sequence 
# downpayment is given as a percentage
variable_payment <- function(principal=500000, downpayment=0.05, time=25, rates) {
  temp_time = 12*time # time in months
  payments = vector(length=temp_time)
  amount_owed = principal - downpayment*principal # initial amount owed
  for (i in 1:temp_time) {
    # calculate payment for month i so that mortgage is payed off in temp_time-i+1 many months
    payment = (rates[i]/12)*((1+rates[i]/12)^(temp_time-i+1))*(amount_owed)/((1+rates[i]/12)^(temp_time-i+1)-1)
    # records payment on month i
    payments[i] = payment
    # updates new amount owed on month i
    amount_owed = amount_owed - payment + amount_owed*rates[i]/12
  }
  return(payments)
}
###--------------------------------------------------------------------------###
# compare variable rate to fixed rate mortgage
compare <- function(principal=500000, downpayment=0.05, time=25, rates, plot = F) {
  months = 12*time
  
  # calculate the amount owed over time for fixed payment
  fixed_amount_owed <- vector(length = months + 2)
  fixed_amount_owed[1] = principal
  fixed_amount_owed[2] = principal - downpayment*principal
  rate = rates[1]
  payment = payment(principal, rate, downpayment, 25)
  for (i in 1:months) {
    fixed_amount_owed[i+2] = fixed_amount_owed[i+1] + fixed_amount_owed[i+1]*rate/12 - payment
  }
  
  # calculate the amount owed over time for variable payments
  variable_payments <- variable_payment(principal=principal, downpayment=downpayment, time=time, rates=rates)
  variable_amount_owed <- vector(length = months + 2)
  variable_amount_owed[1] = principal
  variable_amount_owed[2] = principal - downpayment*principal
  for (i in 1:months) {
    variable_amount_owed[i+2] = variable_amount_owed[i+1] + variable_amount_owed[i+1]*rates[i]/12 - variable_payments[i]
  }
  
  # return a dataframe with the amount owed over time
  data = data.frame(1:(months+2), fixed_amount_owed, variable_amount_owed)
  data <- setNames(data, c('time', 'fixed', 'variable'))
  
  cost <- data.frame(payment*months + principal*downpayment, sum(variable_payments) + principal*downpayment)
  cost <- setNames(cost, c('fixed', 'variable'))
  
  if (plot){
    
    data <- melt(data, id.vars = 'time', variable.name = 'Type')
    
    x <- ggplot(data = data, aes(time, value)) +
      geom_line(aes(colour = Type)) +
      xlab("Time") +
      ylab("Amount Owed") +
      ggtitle("Amount Owed Over Time") +
      theme_bw()
    print(x)
  }
  return(cost)
}
compare(rates = cir(param[1], param[2], param[3], 300), plot = T, downpayment =0.05)
