# may no need to simulate rental for a particular





# initially, I wanted to find a formula for the monthly rental cost based on the property value. This is harder to do 
# than I initially imagined due to differences in market conditions and property values based on location. 
# Now I am going to find data for the average property value and average rental cost to do my comparison. 
# I also like this because it accounts for potential differences in the types of properties available for renting 
# compared to properties available for buying. 

# load data for historical average rents in Vancouver, all years are as of October of that year
raw_rent <- read.table("mortgages/rentprices.txt", header=TRUE, sep="")

#load property value data for october of every year from 1990 to 2022
october_values <- read.csv(file = "mortgages/propertyvalues.csv")
october_values <- tail(october_values, -9)
october_values <- october_values[c(TRUE,rep(FALSE,11)), ]
october_values <- head(october_values, 33)
october_values <- as.numeric(gsub(",", "", october_values$Average))


# load interest rate data for october of every year from 1990 to 2022
october_rates <- read.csv(file = "mortgages/canadarates.csv")
colnames(october_rates) <- c("date", "prime", "overnight")
october_rates <- tail(october_rates, -395)
october_rates <- october_rates[c(TRUE,rep(FALSE,11)), ]
october_rates = october_rates$overnight/100

rent <- data.frame(
  date = head(raw_rent$Date, 33),
  price = head(raw_rent$Total, 33),
  interest_rate = head(october_rates, 33),
  value = october_values
)

# basic linear model to predict average rental price from the average home price + interest rate

m1 <- lm(rent$price ~ rent$value + seq(0,32)*rent$interest_rate)
summary(m1)





# ## Simulating Rental Rates
# 
# In order to compare mortgaging and renting, I need to figure out what the rental rate would be over time for a property with a given value. This way, I can use the simulated rates to calculate what a property would cost to own and also what it would cost to rent. 
# 
# Originally, I wanted to find a formula that took into account the interest rate, and the property value and spit out the rental rate. I googled for such a formula and found examples such as the 1% rule which says that the monthly rent should 1% of the principal value. The problem is that this rule doesn't work if the property value is inflated due to high demand induced by a housing shortage. In Vancouver for example today, following this rule would mean many houses would have monthly rents nearing tens of thousands of dollars.
# 
# The next thing that I tried was to find some data of average rental rates, and average property values over time in Vancouver. It would be nice if this data was price per square footage, or separated into categories by number of bedrooms but unfortunately I could not find such data. With only data on the total averages, it's hard to say with certainty that I'm not comparing apples to oranges.
# 
# In any case I used the data to fit a linear model predicting rental rate from property value, interest rates, and time. This surfaced more issues with my approach. Because interest rates have decreased while property value and rental rates have increased, there is a negative correlation between interest rates and rental rates. This is the opposite of what makes logical sense, and furthermore the term for interest rate in the model has low significance. So I think it makes sense to exclude.
# 
# ```{r, include=TRUE, echo=TRUE}
# # initially, I wanted to find a formula for the monthly rental cost based on the property value. This is harder to do 
# # than I initially imagined due to differences in market conditions and property values based on location. 
# # Now I am going to find data for the average property value and average rental cost to do my comparison. 
# # I also like this because it accounts for potential differences in the types of properties available for renting 
# # compared to properties available for buying. 
# 
# # load data for historical average rents in Vancouver, all years are as of October of that year
# raw_rent <- read.table("rentprices.txt", header=TRUE, sep="")
# 
# #load property value data for october of every year from 1990 to 2022
# october_values <- read.csv(file = "propertyvalues.csv")
# october_values <- tail(october_values, -9)
# october_values <- october_values[c(TRUE,rep(FALSE,11)), ]
# october_values <- head(october_values, 33)
# october_values <- as.numeric(gsub(",", "", october_values$Average))
# 
# 
# # load interest rate data for october of every year from 1990 to 2022
# october_rates <- read.csv(file = "canadarates.csv")
# colnames(october_rates) <- c("date", "prime", "overnight")
# october_rates <- tail(october_rates, -395)
# october_rates <- october_rates[c(TRUE,rep(FALSE,11)), ]
# october_rates = october_rates$overnight/100
# 
# rent <- data.frame(
#   date = head(raw_rent$Date, 33),
#   index = seq(1,33), # number of years that have passed
#   price = head(raw_rent$Total, 33),
#   interest_rate = head(october_rates, 33),
#   value = october_values
# )
# 
# #rent <- tail(rent, -13)
# #rent <- head(rent,15)
# 
# # basic linear model to predict average rental price from the average home price
# m1 <- lm(rent$price ~ rent$index)
# summary(m1)
# 
# # linear model to predict average rental price from the average home price + interest rate
# m2 <- lm(rent$price ~ rent$value + rent$interest_rate)
# summary(m2)
# 
# # linear model to predict average rental price from the average home price + interest rate + index
# m3 <- lm(rent$price ~ rent$value + rent$interest_rate + rent$index)
# summary(m3)
# 
# # linear model to predict average rental price from the average home price + index
# m4 <- lm(rent$price ~ rent$value + rent$index)
# summary(m4)
# 
# ```
# Since all of the p-values in model 4 are small and the R-squared value is large, I like model 4. It is also nice because it shows how the rent should change over time, crude though it may be. At the moment housing supply/demand is not accounted for at all in my model, which the interest rates would hopefully have been a proxy for. I would have preferred to have a way to incorporate my simulated interest rates into pricing the rental rates, but I would simply need more/better data in order to do so. 
# 
# Below is a function which returns the monthly rents according to my pricing model over a given time span and for
# 
# ```{r, include=TRUE, echo=TRUE}
# # value is the principal value of the property
# # time is the number of years over which we wish to find the monthly rent
# rental_rates <- function(value, time) {
#   rental_rates <- vector(length = time)
#   
#   for (t in 1:25) {
#     rental_rates[t] <- m4$coefficients[1] + m4$coefficients[2]*value + m4$coefficients[3]*t
#   }
#   
#   return(rental_rates)
# }
# ```
