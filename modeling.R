loan_amount <- 200000
interest_rate <- 0.05
loan_term_years <- 30
monthly_interest_rate <- interest_rate / 12
num_payments <- loan_term_years * 12
monthly_payment <- loan_amount * monthly_interest_rate *
  (1+ monthly_interest_rate)^ num_payments / 
  ((1 + monthly_interest_rate)^num_payments -1)

amortization_schedule <- data.frame(
  Month = 1: num_payments, Payment = rep(round(monthly_payment, 2), num_payments),
  Interest = numeric(num_payments),
  Pricipal = numeric(num_payments),
  Balance = numeric(num_payments)
)

amortization_schedule$Interest[1] <- round(amortization_schedule$Payment[1] - 
                                             amortization_schedule$Pricipal[1],2)
amortization_schedule$Pricipal[1] <- round(amortization_schedule$Payment[1] - 
                                             amortization_schedule$Payment[1]- 
                                             amortization_schedule$Interest[1],2)
amortization_schedule$Balance[1] <- round(loan_amount - 
                                            amortization_schedule$Pricipal[1],2)

for ( i in 2: num_payments) {
  amortization_schedule$Interest[i] <- round(amortization_schedule$Balance[i-1] * 
                                               monthly_interest_rate,2)
  amortization_schedule$Pricipal[i] <- round(amortization_schedule$Payment[i] - 
                                               amortization_schedule$Interest[i], 2)
  amortization_schedule$Balance[i] <- round(amortization_schedule$Balance[i-1] - 
                                              amortization_schedule$Pricipal[i],2)
}

print(amortization_schedule)

library(ggplot2)
#interest vs principal
ggplot(amortization_schedule,aes(x = Month)) + 
  geom_line(aes(y = Interest, colour = "Interest")) +
  geom_line(aes(y = Pricipal, color = "Principal")) +
  labs(title = "Monthly Payment Breakdown", x = "Month", y = "Amount") +
  scale_colour_manual(NULL)


### OUTSTANDING BALNCE OVER TIME
ggplot(amortization_schedule, aes(x = Month, y = Balance)) +
  geom_line() +
  labs(title = "Outstanding Balance Over Time", x = "Month", y = "Balance")



##CUMULATIVE INTERST PAID
ggplot(amortization_schedule, aes(x = Month, y = cumsum(Interest))) +
  geom_line() +
  labs(title = "Cumulative Interst Paid", x = "Monthh", y = "Cumulative Interest")


#TOTAL AMOUNT PAID ALL IN ALL
total_amount_paid <- sum(amortization_schedule$Payment)
print(paste("Total amount paid:", round(total_amount_paid, 2)))


#REINANCCING THE LOAN
current_loan_amount <- 200000
current_interest_rate <- 0.05       
current_loan_term_years <- 25
#Refinancing terms
new_interest_rate <- 0.04
new_loan_term_years <- 25
refinancing_costs <- 5000


#calculate the current monthhly payment
current_monthly_payment <- current_loan_amount * (current_interest_rate/12) * 
  (1+ current_interest_rate/12)^ (current_loan_term_years*12) / 
  ((1 +current_interest_rate/12)^(current_loan_term_years*12)-1)

#Calculate the new monthhly payment
new_monthly_payment <- current_loan_amount* 
  (new_interest_rate/12)*(1+new_interest_rate/12)^(new_loan_term_years*12) / 
  ((1+new_interest_rate/12)^(new_loan_term_years*12)-1)

savings <- current_monthly_payment - new_monthly_payment
#calculate break even point
break_even_point <- refinancing_costs/savings

print(paste("Current Monthly Payment:", round(current_monthly_payment,2)))
print(paste("new_monthly_payment:",round(new_monthly_payment,2)))
print(paste("Monthly payment Savings:", round(savings,2)))
print(paste("break-even point(months):", round(break_even_point,2)))


#Create a data frame for plotting
plot_data <- data.frame(
  Month = 1:(current_loan_term_years * 12),
  Current_Payment = rep(current_monthly_payment, current_loan_term_years * 12),
  New_Payment = rep(new_monthly_payment, current_loan_term_years * 12)
)

#Plot the monthly payments
ggplot(plot_data, aes(x = Month)) +
  geom_line(aes(y = Current_Payment, color = "Current Payment")) +
  geom_line(aes(y = New_Payment, color = "New Payment")) +
  labs(title = "Monthly Payments", x = "Month", y = "Payment Amount") +
  scale_color_manual(values = c("blue", "red"))

#Plot the cumulative savings
cumulative_savings <- cumsum(rep(savings, current_loan_term_years * 12))
plot_data$cumulative_savings <- cumulative_savings

ggplot(plot_data, aes(x = Month)) +
  geom_line(aes(y = cumulative_savings)) +
  labs(title = "Cumulative Savings", x = "Month", y = "Savings Amount") +
  geom_vline(xintercept = break_even_point, color = "red", linetype = "dashed") +
  annotate("text", x = break_even_point, y = max(cumulative_savings) * 0.8, 
           label = paste("Break-Even Point (", round(break_even_point, 2), " months)"))

