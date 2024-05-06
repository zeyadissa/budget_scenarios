CreateCostIndex <- function(pay_name,prod_name,deflator_name,custom_prod,custom_pay){
  
  #Read all the tables
  FINAL_all_pay <- data.table::fread('const/FINAL_all_pay') %>%
    select(-1)
  FINAL_productivity <- data.table::fread('const/FINAL_productivity') %>%
    select(-1)
  FINAL_deflator <- data.table::fread('const/FINAL_deflator') %>%
    select(-1)
  
# Cost index
cost_index_1 <- list(FINAL_all_pay, FINAL_productivity, FINAL_deflator) %>%
  reduce(full_join, by='fyear')

# High wage scenario
# example: high wage = 2% real wage growth
# example: low wage = -1% real wage growth (decline)

cost_index_1$nhs_pay_high <- cost_index_1$deflator + 2
cost_index_1$nhs_pay_low <- cost_index_1$deflator - 1
cost_index_1 <- cost_index_1 %>%
  filter(fyear>2017 & fyear <2036)

cost_index<- cost_index_1 %>%
  rename('central_nhs_pay' = nhs_pay,
         'min_nhs_pay' = nhs_pay_low,
         'max_nhs_pay' = nhs_pay_high) %>%
  mutate(VAL_pay = get(pay_name),
         VAL_prod = get(prod_name),
         non_deflator = 1,
         VAL_deflator = get(deflator_name))
  

for (i in seq_along(cost_index$fyear)) {
  
  if (i == 1) {
    
    # The first value should be 100
    cost_index$VAL_index[i] <- 100                    
  }
  
  if (i > 1) {
    
    # Consequent values [i] depend on the lagged value [i-1]
    cost_index$VAL_index[i] <- (cost_index$VAL_index[i-1] * (1+((cost_index$VAL_pay[i]-cost_index$VAL_deflator[i])/100))*0.73) +
      (cost_index$VAL_index[i-1] * (1-0.73)) *
      (1-(cost_index$VAL_prod[i]/100))
    
  }
}

#custom
for (i in seq_along(cost_index$fyear)) {
  
  if (i == 1) {
    
    # The first value should be 100
    cost_index$VAL_custom_index[i] <- 100                    
  }
  
  if (i > 1) {
    
    # Consequent values [i] depend on the lagged value [i-1]
    cost_index$VAL_custom_index[i] <- 
      (cost_index$VAL_custom_index[i-1] * (1+((custom_pay-cost_index$VAL_deflator[i])/100))*0.73) +
      (cost_index$VAL_custom_index[i-1] * (1-0.73)) *
      (1-(custom_prod/100))
    
  }
}

cost_index<- cost_index %>%
  dplyr::select(fyear,VAL_deflator,VAL_index,VAL_prod,VAL_pay,VAL_custom_index)

return(cost_index)

}
