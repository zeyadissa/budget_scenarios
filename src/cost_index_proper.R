
  #Read all the tables
  FINAL_all_pay <- data.table::fread('const/FINAL_all_pay') |>
    select(-1)
  FINAL_productivity <- data.table::fread('const/FINAL_productivity') |>
    select(-1)
  FINAL_deflator <- data.table::fread('const/FINAL_deflator') |>
    select(-1)
  
# Cost index
cost_index_1 <- list(FINAL_all_pay, FINAL_productivity, FINAL_deflator) |>
  purrr::reduce(full_join, by='fyear')

cost_index_1 <- cost_index_1 |>
  filter(fyear>2017 & fyear <2036) |>
  mutate(non_deflator = 1)

CreateIndex <- function(a,prod,pay,deflator){
  1 + a*(pay-deflator) - (1-a)*(prod-1)
}

CreateShock <- function(shock_type,e,intensity){
  if(shock_type == 'Permanent'){
    intensity * (1+e)^10
  } else if(shock_type == 'U-turn'){
    (100 - intensity)
  } else {
    
  }
}

pay <- 1.03
deflator <- 1.03
prod <- 1.03
a<-0.73
1 + a*(pay-deflator) - (1-a)*(prod-1)

dataset %>%
  rowwise() %>%
  mutate(t=CreateIndex(a=0.27,prod,pay,deflator))
