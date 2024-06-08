source('src/functions.R')
source('src/base_data.R')

#deflator data
base_year <- 2018
base_growth <- 0.03
max_year <- 2035
THEME <- 'simplex'
min_date <- 2018
base_font <- 14

shock_types <- c('Permanent','U-Shaped')

growth_scenarios <- read.csv('const/growth_scenarios.csv') %>%
  group_by(scenario) %>%
  mutate(
  prod = case_when(
    is.na(val_prod/lag(val_prod)) == T ~ 1,
    T ~val_prod/lag(val_prod)),
  pay = case_when(
    is.na(val_pay/lag(val_pay)) == T ~ 1,
    T ~ val_pay/lag(val_pay))
  ) %>%
  select(!c(val_prod,val_pay))

data_final_a <- data_final_a %>%
  filter(fyear <= max_year)

FINAL_deflator <- read.csv('const/FINAL_deflator.csv') %>%
  select(!c(deflator,X)) 

test <- data_final_a %>%
  select(type,models) %>% 
  mutate(type=gsub(pattern=' ',replacement='_',x=type),
         type = gsub(pattern='&',replacement='',x=type),
         type = tolower(type))%>%
  unique()

#population data
pop_final <- read.csv('const/FINAL_POP.csv') %>%
  select(!c(X,metric))

#supplementary gdp data
supplement <- read.csv('const/supplementary_data.csv') %>%
  mutate(baseline_gdp = (read.csv('const/supplementary_data.csv') %>% filter(fyear==2018))$real_gdp,
         baseline_receipts = (read.csv('const/supplementary_data.csv') %>% filter(fyear==2018))$real_receipts,
         gdp_index = real_gdp / baseline_gdp,
         receipt_index = real_receipts/baseline_receipts) %>%
  select(fyear,gdp_index,receipt_index)

#These are the controls for the zoom: they are needed to set the boundary to UK
water_type <- unique(data_final_a$type)
#this is the dumbest thing i've ever done but it works.
#note to self: this is ideally where purrr::walk should be used.
for(i in test$type){
  assign(x=paste0(i,'_activity_growth_type'),
         value=((test %>% filter(type==i))$models)[order(nchar(((test %>% filter(type==i))$models)), ((test %>% filter(type==i))$models),
                                                         decreasing = T)]
  )
}

data_baseline <- read.csv('const/baseline_data.csv') %>%
  select(fyear,final_value,index,budget_index) %>%
  filter(fyear <= max_year)
