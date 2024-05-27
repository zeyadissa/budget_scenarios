#deflator data
base_year <- 2018
base_growth <- 0.03
max_year <- 2040

FINAL_deflator <- read.csv('const/FINAL_deflator.csv') %>%
  select(!X)

splits <- read.csv('const/splits.csv') %>%
  select(!X) %>%
  mutate(r = case_when(type=='specialised' ~ 0,
                       T~ 1-(w+d)))

#activity growth data
data_final_a <- read.csv('const/final_data.csv') %>%
  select(!c(X))

test <- data_final_a %>%
  select(type,models) %>% 
  mutate(type=gsub(pattern=' ',replacement='_',x=type),
         type = gsub(pattern='&',replacement='',x=type),
         type = tolower(type))%>%
  unique()

#population data
pop_final <- read.csv('const/FINAL_POP.csv')

#supplementary gdp data
supplement <- read.csv('const/supplementary_data.csv') %>%
  mutate(baseline_gdp = (read.csv('const/supplementary_data.csv') %>% filter(fyear==2018))$real_gdp,
         baseline_receipts = (read.csv('const/supplementary_data.csv') %>% filter(fyear==2018))$real_receipts,
         gdp_index = real_gdp / baseline_gdp,
         receipt_index = real_receipts/baseline_receipts) %>%
  select(fyear,gdp_index,receipt_index)

#shock function
CreateShock <- function(df,shock_type,shock_range,val_prod,inten){
  
  if(shock_type == 'Permanent'){
    
    df2<-df %>% dplyr::mutate(
      prod = case_when(
        fyear >= min(shock_range) & fyear <= max(shock_range) ~ ((100 + val_prod)*inten)/10000,
        T ~ (100 + val_prod)/100))
    
    return(df2)
    
  } else if(shock_type == 'U-Shaped'){
    
    intensity_sta <- ((100 + val_prod) * inten)/100
    intensity_inter <- (100)/intensity_sta
    intensity_final <- intensity_inter^(1/(max(shock_range) - min(shock_range)))
    intensity_final <- ifelse(inten == 100, (100 + val_prod)/100,intensity_final)
    
    df2 <- df %>% dplyr::mutate(
      prod = case_when(
        fyear == min(shock_range)  ~ ((100 + val_prod)*inten)/10000,
        fyear > min(shock_range) & fyear <= max(shock_range) ~ intensity_final,
        T ~ (100 + val_prod)/100))
    
    return(df2)
  }
  return(df2)
  
}

CreateIndex <- function(w,d,r,prod,pay,drug){
  1 + (( (w*(pay-prod)) + (r*(drug-prod)) + (d*(1-prod) )))
}

CreatePayIndex <- function(w,d,r,prod,pay,drug){
  1+ (w*(pay-1))
}

CreateDrugIndex <- function(w,d,r,prod,pay,drug){
  1+ (r*(drug-1)) 
}

CreateCommunityMHData <- function(df){
  df %>% 
    mutate(modelled_growth = 
             case_when(type %in% c('Community','Mental Health') & models %in% c('Policy: Recovery') ~ 
                         modelled_growth + (1+base_growth)^(fyear-base_year) - (modelled_growth*index),
                       T ~ modelled_growth))
  
}

#These are the controls for the zoom: they are needed to set the boundary to UK
THEME <- 'simplex'
min_date <- 2018
max_date <- 2035
measures <- c('cost','activity')
activity_growth_type <- c('Constant','Log','Changing')
shock_types <- c('Permanent','U-Shaped')
water_type <- unique(data_final_a$type)
#this is the dumbest thing i've ever done but it works.
#note to self: this is ideally where purrr::walk should be used.
for(i in test$type){
  assign(x=paste0(i,'_activity_growth_type'),
         value=((test %>% filter(type==i))$models)[order(nchar(((test %>% filter(type==i))$models)), ((test %>% filter(type==i))$models),
                                                         decreasing = T)]
  )
}
base_font <- 14

data_baseline <- read.csv('const/baseline_data.csv') %>%
  select(fyear,final_value,index,budget_index)
