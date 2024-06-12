CreatePolicy <- function(dataset,model_filter,type_name,value){
  bind <- dataset %>%
    filter(type == type_name) %>%
    filter(models == model_filter) %>%
    mutate(models = 'Policy: Recovery') %>%
    mutate(modelled_growth = ((modelled_growth) * (baseline + value))/baseline)
  output <- rbind(dataset,bind)
}

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

CreateCommunityMHData <- function(df,growth){
  df %>%
    rowwise() %>%
    mutate(
      modelled_growth = 
        case_when(type %in% c('Community','Mental Health','IAPT') & models %in% c('Policy: Recovery') ~ 
                         (get(growth)) + 0.05,
                       T ~ modelled_growth))
  
}
