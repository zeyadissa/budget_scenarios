source('src/functions.R')

#deflator data
base_year <- 2018
base_growth <- 0.03
max_year <- 2035

FINAL_deflator <- read.csv('const/FINAL_deflator.csv') %>%
  select(!X) 

splits <- read.csv('const/splits.csv') %>%
  select(!X) %>%
  mutate(r = case_when(type=='specialised' ~ 0,
                       T~ 1-(w+d)))

#activity growth data
data_final_a <- read.csv('const/final_data.csv') %>%
  select(!X) %>%
  filter(fyear <= max_year) %>%
  CreatePolicy(.,
               model_filter='Linear growth',
               type_name = 'A&E',
               value = (1.8*1e9)) 

#data
data_final_a <- rbind(data_final_a,
              data_final_a %>%
                filter(type == 'Outpatients') %>%
                filter(models == 'Morbidity') %>%
                mutate(models = 'Policy: Recovery',
                       modelled_growth = 1.032^(fyear-2018))
                        )

#policy recovery
data_final_a <- rbind(data_final_a,
                      data_final_a %>%
                        filter(type == 'Elective') %>%
                        filter(models == 'Morbidity') %>%
                        mutate(models = 'Policy: Recovery',
                               modelled_growth = 1.032^(fyear-2018))
)

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

#These are the controls for the zoom: they are needed to set the boundary to UK
THEME <- 'simplex'
min_date <- 2018
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
  select(fyear,final_value,index,budget_index) %>%
  filter(fyear <= max_year)
