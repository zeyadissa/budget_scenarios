cost_weights <- read.csv('const/cost_weights.csv') %>%
  select(!X)

#activity growth data
data_final_a <- data.table::fread('const/final_data.csv') %>%
  mutate(modelled_growth = as.numeric(modelled_growth)) %>%
  filter(measure == 'cwa') %>%
  left_join(read.csv('const/FINAL_deflator.csv') %>%
              select(!c(deflator,X))) %>%
  rename(deflator = 'index_deflator') %>%
  left_join(.,cost_weights,by='type')

#data
data_final_a <- rbind(data_final_a,
                      data_final_a %>%
                        filter(type == 'Outpatients') %>%
                        filter(models == 'Morbidity') %>%
                        mutate(models = 'Resilience',
                               type = 'Other',
                               modelled_growth = 1,
                               baseline = case_when(fyear == 2018 ~ 0,
                                         T ~ 1.8*1e9),
                               deflator = 1)
)

#ae
data_final_a <- rbind(data_final_a,
                      data_final_a %>%
                        filter(type == 'Outpatients') %>%
                        filter(models == 'Morbidity') %>%
                        mutate(models = 'Policy: Recovery (5-year)',
                               modelled_growth = case_when(
                                 fyear == 2018 ~ 1,
                                 fyear > 2018 & fyear <= 2024 ~ modelled_growth,
                                 fyear == 2030 ~ 0.931869,
                                 fyear < 2030 & fyear > 2024 ~ 1.048,
                                 T ~ 1.02),
                               modelled_growth = cumprod(modelled_growth))
)

data_final_a <- rbind(data_final_a,
                      data_final_a %>%
                        filter(type == 'Outpatients') %>%
                        filter(models == 'Morbidity') %>%
                        mutate(models = 'Policy: Recovery (10-year)',
                               modelled_growth = case_when(
                                 fyear == 2018 ~ 1, 
                                 fyear > 2018 & fyear <= 2024 ~ modelled_growth,
                                 T ~ 1.032^(fyear-2018))))

#ae
data_final_a <- rbind(data_final_a,
                      data_final_a %>%
                        filter(type == 'Elective') %>%
                        filter(models == 'Linear growth') %>%
                        mutate(models = 'Policy: Recovery (5-year)',
                               modelled_growth = case_when(
                                 fyear == 2018 ~ 1,
                                 fyear > 2018 & fyear <= 2024 ~ modelled_growth,
                                 fyear == 2030 ~ 0.931869,
                                 fyear < 2030 & fyear > 2024 ~ 1.048,
                                 T ~ 1.02),
                               modelled_growth = cumprod(modelled_growth)))

data_final_a <- rbind(data_final_a,
                      data_final_a %>%
                        filter(type == 'Elective') %>%
                        filter(models == 'Linear growth') %>%
                        mutate(models = 'Policy: Recovery (10-year)',
                               modelled_growth = case_when(
                                 fyear == 2018 ~ 1, 
                                 fyear > 2018 & fyear <= 2024 ~ modelled_growth,
                                 T ~ 1.032^(fyear-2018)))) %>%
  mutate( modelled_growth = case_when(
    fyear == 2018 ~ 1,
    T ~ modelled_growth))

data_final_a <- rbind(data_final_a,
                      data_final_a %>%
                        filter(type %in% c('IAPT','Community','Mental Health')) %>%
                        filter(models == 'Morbidity') %>%
                        mutate(models = 'Policy: Recovery')) %>%
  mutate( modelled_growth = case_when(
    fyear == 2018 ~ 1,
    T ~ modelled_growth))
