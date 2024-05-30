
#activity growth data
data_final_a <- read.csv('const/final_data.csv') %>%
  select(!X) %>%
  mutate(deflator = case_when(
    fyear == 2018 ~ 1,
    T ~ deflator
  ))

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
                                 fyear == 2030 ~ 0.931869,
                                 fyear < 2030 ~ 1.048,
                                 T ~ 1.02),
                               modelled_growth = cumprod(modelled_growth))
)

data_final_a <- rbind(data_final_a,
                      data_final_a %>%
                        filter(type == 'Outpatients') %>%
                        filter(models == 'Morbidity') %>%
                        mutate(models = 'Policy: Recovery (10-year)',
                               modelled_growth = 1.032^(fyear-2018)))

#ae
data_final_a <- rbind(data_final_a,
                      data_final_a %>%
                        filter(type == 'Elective') %>%
                        filter(models == 'Morbidity') %>%
                        mutate(models = 'Policy: Recovery (5-year)',
                               modelled_growth = case_when(
                                 fyear == 2018 ~ 1,
                                 fyear == 2030 ~ 0.931869,
                                 fyear < 2030 ~ 1.048,
                                 T ~ 1.02),
                               modelled_growth = cumprod(modelled_growth))
)

data_final_a <- rbind(data_final_a,
                      data_final_a %>%
                        filter(type == 'Elective') %>%
                        filter(models == 'Morbidity') %>%
                        mutate(models = 'Policy: Recovery (10-year)',
                               modelled_growth = 1.032^(fyear-2018))) %>%
  mutate( modelled_growth = case_when(
    fyear == 2018 ~ 1,
    T ~ modelled_growth))
