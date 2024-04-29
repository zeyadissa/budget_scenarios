#####################################################
################### Wages ####################
#####################################################

# NHS page for st pay
read.nhs.pay.page <- read_html("https://digital.nhs.uk/data-and-information/publications/statistical/nhs-staff-earnings-estimates")

# Check NHS
st.pay <- read.nhs.pay.page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("nhs-staff-earnings-estimates") %>%
  str_subset("march") %>%
  str_subset("2022")

# lt pay page
read.st.pay <- read_html(paste("https://digital.nhs.uk",st.pay,sep=""))

st.pay <- read.st.pay %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("xlsx") %>%
  str_subset("Trusts") 

download((st.pay),
         dest=("const/st_pay.xlsx"), mode="wb")

st_pay <- read_excel("const/st_pay.xlsx", sheet = "Table 2a", 
                     skip = 7)

#####################################################
################### Wages ####################
#####################################################

# NHS page for st pay
read.nhs.pay.page <- read_html("https://digital.nhs.uk/data-and-information/publications/statistical/nhs-staff-earnings-estimates")

# Check NHS
st.pay <- read.nhs.pay.page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("nhs-staff-earnings-estimates") %>%
  str_subset("march") %>%
  str_subset("2022")

# lt pay page
read.st.pay <- read_html(paste("https://digital.nhs.uk",st.pay,sep=""))

st.pay <- read.st.pay %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("xlsx") %>%
  str_subset("Trusts") 

download((st.pay),
         dest=("const/st_pay.xlsx"), mode="wb")

st_pay <- read_excel("const/st_pay.xlsx", sheet = "Table 2a", 
                     skip = 7) %>%
  dplyr::select(`Staff Group`, starts_with('12 month period ending March')) %>%
  slice(2:34) %>%
  drop_na() %>%
  rename_at(vars(matches('March')),~paste0(stringr::str_sub(.x,-4,-1))) %>%
  pivot_longer(!`Staff Group`, names_to = 'year', values_to = 'pay') %>%
  group_by(`Staff Group`) %>%
  mutate(nhs_pay = ((pay / lag(pay)-1)*100)) %>%
  rename(fyear=year, staff_group=`Staff Group`) %>%
  ungroup()

st_pay_all <- st_pay %>%
  filter(staff_group=="All staff") %>%
  drop_na() %>%
  dplyr::select(-c(staff_group, pay))

st_pay_low <- read_excel("const/st_pay.xlsx", sheet = "Table 2a", 
                         skip = 7) %>%
  dplyr::select(`Staff Group`, starts_with('12 month period ending March')) %>%
  slice(2:34) %>%
  drop_na() %>%
  rename_at(vars(matches('March')),~paste0(stringr::str_sub(.x,-4,-1))) %>%
  pivot_longer(!`Staff Group`, names_to = 'year', values_to = 'pay') %>%
  group_by(`Staff Group`) %>%
  rename(fyear=year, staff_group=`Staff Group`) %>%
  filter(staff_group=="All staff") %>%
  mutate(cagr=(((pay/lag(pay,7))^(1/7))-1)*100) %>%
  mutate(min_pay=min(cagr)) %>%
  ungroup() %>%
  dplyr::select(min_pay) %>%
  distinct()

# Read lt pay file
# OBR page for lt pay
read.obr.page <- read_html("https://obr.uk")

# Check OBR for lt pay (in economic determinants)
lt.pay <- read.obr.page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("fiscal-risks") %>%
  .[[1]] # find those that end in xlsx

# lt pay page
read.lt.pay <- read_html(paste('https://obr.uk/download/july-2023-fiscal-risks-and-sustainability-charts-and-tables-chapter-2/?tmstv=1712325923'))

lt.pay.xlsx <- 'https://obr.uk/download/long-term-economic-determinants-march-2023-economic-and-fiscal-outlook/?tmstv=1712326135'

#Download file
downloader::download((lt.pay.xlsx),
         dest=("const/LT_pay.xlsx"), mode="wb")

#Read file into R
lt_pay <- read_excel("const/LT_pay.xlsx",sheet = "Long-term economic determinants",skip = 4)

lt_pay <- lt_pay %>%
  filter(lt_pay$...1 %in% c("Average earnings","Public sector earnings","GDP deflator")) %>% 
  pivot_longer(!...1 , names_to = "Financial year") %>%
  pivot_wider(names_from=...1)

# keep first 4 characters of FY
lt_pay <- lt_pay %>%
  rename(fyear = `Financial year`) %>%
  mutate(fyear=substr(fyear,1,4)) %>%
  filter(fyear>2022) %>%
  dplyr::select(-c(`GDP deflator`,`Public sector earnings`)) %>%
  rename(nhs_pay =`Average earnings`)

FINAL_all_pay <- na.omit(rbind(st_pay_all, lt_pay))







