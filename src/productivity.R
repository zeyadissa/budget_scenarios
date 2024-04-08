# This file scrapes the latest productivity file from the ONS 

#Scrape addresses of files

#####################################################
################### ONS productivity ####################
#####################################################

# ONS page for productivity
read.prod.page <- read_html("https://www.ons.gov.uk/economy/economicoutputandproductivity/publicservicesproductivity/datasets/publicserviceproductivityestimateshealthcareengland")

# Check gov.uk for latest deflator
latest.prod.pg <- read.prod.page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("publicservicesproductivity/datasets/publicserviceproductivityestimateshealthcareengland/financialyearending") %>%
  .[[1]] # find those that end in xlsx

# latest deflator page
latest.prod.xlsx <- paste("https://www.ons.gov.uk",latest.prod.pg, sep="")

#Download file
download((latest.prod.xlsx),
         dest=("const/latest_productivity.xlsx"), mode="wb")

#Read file into R
latest_productivity <- read_excel("const/latest_productivity.xlsx", 
                                  sheet = "3b", skip = 3) %>%
  select(1,4) %>%
  filter(row_number()<which(`Year`=="2019/20")) %>%
  rename_with(~ ifelse(grepl('NQA Prod', .x), 'central_prod', .x)) %>%
  mutate(central_prod = (central_prod / lag(central_prod)-1)*100) %>%
  mutate(max_prod = central_prod, min_prod=central_prod) %>%
  rename(fyear = Year) %>%
  mutate(fyear=substr(fyear,1,4))

#Create values for projection

project_productivity <- read_excel("const/latest_productivity.xlsx", 
                                   sheet = "3b", skip = 3) %>%
  select(1,4) %>%
  filter(row_number()<which(`Year`=="2019/20")) %>%
  rename_with(~ ifelse(grepl('NQA Prod', .x), 'NQA_prod', .x)) %>%
  mutate(cagr=(((NQA_prod/lag(NQA_prod,10))^(1/10))-1)*100) %>%
  mutate(cagr_all=(((NQA_prod/lag(NQA_prod,23))^(1/23))-1)*100) 

project_productivity <-project_productivity %>% 
  mutate(max_prod=max((project_productivity$cagr),na.rm=T),
         min_prod=min((project_productivity$cagr),na.rm=T),
         central_prod=max(cagr_all,na.rm=T))%>%
  dplyr::select(-c(Year, NQA_prod, cagr, cagr_all)) %>%
  distinct()

project_productivity_df <-data.frame(fyear=c(2019:2035),
                                     min_prod=(project_productivity$min_prod),
                                     central_prod=(project_productivity$central_prod),
                                     max_prod=(project_productivity$max_prod)) %>%
  mutate(fyear=as.character(fyear))

# keep first 4 characters of FY
FINAL_productivity <- rbind(latest_productivity, project_productivity_df) 

