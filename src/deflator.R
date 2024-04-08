
#Scrape addresses of files

##############################################
################### SETUP ####################
##############################################

###### Libraries ######

rm(list = ls()) #Clear the working environment

#####################################################
################### latest deflator ####################
#####################################################

# Gov.uk page for deflator
read.def.page <- read_html("https://www.gov.uk/government/collections/gdp-deflators-at-market-prices-and-money-gdp")

# Check gov.uk for latest deflator
latest.deflator.pg <- read.def.page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("gdp-deflators-at-market-prices-and-money-gdp")%>%
  .[[1]] # find those that end in xlsx

# latest deflator page
read.latest.deflator <- read_html(paste("https://www.gov.uk",latest.deflator.pg, sep=""))

# Read latest deflator file
latest.deflator.xlsx <- read.latest.deflator  %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("\\.xlsx") %>% # find those that end in xlsx
  .[[1]]  

#Download file
downloader::download((latest.deflator.xlsx),
         dest=("const/latest_deflator.xlsx"), mode="wb")

#Read file into R
latest_deflator <- read_excel("const/latest_deflator.xlsx",skip = 6) 
latest_deflator$`Financial year` <- gsub("\\s*\\([^\\)]+\\)","",as.character(latest_deflator$`Financial year`))
latest_deflator$`Financial year` <- gsub(",","",as.character(latest_deflator$`Financial year`))
latest_deflator <- latest_deflator %>%
  select(1,3) %>%
  mutate(`per cent change on previous year...3`=ifelse(is.na(`per cent change on previous year...3`),NA,`per cent change on previous year...3`))%>%
  filter(row_number()<which(`Financial year`=="Sources and footnotes:"))
  


#####################################################
################### OBR long term ####################
#####################################################

# OBR page for lt deflator
read.obr.page <- read_html("https://obr.uk")

# Check OBR for lt deflator (in economic determinants)
lt.deflator <- read.obr.page %>%
  html_nodes("a") %>%       # find all links
  html_attr("href") %>%     # get the url
  str_subset("fiscal-risks") %>%
  .[[1]] # find those that end in xlsx

# lt deflator page
read.lt.deflator <- read_html(paste(lt.deflator))

# Read lt deflator file
lt.deflator.xlsx <- 'https://obr.uk/download/long-term-economic-determinants-march-2023-economic-and-fiscal-outlook/?tmstv=1712326135'

#Download file
downloader::download((lt.deflator.xlsx),
         dest=("const/LT_deflator.xlsx"), mode="wb")

#Read file into R
lt_deflator <- read_excel("const/LT_deflator.xlsx",sheet = "Long-term economic determinants",skip = 2)
names(lt_deflator) <- as.data.frame(lt_deflator[2,])
names(lt_deflator)[1] <- 'type'
lt_deflator <- lt_deflator %>%
              filter(type=="GDP deflator")
lt_deflator <- lt_deflator %>% 
  tidyr::pivot_longer(!type, values_to = "per cent change on previous year...3", names_to = "Financial year")%>%
  dplyr::select("Financial year","per cent change on previous year...3") %>% 
  dplyr::mutate(`per cent change on previous year...3` = as.numeric(`per cent change on previous year...3`)) %>%
  filter(!`Financial year` %in% (latest_deflator$`Financial year`))


#####################################################
################### Combine ####################
#####################################################

# join files
deflator <- dplyr::bind_rows(latest_deflator,lt_deflator) 

# create new deflator series (note, index will be first year in series)
deflator$deflator <-0
deflator$deflator[1] <-100

# multiple first row by y-o-y growth
for (i in 2:nrow(deflator)) {
  deflator$deflator[i] <- deflator$deflator[i-1]*(1+(deflator$`per cent change on previous year`[i]/100))
}

# select base year
Base_year <- "2022-23"

# re-index to base year
deflator$deflator_index_base <- deflator$deflator/deflator$deflator[deflator$`Financial year`==Base_year]*100

# keep first 4 characters of FY
FINAL_deflator <- deflator %>%
            rename(fyear = `Financial year`) %>%
            rename(deflator_index = deflator, deflator=`per cent change on previous year`) %>%
            mutate(fyear=substr(fyear,1,4)) %>%
            select(fyear,deflator)



