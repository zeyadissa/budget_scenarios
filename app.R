library(tidyverse)
library(shinythemes)
library(shiny)
library(shinyWidgets)
library(devtools)
library(bslib)
library(bsicons)

devtools::install_github('THF-evaluative-analytics/THFstyle')

#deflator data
FINAL_deflator <- read.csv('const/FINAL_deflator.csv') %>%
  select(!X)

splits <- read.csv('const/splits.csv') %>%
  select(!X) %>%
  mutate(r = case_when(type=='specialised' ~ 0,
                       T~ 1-(w+d)))

#activity growth data
data_final <- read.csv('const/test_data.csv') %>%
  select(!c(X)) %>%
  filter(type != 'iapt') %>%
  left_join(.,splits,by='type') %>%
  left_join(.,FINAL_deflator %>% 
              rename(real_deflator = 'deflator') %>%
              mutate(fyear=as.integer(fyear)),by=c('fyear')) %>%
  rename(deflator = 'index_deflator')

test <- data_final %>%
  select(type,models) %>% 
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

CreateIndex <- function(w,d,r,prod,pay,drug,deflator){
  
  1 + (( (w*(pay-prod)) + (r*(drug-prod)) + (d*(1-prod) )))

}

CreatePayIndex <- function(w,d,r,prod,pay,drug,deflator){
  1+ (w*(pay))
}

CreateDrugIndex <- function(w,d,r,prod,pay,drug,deflator){
  1+ (r*(drug)) 
}

CreateProdIndex <- function(w,d,r,prod,pay,drug,deflator){
  CreateIndex(w,d,r,prod,pay,drug,deflator) - CreateProdIndex(w,d,r,prod,pay,drug,deflator) - CreateDrugIndex(w,d,r,prod,pay,drug,deflator)
}

# Global Variables --------------------------------------------------------

#These are the controls for the zoom: they are needed to set the boundary to UK
THEME <- 'simplex'
min_date <- 2018
max_date <- 2035
measures <- c('cost','activity')
activity_growth_type <- c('Constant','Log','Changing')
shock_types <- c('Permanent','U-Shaped')
water_type <- unique(test$type)
#this is the dumbest thing i've ever done but it works.
#note to self: this is ideally where purrr::walk should be used.
for(i in test$type){
  assign(x=paste0(i,'_activity_growth_type'),
         value=((test %>% filter(type==i))$models)[order(nchar(((test %>% filter(type==i))$models)), ((test %>% filter(type==i))$models),
                                                         decreasing = T)]
         )
         }
    

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML(".col-sm-8 {
        border: 2px solid #486178;
        padding-left: 0px; 
        padding-right: 0px;
      }"))
  ),
  #Determines theme
  theme = shinythemes::shinytheme(THEME),
  #Navigation page
  navbarPage(
    "Budgetary Scenarios",
    tabPanel('Scenario Maker',
             fluid = T,
             icon=icon('house'),
             sidebarLayout(
               sidebarPanel(
                 titlePanel(h4(strong('BUDGET SCENARIOS'),icon('house',class='about-icon fa-pull-left'))),
                 hr(),
                 
                 numericInput(
                   inputId = 'prod',
                   label = 'Custom productivity CAGR (%)',
                   min = -100,
                   value = 0.58,
                   max = 100,
                   step = 1
                 ),
                 numericInput(
                   inputId = 'pay',
                   label = 'Custom above-inflation pay growth (%)',
                   min = -100,
                   value = 0.8,
                   max = 100,
                   step = 1
                 ),
                 numericInput(
                   inputId = 'drug',
                   label = 'Custom drug growth (%)',
                   min = -100,
                   value = 2.5,
                   max = 100,
                   step = 1
                 ),
                 materialSwitch(
                   inputId = "real_flag",
                   label = "Real terms?", 
                   value = TRUE,
                   status = "primary"
                 ),
                 materialSwitch(
                   inputId = "per_capita",
                   label = "Make per capita?", 
                   value = F,
                   status = "primary"
                 ),
                 hr(),
                 radioGroupButtons(
                   inputId = "shock_type",
                   label = "Select shock type",
                   choices = shock_types,
                   justified = TRUE
                 ),
                 numericInput(
                   inputId = 'intensity',
                   label = 'Shock intensity',
                   min = -100,
                   value = 0,
                   max = 100,
                   step = 1
                 ),
                 sliderTextInput(
                   inputId = "range",
                   label = "Choose shock range", 
                   choices = min_date:max_date,
                   selected = c(2020,2021)
                 ),
                 hr(),
                 selectInput(
                   inputId = "water_type",
                   label = "Select POD for waterfall",
                   choices = water_type),
                 
                 selectInput(
                   inputId = "water_year",
                   label = "Select financial year for waterfall chart",
                   choices = 2018:2035,
                   selected = 2035),
                 hr(),
                 actionButton("run",
                              "Generate Scenario",
                              style = "width:100%;"),
                 width = 3),
               mainPanel(
                 fluidRow(
                   dropdown(
                            tags$h4('Activity Inputs'),
                            circle = T,
                            style = 'unite',
                            label = 'Model assumptions per POD',
                            status = 'danger',
                            icon = icon('hospital'),
                            width = '300px',
                            #item 1
                            selectInput(
                              inputId = "ae_growth",
                              label = "A&E",
                              choices = ae_activity_growth_type
                            ),
                            selectInput(
                              inputId = "elective_growth",
                              label = "Elective",
                              choices = elective_activity_growth_type
                            ),
                            selectInput(
                              inputId = "op_growth",
                              label = "Outpatients",
                              choices = op_activity_growth_type
                            ),
                            selectInput(
                              inputId = "emergency_growth",
                              label = "Emergency",
                              choices = emergency_activity_growth_type
                            ),
                            selectInput(
                              inputId = "gp_growth",
                              label = "General Practice",
                              choices = gp_activity_growth_type
                            ),
                            selectInput(
                              inputId = "prescribing_growth",
                              label = "Prescribing",
                              choices = prescribing_activity_growth_type
                            ),
                            selectInput(
                              inputId = "specialised_growth",
                              label = "Specialised Services",
                              choices = specialised_activity_growth_type
                            ),
                            selectInput(
                              inputId = "mh_growth",
                              label = "Mental Health",
                              choices = mh_activity_growth_type
                            ),
                            selectInput(
                              inputId = "maternity_growth",
                              label = "Maternity",
                              choices = maternity_activity_growth_type
                            ),
                            selectInput(
                              inputId = "community_growth",
                              label = "Community",
                              choices = community_activity_growth_type
                            )),
                   br(),
                   tabsetPanel(
                     tabPanel('Time-series',
                              icon=icon('house'),
                              fluidRow(
                                br(),
                                column(12,plotly::plotlyOutput('maingraph')),
                                column(12, plotly::plotlyOutput('subgraph')),
                                column(12, textOutput('growth_rate')))),
                     tabPanel('Waterfall',
                              icon=icon('water'),
                              fluidRow(
                                br(),
                                column(12,plotOutput('waterfall_graph')),
                                column(12,plotOutput('waterfall_graph2'))))
                          )),
                 style='border: 0px',
                 width = 9))
               )))
               
# SERVER ------------------------------------------------------------------

server <- function(input,output,session){
  
  deflator_adj <- reactive({
    if(input$real_flag == F){
      deflator <- 1
    } else {
      deflator <- 0
    }
  })%>%
    bindEvent(input$run)
  
  pop_adj <- reactive({
    if(input$per_capita == F){
      pop_final %>%
        mutate(values = 1) %>%
        select(!c(metric,X))
    } else
      pop_final %>%
      select(!c(metric,X)) %>%
      mutate(values = values / 1000000)
  })%>%
    bindEvent(input$run)
  
  intensity_adj <- reactive({
    intensity <- (100 + input$intensity)
    return(intensity)
  })%>%
    bindEvent(input$run)
  
  base_data <- reactive({
    data_final %>%
      CreateShock(.,
                  shock_type=input$shock_type,
                  shock_range = input$range,
                  val_prod = input$prod,
                  inten = intensity_adj()) %>%
      group_by(type,measure,models)%>%
      mutate(pay = (100 + input$pay)/100,
             drug = (100 + input$drug)/100,
             val_drug = cumprod(drug),
             val_deflator = deflator ^ deflator_adj(),
             val_prod = cumprod(prod),
             val_pay = cumprod(pay)) %>%
      rowwise()%>%
      mutate(index = CreateIndex(w = w,
                                 d = d,
                                 r=r,
                                 prod=val_prod,
                                 pay=val_pay,
                                 drug = val_drug,
                                 deflator=val_deflator),
             pay_index = CreatePayIndex(w = w,
                                     d = d,
                                     r=r,
                                     prod=val_prod,
                                     pay=val_pay,
                                     drug = val_drug,
                                     deflator=val_deflator),
             drugs_index = CreateDrugIndex(w = w,
                                       d = d,
                                       r=r,
                                       prod=val_prod,
                                       pay=val_pay,
                                       drug = val_drug,
                                       deflator=val_deflator),
             final_value = baseline * (modelled_growth)*index*val_deflator,
             final_value_no_cost_index = baseline * (modelled_growth)*val_deflator,
             final_cost_value = baseline * (modelled_growth-1)*(1-index)*val_deflator,
             final_drugs_value = baseline * (modelled_growth-1)*(1-drugs_index)*val_deflator,
             final_pay_value = baseline * (modelled_growth-1)*(1-pay_index)*val_deflator,
             final_prod_value = final_cost_value - final_drugs_value - final_pay_value) %>%
      filter(measure == 'cwa')
  })%>%
    bindEvent(input$run)
  
  base_data_noshock <- reactive({
    data_final %>%
      group_by(type,measure,models)%>%
      mutate(pay = (100 + input$pay)/100,
             prod = (100 + input$prod)/100,
             drug = (100 + input$drug)/100,
             val_drug = cumprod(drug),
             val_deflator = deflator ^ deflator_adj(),
             val_prod = cumprod(prod),
             val_pay = cumprod(pay)) %>%
      rowwise()%>%
      mutate(index = CreateIndex(w = w,
                                 d = d,
                                 r=r,
                                 prod=val_prod,
                                 pay=val_pay,
                                 drug = val_drug,
                                 deflator=val_deflator),
             pay_index = CreatePayIndex(w = w,
                                        d = d,
                                        r=r,
                                        prod=val_prod,
                                        pay=val_pay,
                                        drug = val_drug,
                                        deflator=val_deflator),
             drugs_index = CreateDrugIndex(w = w,
                                           d = d,
                                           r=r,
                                           prod=val_prod,
                                           pay=val_pay,
                                           drug = val_drug,
                                           deflator=val_deflator),
             prod_index = index - pay_index - drugs_index,
             final_value_no_cost_index = baseline * (modelled_growth)*val_deflator,
             final_value_base = baseline * (modelled_growth)*index*val_deflator,
             final_cost_value = baseline * (modelled_growth-1)*(1-index)*val_deflator,
             final_drugs_value = baseline * (modelled_growth-1)*(1-drugs_index)*val_deflator,
             final_pay_value = baseline * (modelled_growth-1)*(1-pay_index)*val_deflator,
             final_prod_value = final_cost_value - final_drugs_value - final_pay_value) %>%
      filter(measure == 'cwa') %>%
        #true filter
        filter(
          type == 'ae' & models == tolower(input$ae_growth) |
            type == 'elective' & models == tolower(input$elective_growth) |
            type == 'emergency' & models == tolower(input$emergency_growth) |
            type == 'prescribing' & models == tolower(input$prescribing_growth) |
            type == 'op' & models == tolower(input$op_growth) |
            type == 'gp' & models == tolower(input$gp_growth) |
            type == 'community' & models == tolower(input$community_growth) |
            type == 'specialised' & models == tolower(input$specialised_growth) |
            type == 'mh' & models == tolower(input$mh_growth) |
            type == 'maternity' & models == tolower(input$maternity_growth)) %>%
        group_by(fyear) %>%
        summarise(final_value_base = sum(final_value_base,na.rm=T))%>%
        left_join(.,pop_adj(),by='fyear') %>%
        mutate(final_value= final_value_base / values)
  })%>%
    bindEvent(input$run)
  
  #data for index
  #data for index
  data_index <- reactive({
    base_data() %>%
      #true filter
      filter(
        type == 'ae' & models == tolower(input$ae_growth) |
          type == 'elective' & models == tolower(input$elective_growth) |
          type == 'emergency' & models == tolower(input$emergency_growth) |
          type == 'prescribing' & models == tolower(input$prescribing_growth) |
          type == 'op' & models == tolower(input$op_growth) |
          type == 'gp' & models == tolower(input$gp_growth) |
          type == 'community' & models == tolower(input$community_growth) |
          type == 'specialised' & models == tolower(input$specialised_growth) |
          type == 'mh' & models == tolower(input$mh_growth) |
          type == 'maternity' & models == tolower(input$maternity_growth)) %>%
      ungroup()%>%
      select(fyear,val_prod,val_drug,val_pay,val_deflator) %>%
      left_join(.,supplement %>% filter(fyear %in% data_final$fyear),by=c('fyear')) %>%
      pivot_longer(cols=!c(fyear),names_to='names',values_to='values') %>%
      unique()
  })%>%
    bindEvent(input$run)
  
  #data for baseline
  data_model <- reactive({
    data_model1 <- base_data()%>%
      #true filter
      filter(
        type == 'ae' & models == tolower(input$ae_growth) |
          type == 'elective' & models == tolower(input$elective_growth) |
          type == 'emergency' & models == tolower(input$emergency_growth) |
          type == 'prescribing' & models == tolower(input$prescribing_growth) |
          type == 'op' & models == tolower(input$op_growth) |
          type == 'gp' & models == tolower(input$gp_growth) |
          type == 'community' & models == tolower(input$community_growth) |
          type == 'specialised' & models == tolower(input$specialised_growth) |
          type == 'mh' & models == tolower(input$mh_growth) |
          type == 'maternity' & models == tolower(input$maternity_growth)) %>%
      group_by(fyear) %>%
      summarise(final_value = sum(final_value,na.rm=T))%>%
      left_join(.,pop_adj(),by='fyear') %>%
      mutate(final_value=final_value / values)
    
    data_model2 <-  data_model1 %>%
      mutate(baseline_budget = (data_model1 %>% filter(fyear==2018))$final_value,
             budget_index = final_value / baseline_budget)
    
    return(data_model2)
    
  })%>%
    bindEvent(input$run)
  
  #area in model variation from baseline
  data_ribbon <- reactive({
    base_data_noshock() %>% 
      select(fyear,final_value_base) %>%
      left_join(.,data_model() %>% select(fyear,final_value),
                by=c('fyear'))
    
  })%>%
    bindEvent(input$run)
  
  growth <- reactive({
    round(100*(((data_model() %>% 
                   ungroup() %>% 
                   select(fyear,final_value) %>% 
                   filter(fyear == max(fyear)))[1,2] -
                  (data_model() %>% 
                     ungroup() %>% 
                     select(fyear,final_value) %>% 
                     filter(fyear == min(fyear)))[1,2])/
                 (length(unique(data_model()$fyear))*(data_model() %>% 
                                                        ungroup() %>% 
                                                        select(fyear,final_value) %>% 
                                                        filter(fyear == min(fyear)))[1,2])),2)
    
  })%>%
    bindEvent(input$run)
  
  output$growth_rate <- renderText({
    paste0('   *Average annualised growth rate is: ', 
           growth(),
           '%')
  })
  
  #graphs and outputs
  output$maingraph <- plotly::renderPlotly({
    plotly::ggplotly(ggplot()+
                       ggtitle("Shock adjusted budget prediction relative to baseline") +
                       geom_line(data = base_data_noshock(),aes(x=fyear,y=final_value_base/1e9),linetype = 2,col='#dd0031',alpha=1) +
                       geom_line(data=data_model(),aes(x=fyear,y=final_value/1e9),col='#dd0031') +
                       geom_ribbon(data=data_ribbon(),aes(x=fyear,ymin=final_value_base/1e9,ymax=final_value/1e9),fill='#dd0031',alpha=0.1)+
                       THFstyle::scale_colour_THF()+
                       theme_bw(base_size = 10) +
                       xlab('') +
                       ylab('Budget (£bn)') +
                       labs(fill='POD') + 
                       theme(legend.position="bottom"))
    
  }) 
  
  output$subgraph <- plotly::renderPlotly({
    plotly::ggplotly(
      ggplot2::ggplot()+
        ggtitle("Index of supplementary variables") +
        geom_line(data = data_index(),aes(x=fyear,y=values,col=names)) +
        geom_point(data = data_index(),aes(x=fyear,y=values,col=names)) +
        geom_line(data = data_model(),aes(x=fyear,y=budget_index),linetype=2,col='black') +
        THFstyle::scale_colour_THF()+
        theme_bw(base_size = 10) +
        theme(panel.background = element_blank())+
        xlab('') +
        ylab('Index (2017 = 100)') +
        labs(col='Measure') + 
        theme(legend.position="bottom")
      
    )
  })   
  
  #data for model
  data_waterfall <- reactive({
    df1 <- base_data() %>%
      #true filter
      group_by(fyear,models,type) %>%
      mutate(final_value_no_cost_index =   final_value_no_cost_index - baseline * (1) * val_deflator ) %>%
      summarise(final_value_no_cost_index = sum(final_value_no_cost_index,na.rm=T),
                final_value = sum(final_value,na.rm=T),
                pay_adjustment = -sum(final_pay_value,na.rm=T),
                drug_cost_adjustment = -sum(final_drugs_value,na.rm=T),
                productivity_adjustment = -sum(final_prod_value,na.rm=T)) %>%
      ungroup() %>%
      select(type,models,fyear,final_value_no_cost_index,pay_adjustment,drug_cost_adjustment,productivity_adjustment)
    
    df2 <- df1 %>%
      filter(
        type == 'ae' & models == tolower(input$ae_growth) |
          type == 'elective' & models == tolower(input$elective_growth) |
          type == 'emergency' & models == tolower(input$emergency_growth) |
          type == 'prescribing' & models == tolower(input$prescribing_growth) |
          type == 'op' & models == tolower(input$op_growth) |
          type == 'gp' & models == tolower(input$gp_growth) |
          type == 'community' & models == tolower(input$community_growth) |
          type == 'specialised' & models == tolower(input$specialised_growth) |
          type == 'mh' & models == tolower(input$mh_growth) |
          type == 'maternity' & models == tolower(input$maternity_growth)) %>%
      select(!c(models,final_value_no_cost_index)) %>%
      pivot_longer(cols=!c(type,fyear),names_to='models',values_to='final_value_no_cost_index')
    
    df3 <- rbind(df1 %>% select(type,fyear,models,final_value_no_cost_index),df2)

      
  })%>%
    bindEvent(input$run)
  
  filter_val_waterfall <- reactive({
    (data_waterfall() %>%
      filter(fyear == input$water_year) %>%
      filter(type == input$water_type) %>%
      filter(
        type == 'ae' & models == tolower(input$ae_growth) |
          type == 'elective' & models == tolower(input$elective_growth) |
          type == 'emergency' & models == tolower(input$emergency_growth) |
          type == 'prescribing' & models == tolower(input$prescribing_growth) |
          type == 'op' & models == tolower(input$op_growth) |
          type == 'gp' & models == tolower(input$gp_growth) |
          type == 'community' & models == tolower(input$community_growth) |
          type == 'specialised' & models == tolower(input$specialised_growth) |
          type == 'mh' & models == tolower(input$mh_growth) |
          type == 'maternity' & models == tolower(input$maternity_growth)))$final_value_no_cost_index/1e9
  }) %>%
    bindEvent(input$run)
  
  base_waterfall <- reactive({
    df1 <- data_waterfall() %>%
      filter(fyear == input$water_year) %>%
      filter(type == input$water_type) %>%
      mutate(final_value_no_cost_index = round(final_value_no_cost_index / 1e9,2)) %>%
      ungroup()%>%
      select(models,final_value_no_cost_index)
    
    filter_val <- case_when(
      input$water_type == 'ae' ~ tolower(input$ae_growth),
      input$water_type == 'elective' ~ tolower(input$elective_growth),
      input$water_type == 'emergency' ~ tolower(input$emergency_growth),
      input$water_type == 'prescribing' ~ tolower(input$prescribing_growth),
      input$water_type == 'op' ~ tolower(input$op_growth),
      input$water_type == 'gp' ~ tolower(input$gp_growth),
      input$water_type == 'community' ~ tolower(input$community_growth),
      input$water_type == 'specialised' ~ tolower(input$specialised_growth),
      input$water_type == 'mh' ~ tolower(input$mh_growth),
      input$water_type == 'maternity' ~ tolower(input$maternity_growth))
    
    df2 <- df1 %>%
      filter(
        case_when(
          grepl(pattern='log',filter_val) == T ~ grepl(pattern='linear',models) == F,
          grepl(pattern='linear',filter_val) == T ~ grepl(pattern='log',models) == F,
          T ~  final_value_no_cost_index <= filter_val_waterfall() | models %in% c('pay_adjustment','productivity_adjustment','drug_cost_adjustment'))
        )
      return(df2)
  })%>%
    bindEvent(input$run)
  
  output$waterfall_graph <- renderPlot({
    waterfalls::waterfall(base_waterfall() %>%
                            mutate(final_value_no_cost_index = case_when(
                              final_value_no_cost_index < 0 ~ 0,
                              T ~ final_value_no_cost_index
                            )) ,calc_total = TRUE,total_rect_color = "orange",rect_text_size=1.5) +
      theme_bw(base_size = 16) +
      xlab('') + 
      ylab('') +
      THFstyle::scale_fill_THF()+
      ggtitle('Growth within POD dissagregated by model type')+
      theme(text=element_text(size=16))
  })
  
  type_waterfall <- reactive({
    base_data() %>%
      filter(fyear == input$water_year) %>%
      filter(
        type == 'ae' & models == tolower(input$ae_growth) |
          type == 'elective' & models == tolower(input$elective_growth) |
          type == 'emergency' & models == tolower(input$emergency_growth) |
          type == 'prescribing' & models == tolower(input$prescribing_growth) |
          type == 'op' & models == tolower(input$op_growth) |
          type == 'gp' & models == tolower(input$gp_growth) |
          type == 'community' & models == tolower(input$community_growth) |
          type == 'specialised' & models == tolower(input$specialised_growth) |
          type == 'mh' & models == tolower(input$mh_growth) |
          type == 'maternity' & models == tolower(input$maternity_growth)) %>%
      #NOTE TO SELF: THIS MAY BE MUCKING THINGS UP. CHECK
      tidyr::drop_na ()%>%
      select(type,final_value) %>%
      mutate(final_value = round(final_value/1e9,2))
  })%>%
    bindEvent(input$run)
  
  output$waterfall_graph2 <- renderPlot({
    waterfalls::waterfall(type_waterfall() %>%
                            mutate(final_value = case_when(
                              final_value < 0 ~ 0,
                              T ~ final_value
                            )),calc_total = TRUE,total_rect_color = "orange",rect_text_size=1.5) +
      theme_bw(base_size = 16) +
      xlab('') + 
      ylab('') +
      THFstyle::scale_fill_THF() +
      ggtitle('Change in budget relative to baseline scenario') +
      theme(text=element_text(size=16))
    
  })
  
}
  
shinyApp(ui=ui,server=server)

