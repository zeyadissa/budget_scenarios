library(tidyverse)
library(shinythemes)
library(shiny)
library(shinyWidgets)
library(devtools)

devtools::install_github('THF-evaluative-analytics/THFstyle')

#deflator data
FINAL_deflator <- read.csv('const/FINAL_deflator.csv') %>%
  select(!X)

waterfall_baseline <- read.csv('const/waterfall_baseline.csv') %>%
  select(!X) %>%
  rename(baseline_value = 'final_value')

splits <- read.csv('const/splits.csv') %>%
  select(!X)

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

CreateIndex <- function(w,d,prod,pay,drug,deflator){
  1 + ((w*(((pay+deflator)-2))) - ((d)*(deflator-1)) + ((1-w-d)*(drug-1)))/(prod)
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
         value=(test %>% filter(type==i))$models)
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
                   value = 0,
                   max = 100,
                   step = 1
                 ),
                 numericInput(
                   inputId = 'drug',
                   label = 'Custom drug growth (%)',
                   min = -100,
                   value = 0.5,
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
      deflator <- 0
    } else {
      deflator <- 1
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
  
  #data for index
  data_index <- reactive({
    
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
                                 prod=val_prod,
                                 pay=val_pay,
                                 drug = val_drug,
                                 deflator=val_deflator)) %>%
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
      ungroup()%>%
      select(fyear,val_prod,val_drug,val_pay,val_deflator) %>%
      left_join(.,supplement %>% filter(fyear %in% data_final$fyear),by=c('fyear')) %>%
      pivot_longer(cols=!c(fyear),names_to='names',values_to='values') %>%
      unique()
  })%>%
    bindEvent(input$run)
  
  #data for baseline
  data_baseline <- reactive({
    data_final %>%
      group_by(models,measure,type) %>%
      mutate(prod = (100 + input$prod)/100,
             pay = (100 + input$pay)/100,
             drug = (100 + input$drug)/100,
             val_drug = cumprod(drug),
             val_deflator = deflator ^ deflator_adj(),
             val_prod = cumprod(prod),
             val_pay = cumprod(pay)
      ) %>%
      rowwise()%>%
      mutate(index = CreateIndex(w = w,
                                 d = d,
                                 drug = val_drug,
                                 prod=val_prod,
                                 pay=val_pay,
                                 deflator=val_deflator),
             final_value = baseline * (modelled_growth)*index) %>%
      filter(measure == 'cwa') %>%
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
      summarise(final_value_base = sum(final_value,na.rm=T))%>%
      left_join(.,pop_adj(),by='fyear') %>%
      mutate(final_value_base= final_value_base / values)
  })%>%
    bindEvent(input$run)
  
  intensity_adj <- reactive({
    intensity <- (100 + input$intensity)
    return(intensity)
  })%>%
    bindEvent(input$run)
  
  #data for model
  data_model <- reactive({
    data_model1 <- data_final %>%
      CreateShock(.,
                  shock_type = input$shock_type,
                  shock_range = input$range,
                  val_prod = input$prod,
                  inten = intensity_adj()) %>%
      group_by(models,measure,type) %>%
      mutate(pay = (100 + input$pay)/100,
             drug = (100 + input$drug)/100,
             val_drug = cumprod(drug),
             val_deflator = deflator ^ deflator_adj(),
             val_prod = cumprod(prod),
             val_pay = cumprod(pay)
      ) %>%
      rowwise()%>%
      mutate(index = CreateIndex(w = w,
                                 d = d,
                                 drug = val_drug,
                                 prod=val_prod,
                                 pay=val_pay,
                                 deflator=val_deflator),
             final_value = baseline * (modelled_growth)*index) %>%
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
    
    data_baseline() %>% select(fyear,final_value_base) %>%
      left_join(.,data_model() %>% select(fyear,final_value),by=c('fyear'))
    
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
                       geom_line(data = data_baseline(),aes(x=fyear,y=final_value_base/1e9),linetype = 2,col='#dd0031',alpha=1) +
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
    data_final %>%
      CreateShock(.,
                  shock_type = input$shock_type,
                  shock_range = input$range,
                  val_prod = input$prod,
                  inten = intensity_adj()) %>%
      group_by(models,measure,type) %>%
      mutate(pay = (100 + input$pay)/100,
             drug = (100 + input$drug)/100,
             val_drug = cumprod(drug),
             val_deflator = deflator ^ deflator_adj(),
             val_prod = cumprod(prod),
             val_pay = cumprod(pay)
      ) %>%
      rowwise()%>%
      mutate(index = CreateIndex(w = w,
                                 d = d,
                                 drug = val_drug,
                                 prod=val_prod,
                                 pay=val_pay,
                                 deflator=val_deflator),
             final_value = baseline * (modelled_growth)*index) %>%
      filter(measure == 'cwa') %>%
      #true filter
      group_by(fyear,models,type) %>%
      summarise(final_value = sum(final_value,na.rm=T))%>%
      ungroup() %>%
      left_join(.,waterfall_baseline,by=c('type','fyear')) %>%
      mutate(final_value_base = round((final_value - baseline_value)/1e9),2) %>%
      ungroup()%>%
      select(type,models,fyear,final_value,final_value_base)
  })%>%
    bindEvent(input$run)
  
  base_waterfall <- reactive({
    data_waterfall() %>%
      filter(fyear == input$water_year) %>%
      filter(type == input$water_type) %>%
      mutate(final_value = round(final_value / 1e9,2) ) %>%
      select(models,final_value)
  })%>%
    bindEvent(input$run)
  
  output$waterfall_graph <- renderPlot({
    waterfalls::waterfall(base_waterfall(),calc_total = TRUE,total_rect_color = "orange",rect_text_size=1.5) +
      theme_bw(base_size = 16) +
      xlab('') + 
      ylab('') +
      THFstyle::scale_fill_THF()+
      ggtitle('Growth within POD dissagregated by model type')+
      theme(text=element_text(size=16))
    
      
  })
  
  type_waterfall <- reactive({
    data_waterfall() %>%
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
      select(type,final_value_base)
  })%>%
    bindEvent(input$run)
  
  output$waterfall_graph2 <- renderPlot({
    waterfalls::waterfall(type_waterfall(),calc_total = TRUE,total_rect_color = "orange",rect_text_size=1.5) +
      theme_bw(base_size = 16) +
      xlab('') + 
      ylab('') +
      THFstyle::scale_fill_THF() +
      ggtitle('Change in budget relative to baseline scenario') +
      theme(text=element_text(size=16))
    
  })
  
}
  
shinyApp(ui=ui,server=server)

