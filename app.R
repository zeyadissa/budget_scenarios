library(tidyverse)
library(shinythemes)
library(shiny)
library(shinyWidgets)
library(fontawesome)
library(tidyverse)
library(devtools)

#deflator data
FINAL_deflator <- read.csv('const/FINAL_deflator.csv')
  
#activity growth data
data_final <- read.csv('const/FINAL_DATA.csv') %>%
  select(!c(deflator,X)) %>%
  left_join(.,FINAL_deflator %>% 
              select(!c(X)) %>% 
              rename(real_deflator = 'deflator') %>%
              mutate(fyear=as.integer(fyear)),by=c('fyear')) %>%
  rename(deflator = 'index_deflator')

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

CreateIndex <- function(a,prod,pay,deflator){
  1 + ((a*((pay-1))) - ((1-a)*(deflator-1)))/(prod)
}

# Global Variables --------------------------------------------------------

#These are the controls for the zoom: they are needed to set the boundary to UK
THEME <- 'simplex'
min_date <- 2018
max_date <- 2035
types <- c('elective','ae','op','gp','emergency')
models <- c('log','constant','linear')
measures <- c('cost','activity')
val_a <- 0.73
activity_growth_type <- c('Constant','Log','Changing')
shock_types <- c('Permanent','U-Shaped')
scenarios <- c('Scenario A','Scenario B','Scenario C')

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
                 radioGroupButtons(
                   inputId = "scenario",
                   label = "Select Scenario Type",
                   choices = scenarios,
                   justified = TRUE
                 ),
                 numericInput(
                   inputId = 'prod',
                   label = 'Custom productivity CAGR (%)',
                   min = -100,
                   value = 0.82,
                   max = 100,
                   step = 1
                 ),
                 numericInput(
                   inputId = 'pay',
                   label = 'Custom pay growth (%)',
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
                 materialSwitch(
                   inputId = "activity_flag",
                   label = "Split by activity?", 
                   value = F,
                   status = "primary"
                 ),
                 hr(),
                 downloadButton("downloadData",
                                "Download",
                                style = "width:100%;")),
               mainPanel(
                 fluidRow(
                   column(12, 
                          dropdown(
                            tags$h4('Activity Inputs'),
                            circle = T,
                            style = 'unite',
                            status = 'danger',
                            icon = icon('hospital'),
                            width = '300px',
                            #item 1
                            radioGroupButtons(
                              inputId = "ae_growth",
                              label = "A&E",
                              choices = activity_growth_type,
                              justified = TRUE
                            ),
                            radioGroupButtons(
                              inputId = "elective_growth",
                              label = "Elective",
                              choices = activity_growth_type,
                              justified = TRUE
                            ),
                            radioGroupButtons(
                              inputId = "op_growth",
                              label = "Outpatients",
                              choices = activity_growth_type,
                              justified = TRUE
                            ),
                            radioGroupButtons(
                              inputId = "emergency_growth",
                              label = "Emergency",
                              choices = activity_growth_type,
                              justified = TRUE
                            ),
                            radioGroupButtons(
                              inputId = "gp_growth",
                              label = "General Practice",
                              choices = activity_growth_type,
                              justified = TRUE
                            )
                          ),
                          plotly::plotlyOutput('maingraph')),
                   column(12, plotly::plotlyOutput('subgraph')),
                   column(12, textOutput('growth_rate'))),
                 style='border: 0px')
               ))
    ))
               
# SERVER ------------------------------------------------------------------

server <- function(input,output,session){
  
  deflator_adj <- reactive({
    if(input$real_flag == F){
      deflator <- 0
    } else {
      deflator <- 1
    }
  })
  
  pop_adj <- reactive({
    if(input$per_capita == F){
      pop_final %>%
        mutate(values = 1) %>%
        select(!c(metric,X))
    } else
      pop_final %>%
      select(!c(metric,X)) %>%
      mutate(values = values / 1000000)
  })
  
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
               val_deflator = deflator ^ deflator_adj(),
               val_prod = cumprod(prod),
               val_pay = cumprod(pay)) %>%
      rowwise()%>%
      mutate(index = CreateIndex(a = val_a,
                                   prod=val_prod,
                                   pay=val_pay,
                                   deflator=val_deflator)) %>%
      filter(measure == 'cost') %>%
        #true filter
      filter(
          type == 'ae' & models == tolower(input$ae_growth) |
          type == 'emergency' & models == tolower(input$emergency_growth) |
          type == 'op' & models == tolower(input$op_growth) |
          type == 'gp' & models == tolower(input$gp_growth) |
          type == 'elective' & models == tolower(input$elective_growth)) %>%
      ungroup()%>%
      select(fyear,val_prod,val_pay,val_deflator,index) %>%
      left_join(.,supplement %>% filter(fyear %in% data_final$fyear),by=c('fyear')) %>%
      pivot_longer(cols=!c(fyear),names_to='names',values_to='values') %>%
      unique()
  })
  
  #data for budget activity
  data_baseline_activity <- reactive({
    data_final %>%
      group_by(models,measure,type) %>%
      mutate(prod = (100 + input$prod)/100,
             pay = (100 + input$pay)/100,
             val_deflator = deflator ^ deflator_adj(),
             val_prod = cumprod(prod),
             val_pay = cumprod(pay)
      ) %>%
      rowwise()%>%
      mutate(index = CreateIndex(a = val_a,
                                 prod=val_prod,
                                 pay=val_pay,
                                 deflator=val_deflator),
             final_value = baseline * (modelled_growth)*index)%>%
      filter(measure == 'cost') %>%
      #true filter
      filter(
        type == 'ae' & models == tolower(input$ae_growth) |
          type == 'emergency' & models == tolower(input$emergency_growth) |
          type == 'op' & models == tolower(input$op_growth) |
          type == 'gp' & models == tolower(input$gp_growth) |
          type == 'elective' & models == tolower(input$elective_growth)
      ) %>%
      group_by(fyear,type) %>%
      summarise(final_value_base = sum(final_value,na.rm=T))%>%
      left_join(.,pop_adj(),by='fyear') %>%
      mutate(final_value_base=final_value_base / values)
  })
  
  #data for baseline
  data_baseline <- reactive({
    data_final %>%
      group_by(models,measure,type) %>%
      mutate(prod = (100 + input$prod)/100,
             pay = (100 + input$pay)/100,
             val_deflator = deflator ^ deflator_adj(),
             val_prod = cumprod(prod),
             val_pay = cumprod(pay)
      ) %>%
      rowwise()%>%
      mutate(index = CreateIndex(a = val_a,
                                 prod=val_prod,
                                 pay=val_pay,
                                 deflator=val_deflator),
             final_value = baseline * (modelled_growth)*index) %>%
      filter(measure == 'cost') %>%
      #true filter
      filter(
        type == 'ae' & models == tolower(input$ae_growth) |
          type == 'emergency' & models == tolower(input$emergency_growth) |
          type == 'op' & models == tolower(input$op_growth) |
          type == 'gp' & models == tolower(input$gp_growth) |
          type == 'elective' & models == tolower(input$elective_growth)
      ) %>%
      group_by(fyear) %>%
      summarise(final_value_base = sum(final_value,na.rm=T))%>%
      left_join(.,pop_adj(),by='fyear') %>%
      mutate(final_value_base= final_value_base / values)
  })
  
  intensity_adj <- reactive({
    intensity <- (100 + input$intensity)
    return(intensity)
  })
  
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
               val_deflator = deflator ^ deflator_adj(),
               val_prod = cumprod(prod),
               val_pay = cumprod(pay)
        ) %>%
        rowwise()%>%
        mutate(index = CreateIndex(a = val_a,
                                   prod=val_prod,
                                   pay=val_pay,
                                   deflator=val_deflator),
               final_value = baseline * (modelled_growth)*index) %>%
      filter(measure == 'cost') %>%
      #true filter
      filter(
        type == 'ae' & models == tolower(input$ae_growth) |
          type == 'emergency' & models == tolower(input$emergency_growth) |
          type == 'op' & models == tolower(input$op_growth) |
          type == 'gp' & models == tolower(input$gp_growth) |
          type == 'elective' & models == tolower(input$elective_growth)
      ) %>%
      group_by(fyear) %>%
        summarise(final_value = sum(final_value,na.rm=T))%>%
        left_join(.,pop_adj(),by='fyear') %>%
        mutate(final_value=final_value / values)
    
    data_model2 <-  data_model1 %>%
      mutate(baseline_budget = (data_model1 %>% filter(fyear==2018))$final_value,
             budget_index = final_value / baseline_budget)
    
    return(data_model2)
    
    })
    
  #area in model variation from baseline
  data_ribbon <- reactive({
    
    data_baseline() %>% select(fyear,final_value_base) %>%
      left_join(.,data_model() %>% select(fyear,final_value),by=c('fyear'))
    
  })

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
    
  })
  
  output$growth_rate <- renderText({
      paste0('   *Average annualised growth rate is: ', 
            growth(),
            '%')
  })
  
  #graphs and outputs
  output$maingraph <- plotly::renderPlotly({
    
    if(input$activity_flag == F){
      plotly::ggplotly(ggplot()+
                         ggtitle("Shock adjusted budget prediction relative to baseline") +
                         geom_line(data = data_baseline(),aes(x=fyear,y=final_value_base/1e11),linetype = 2,col='#dd0031',alpha=1) +
                         geom_line(data=data_model(),aes(x=fyear,y=final_value/1e11),col='#dd0031') +
                         geom_ribbon(data=data_ribbon(),aes(x=fyear,ymin=final_value_base/1e11,ymax=final_value/1e11),fill='#dd0031',alpha=0.1)+
                         THFstyle::scale_colour_THF()+
                         theme_bw(base_size = 10) +
                         xlab('') +
                         ylab('Budget (£bn)') +
                         labs(fill='POD') + 
                         theme(legend.position="bottom")
      ) } else {
        plotly::ggplotly(ggplot()+
                           ggtitle("Budget prediction by activity split") + 
                           geom_col(data = data_baseline_activity(),aes(x=fyear,y=final_value_base/1e11,fill=type)) +
                           THFstyle::scale_fill_THF()+
                           theme_bw(base_size = 10) +
                           xlab('') +
                           ylab('Budget (£bn)') +
                           labs(fill='POD') + 
                           theme(legend.position="bottom"))
                       }
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
  
}
  
shinyApp(ui=ui,server=server)

