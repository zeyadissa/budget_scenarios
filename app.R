library(tidyverse)
library(shinythemes)
library(shiny)
library(shinyWidgets)
library(fontawesome)
library(tidyverse)
library(devtools)

CreateIndex <- function(a,prod,pay,deflator){
  1 + a*(pay-deflator) - (1-a)*(prod-1)
}

FINAL_deflator <- read.csv('const/FINAL_deflator.csv')
  
data_final <- read.csv('const/FINAL_DATA.csv') %>%
  select(!c(deflator,X)) %>%
  left_join(.,FINAL_deflator %>% select(!c(deflator,X)) %>% mutate(fyear=as.integer(fyear)),by=c('fyear')) %>%
  rename(deflator = 'index_deflator')

pop_final <- read.csv('const/FINAL_POP.csv')

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
    intensity_final <- ifelse(intensity_final == 1, (100 + val_prod)/100,intensity_final)

    df2 <- df %>% dplyr::mutate(
      prod = case_when(
        fyear == min(shock_range)  ~ ((100 + val_prod)*inten)/10000,
        fyear > min(shock_range) & fyear <= max(shock_range) ~ intensity_final,
        T ~ (100 + val_prod)/100))
    
    return(df2)
  }
  return(df2)
  
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
activity_growth_type <- c('Changing','Constant','Log')
shock_types <- c('Permanent','U-Shaped')

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
                 em('Create and vary budgetary scenarios through productivity and wage levers'),
                 hr(),
                 radioGroupButtons(
                   inputId = "growth_name",
                   label = "Activity growth type",
                   choices = activity_growth_type,
                   justified = TRUE
                 ),
                 numericInput(
                   inputId = 'prod',
                   label = 'Custom productivity CAGR (%)',
                   min = -100,
                   value = 1,
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
                 materialSwitch(
                   inputId = "shock",
                   label = "Create shock?", 
                   value = F,
                   status = "primary"
                 ),
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
                   selected = c(2019,2021)
                 ),
                 hr(),
                 downloadButton("downloadData",
                                "Download",
                                style = "width:100%;")),
               mainPanel(
                 fluidRow(
                   column(12, plotly::plotlyOutput('maingraph')),
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
  
  model_select <- reactive({
    tolower(input$growth_name)
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
  
  data_index <- reactive({

      data_final %>%
      CreateShock(.,
                  shock_type=input$shock_type,
                  shock_range = input$range,
                  val_prod = input$prod,
                  inten = intensity_adj()) %>%
        group_by(type,measure,models) %>%
        mutate(pay = (100 + input$pay)/100,
               val_deflator = deflator ^ deflator_adj(),
               val_prod = cumprod(prod),
               val_pay = cumprod(pay)
        ) %>%
        rowwise()%>%
        mutate(index = CreateIndex(a = val_a,prod=val_prod,pay=val_pay,deflator=val_deflator),
               final_value = baseline * (modelled_growth)*index)%>%
        group_by(fyear,measure,models) %>%
        filter(measure == 'cost' & models == model_select()) %>%
        ungroup()%>%
        select(fyear,val_prod,val_pay,val_deflator,index) %>%
        unique() %>%
        pivot_longer(cols=!c(fyear),names_to='names',values_to='values')

  })
  
  data_baseline <- reactive({
    data_final %>%
      group_by(type,measure,models) %>%
      mutate(prod = (100 + input$prod)/100,
             pay = (100 + input$pay)/100,
             val_deflator = deflator ^ deflator_adj(),
             val_prod = cumprod(prod),
             val_pay = cumprod(pay)
      ) %>%
      rowwise()%>%
      mutate(index = CreateIndex(a = val_a,prod=val_prod,pay=val_pay,deflator=val_deflator),
             final_value = baseline * (modelled_growth)*index)%>%
      group_by(fyear,measure,models) %>%
      summarise(final_value_base = sum(final_value,na.rm=T))%>%
      filter(measure == 'cost' & models == model_select())  %>%
      left_join(.,pop_adj(),by='fyear') %>%
      mutate(final_value_base=final_value_base / values)
  })
  
  intensity_adj <- reactive({
    if(input$shock == F){
      intensity <- 100
    } else {
      intensity <- (100 + input$intensity)
    }
    return(intensity)
  })
  
  data_model <- reactive({
    data_final %>%
      CreateShock(.,
                  shock_type = input$shock_type,
                  shock_range = input$range,
                  val_prod = input$prod,
                  inten = intensity_adj()) %>%
        group_by(type,measure,models)  %>%
        mutate(pay = (100 + input$pay)/100,
               val_deflator = deflator ^ deflator_adj(),
               val_prod = cumprod(prod),
               val_pay = cumprod(pay)
        ) %>%
        rowwise()%>%
        mutate(index = CreateIndex(a = val_a,prod=val_prod,pay=val_pay,deflator=val_deflator),
               final_value = baseline * (modelled_growth)*index) %>%
        group_by(fyear,measure,models) %>%
        summarise(final_value = sum(final_value,na.rm=T))%>%
        filter(measure == 'cost' & models == model_select())%>%
        left_join(.,pop_adj(),by='fyear') %>%
        mutate(final_value=final_value / values)
    })
    
  data_ribbon <- reactive({
    
    data_baseline() %>% select(fyear,measure,models,final_value_base) %>%
      left_join(.,data_model() %>% select(fyear,measure,models,final_value),by=c('fyear','measure','models'))
    
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
      plotly::ggplotly(ggplot()+
                         geom_line(data = data_baseline(),aes(x=fyear,y=final_value_base/1e11),linetype = 2,col='#dd0031',alpha=1) +
                         geom_line(data=data_model(),aes(x=fyear,y=final_value/1e11),col='#dd0031') +
                         geom_ribbon(data=data_ribbon(),aes(x=fyear,ymin=final_value_base/1e11,ymax=final_value/1e11),fill='#dd0031',alpha=0.1)+
                         THFstyle::scale_colour_THF()+
                         theme_bw(base_size = 12) +
                         xlab('') +
                         ylab('Budget (£bn)') +
                         labs(fill='POD') + 
                         theme(legend.position="bottom")
                       )
      })
  
  output$subgraph <- plotly::renderPlotly({
    plotly::ggplotly(
      ggplot2::ggplot()+
        geom_line(data = data_index(),aes(x=fyear,y=values,col=names)) +
        geom_point(data = data_index(),aes(x=fyear,y=values,col=names)) +
        THFstyle::scale_colour_THF()+
        theme_bw(base_size = 12) +
        xlab('') +
        ylab('Index (2017 = 100)') +
        labs(col='Measure') + 
        theme(legend.position="bottom")

    )
  })
  
}
  
shinyApp(ui=ui,server=server)

