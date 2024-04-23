library(tidyverse)
library(shinythemes)
library(shiny)
library(shinyWidgets)
library(fontawesome)
library(tidyverse)
library(devtools)

source('src/cost_index.R')

# Global Variables --------------------------------------------------------

#These are the controls for the zoom: they are needed to set the boundary to UK
THEME <- 'simplex'
data <- data.table::fread('const/predicted_cwa_budget.csv') %>%
  filter(fyear < 2036)
devtools::install_github('THF-evaluative-analytics/THFstyle')

productivity_data <- data.table::fread('const/productivity_data.csv')
pay_data <- data.table::fread('const/pay_data.csv')

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
                   inputId = "type",
                   label = "Select Graph Type",
                   choices = c('Budget','Budget Split'),
                   justified = TRUE
                 ),
                 hr(),
                 radioGroupButtons(
                   inputId = "pay_name",
                   label = "Pay growth above inflation",
                   choices = c('High (+2%)','Central (0%)','Low (-1%)'),
                   justified = TRUE
                 ),
                 radioGroupButtons(
                   inputId = "prod_name",
                   label = "Productivity growth",
                   choices = c('High (0.93%)','Central (0.58%)','Low (0.17%)'),
                   justified = TRUE
                 ),
                 radioGroupButtons(
                   inputId = "growth_name",
                   label = "Activity growth type",
                   choices = c('Constant','Changing'),
                   justified = TRUE
                 ),
                 numericInput(
                   inputId = 'custom_prod',
                   label = 'Custom productivity CAGR (%)',
                   min = -20,
                   value = 0.571,
                   max = 20,
                   step = 0.01
                 ),
                 numericInput(
                   inputId = 'custom_pay',
                   label = 'Custom pay growth (%)',
                   min = -100,
                   value = 0,
                   max = 100,
                   step = 0.01
                 ),
                 materialSwitch(
                   inputId = "real_flag",
                   label = "Real terms?", 
                   value = TRUE,
                   status = "primary"
                 ),
                 materialSwitch(
                   inputId = "custom",
                   label = "Custom growth rates?", 
                   value = TRUE,
                   status = "primary"
                 ),
                 hr(),
                 downloadButton("downloadData",
                                "Download",
                                style = "width:100%;")),
               mainPanel(plotly::plotlyOutput('maingraph'),style='border: 0px')))))
    
# SERVER ------------------------------------------------------------------

server <- function(input,output,session){
  
  pay_name <- reactive({
    if(input$pay_name == 'High (+2%)'){
      'max_nhs_pay'
    } else if(input$pay_name == 'Low (-1%)'){
      'min_nhs_pay'
    } else {
      'central_nhs_pay'
    }
  })
  
  prod_name <- reactive({
    if(input$prod_name == 'High (0.93%)'){
      'min_prod'
    } else if(input$prod_name == 'Low (0.17%)'){
      'max_prod'
    } else {
      'central_prod'
    }
  })
  
  deflator_name <- reactive({
    if(input$real_flag == T){
      'deflator'
    } else {
      'non_deflator'
    }
  })
  
  cost_index <- reactive({
    if(input$custom == T){
    data <- CreateCostIndex(pay_name = pay_name(),
                    prod_name = prod_name(),
                    deflator_name = deflator_name(),
                    custom_prod = input$custom_prod,
                    custom_pay = input$custom_pay) %>%
      mutate(VAL_index = VAL_custom_index)}
    else {
      data <- CreateCostIndex(pay_name = pay_name(),
                              prod_name = prod_name(),
                              deflator_name = deflator_name(),
                              custom_prod = input$custom_prod,
                              custom_pay = input$custom_pay)
    }
    return(data)
  })
  
  graph_budget_data <- reactive({data %>%
      left_join(cost_index(),by=c('fyear')) %>%
      mutate(predicted_budget_constant = (VAL_index/100)*predicted_budget_constant,
             predicted_budget_changing = (VAL_index/100)*predicted_budget_changing) %>%
      pivot_longer(cols=c(starts_with('predicted_budget')),names_to='budget_type',values_to='budget_value')})
  
  budget_data_low <- reactive({
    data %>%
      left_join(CreateCostIndex('min_nhs_pay','max_prod','deflator',custom_prod = input$custom_prod,custom_pay = input$custom_pay),by=c('fyear')) %>%
      mutate(predicted_budget_constant = (VAL_index/100)*predicted_budget_constant,
             predicted_budget_changing = (VAL_index/100)*predicted_budget_changing) %>%
      group_by(fyear) %>%
      summarise(predicted_budget_constant = sum(predicted_budget_constant,na.rm=T),
                predicted_budget_changing = sum(predicted_budget_changing,na.rm=T))})
  
  budget_data_high <- reactive({
    data %>%
      left_join(CreateCostIndex('max_nhs_pay','min_prod','deflator',custom_prod = input$custom_prod,custom_pay = input$custom_pay),by=c('fyear')) %>%
      mutate(predicted_budget_constant = (VAL_index/100)*predicted_budget_constant,
             predicted_budget_changing = (VAL_index/100)*predicted_budget_changing) %>%
      group_by(fyear) %>%
      summarise(predicted_budget_constant = sum(predicted_budget_constant,na.rm=T),
                predicted_budget_changing = sum(predicted_budget_changing,na.rm=T))})
  
  budget_select <- reactive({
    if(input$growth_name == 'Constant'){
      return('predicted_budget_constant')
    } else {
      return('predicted_budget_changing')
    }
  })
  
  activity_select <- reactive({
    if(input$growth_name == 'Constant'){
      return('predicted_activity_constant')
    } else {
      return('predicted_activity_changing')
    }
  })
  
  output$maingraph <- plotly::renderPlotly(
    {
      if(input$type == 'Budget Split'){
      plotly::ggplotly(ggplot(data=(graph_budget_data() %>% filter(budget_type == budget_select())))+
        geom_col(aes(x=fyear,y=budget_value/1e9,fill=type)) +
        THFstyle::scale_fill_THF()+
        theme_bw(base_size = 12) +
        xlab('') +
        ylab('Budget (£bn)') +
        labs(fill='POD') + 
        theme(legend.position="bottom"))
      } else if(input$type == 'Budget'){
        plotly::ggplotly(ggplot()+
                           #chosen pick
                           geom_line(data=graph_budget_data() %>% 
                                       group_by(fyear,budget_type) %>% 
                                       summarise(budget_value=sum(budget_value)) %>% 
                                       filter(budget_type==budget_select()),
                                     aes(x=fyear,y=budget_value/1e9),col='#2a7979') +
                           geom_line(data=budget_data_high(),aes(x=fyear,y=predicted_budget_changing/1e9),linetype=2,col='#dd0031') +
                           geom_line(data=budget_data_low(),aes(x=fyear,y=predicted_budget_changing/1e9),linetype=2,col='#dd0031') +
                           THFstyle::scale_colour_THF()+
                           theme_bw(base_size = 12) +
                           xlab('') +
                           ylab('Budget (£bn)') +
                           labs(fill='POD') + 
                           theme(legend.position="bottom"))
      } else {
        plotly::ggplotly(ggplot()+
                           #chosen pick
                           geom_col(data=graph_activity_data() %>% filter(activity_type == activity_select()),aes(x=fyear,y=activity_value/1e6,fill=type)) +
                           THFstyle::scale_fill_THF()+
                           theme_bw(base_size = 12) +
                           xlab('') +
                           ylab('Cost-weighted activity (mn)') +
                           labs(fill='POD') + 
                           theme(legend.position="bottom"))
      }
    }
  )
}
  
shinyApp(ui=ui,server=server)

