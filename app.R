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
pop1 <- data.table::fread('const/pop.csv')
pop2 <- pop1 %>%
  mutate(pop = 100*pop/(pop1 %>% filter(date==2023))$pop)

supplementary <- read.csv('const/data2.csv') %>%
  pivot_longer(cols=!fyear,names_to='metric',values_to='values')

supplementary2 <- supplementary %>%
  left_join(.,supplementary %>% filter(fyear=='2018-19') %>% select(!fyear) %>% rename('base'=values),by=c('metric')) %>%
  mutate(index = (100*values) / base ) %>%
  select(fyear,metric,index,values)

other_data <- read.csv('const/data1.csv') %>%
  mutate(pred_cross=cross_2018_age_sex_pred,
         pred_cross_all = cross_2018_all_vars_pred,
         panel_log=panel_all_vars_log_pred,
         panel_pred = panel_all_vars_pred) %>%
  select(outcome,fyear,pred_cross,pred_cross_all,panel_pred,panel_log) %>%
  pivot_longer(cols=!c(outcome,fyear),names_to='type',values_to='values')

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
                   choices = c('Budget','Budget Split','Supplementary'),
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
                   choices = c('Changing','Constant'),
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
                   value = F,
                   status = "primary"
                 ),
                 materialSwitch(
                   inputId = "per_capita",
                   label = "Make per capita?", 
                   value = F,
                   status = "primary"
                 ),
                 hr(),
                 downloadButton("downloadData",
                                "Download",
                                style = "width:100%;")),
               mainPanel(plotly::plotlyOutput('maingraph'),style='border: 0px'))),
    tabPanel('Model Outputs',
             fluid = T,
             icon=icon('map'),
             sidebarLayout(
               sidebarPanel(
                 titlePanel(h4(strong('MODEL OUTPUTS'),icon('map',class='about-icon fa-pull-left'))),
                 em('Explore various model outputs'),
                 hr(),
                 pickerInput(
                   inputId = "metric_filter",
                   label = "Select metrics of interest", 
                   choices = unique(other_data$outcome),
                   selected = ('apc_spell_cost_elective'),
                   multiple = TRUE
                 ),
                 pickerInput(
                   inputId = "metric_type",
                   label = "Select output type", 
                   choices = unique(other_data$type),
                   multiple = F
                 ),
                 hr(),
                 downloadButton("downloadData",
                                "Download",
                                style = "width:100%;")),
               mainPanel(plotly::plotlyOutput('other_graph'),style='border: 0px')))
    ))
    
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
      'max_prod'
    } else if(input$prod_name == 'Low (0.17%)'){
      'min_prod'
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
      cost_index <- CreateCostIndex(pay_name = pay_name(),
                    prod_name = prod_name(),
                    deflator_name = deflator_name(),
                    custom_prod = input$custom_prod,
                    custom_pay = input$custom_pay) %>%
      mutate(VAL_index = VAL_custom_index)}
    else {
      cost_index <- CreateCostIndex(pay_name = pay_name(),
                              prod_name = prod_name(),
                              deflator_name = deflator_name(),
                              custom_prod = input$custom_prod,
                              custom_pay = input$custom_pay)
    }
    return(cost_index)
  })
  
  data2 <- reactive({
    if(input$per_capita == T){
      data %>%
        dplyr::left_join(.,pop1,by=c('fyear'='date')) %>%
        mutate(pop = case_when(is.na(pop)==T~68,T~pop)) %>%
        mutate(predicted_budget_constant = predicted_budget_constant/(pop),
               predicted_budget_changing=predicted_budget_changing/(pop))
    } else {
      data
    }
  })
  
  graph_budget_data <- reactive({data2() %>%
      left_join(cost_index(),by=c('fyear')) %>%
      mutate(predicted_budget_constant = (VAL_index/100)*predicted_budget_constant,
             predicted_budget_changing = (VAL_index/100)*predicted_budget_changing) %>%
      pivot_longer(cols=c(starts_with('predicted_budget')),names_to='budget_type',values_to='budget_value')})
  
  budget_data_low <- reactive({
    data2() %>%
      left_join(CreateCostIndex('min_nhs_pay','max_prod','deflator',custom_prod = input$custom_prod,custom_pay = input$custom_pay),by=c('fyear')) %>%
      mutate(predicted_budget_constant = (VAL_index/100)*predicted_budget_constant,
             predicted_budget_changing = (VAL_index/100)*predicted_budget_changing) %>%
      group_by(fyear) %>%
      summarise(predicted_budget_constant = sum(predicted_budget_constant,na.rm=T),
                predicted_budget_changing = sum(predicted_budget_changing,na.rm=T))})
  
  budget_data_high <- reactive({
    data2() %>%
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
  
  output$other_graph <- plotly::renderPlotly({
    plotly::ggplotly(
      ggplot(data=other_data %>% 
               filter(outcome %in% input$metric_filter & type == input$metric_type))+
        geom_line(aes(x=fyear,y=values,col=outcome)) +
        THFstyle::scale_colour_THF() +
        theme_bw(base_size=12)+
        xlab('')+
        ylab('Index (2018/19) = 100')
      )
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
                           theme(legend.position="bottom")) %>%
          plotly::layout(annotations = list(
            x=2020, 
            y=125,
            text = paste('Average growth rate:',
                         round(100*(
                           (
                           (graph_budget_data() %>% 
                           group_by(fyear,budget_type) %>% 
                           summarise(budget_value=sum(budget_value)) %>% 
                           filter(budget_type==budget_select())%>%
                           filter(fyear == 2035))$budget_value - 
                            
                             (graph_budget_data() %>% 
                               group_by(fyear,budget_type) %>% 
                               summarise(budget_value=sum(budget_value)) %>% 
                               filter(budget_type==budget_select())%>%
                               filter(fyear == 2018))$budget_value
                           
                           )/
                            (
                              ((graph_budget_data() %>% 
                               group_by(fyear,budget_type) %>% 
                               summarise(budget_value=sum(budget_value)) %>% 
                               filter(budget_type==budget_select())%>%
                               filter(fyear == 2018))$budget_value)
                              *
                                17)),1)
                         ,'%'), 
            showarrow = F))
      } else {
        plotly::ggplotly(ggplot(data=supplementary2 %>%
                                  mutate(fyear = as.numeric(substr(fyear,1,4))))+
                           #chosen pick
                           geom_line(aes(x=fyear,y=index,values=values,col=metric)) +
                           THFstyle::scale_colour_THF()+
                           theme_bw(base_size = 12) +
                           xlab('') +
                           ylab('Factor (2018/19) = 100)'),
                         tooltip = c("fyear", "values")
        )
      }
    }
  )
}
  
shinyApp(ui=ui,server=server)

