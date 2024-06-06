library(tidyverse)
library(shinythemes)
library(shiny)
library(shinyWidgets)
library(devtools)
library(bslib)
library(bsicons)

devtools::install_github("THF-evaluative-analytics/THFstyle")

source("const/glob.R")
source("ui/sidebarUI.R")
source("ui/activityUI.R")
source("ui/indexUI.R")
source("ui/growthUI.R")
source('ui/mainPanelUI.R')

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    # Note the wrapping of the string in HTML()
    tags$style(HTML(".col-sm-8 {
        border: 0px;
        padding-left: 0px; 
        padding-right: 0px;
      }"))
  ),
  # Determines theme
  theme = shinythemes::shinytheme(THEME),
  br(),
  sidebarLayout(
    #sidebar panel
    sidebarPanel(
      style = "height: 90vh; overflow-y: auto;", 
      titlePanel(h4(strong("BUDGET SCENARIOS"), icon("house", class = "about-icon fa-pull-left"))),
      hr(),
      sidebarUI("var"),
      width = 3),
    #main panel
    mainPanelUI()
  ),
  style = "font-size: 15px"
)

# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  
  pop_adj <- reactive({
    if (input$per_capita == F) {
      pop_final %>%
        mutate(values = 1,
               index = 100)
    } else {
      pop_final %>%
        mutate(values = values / 1000000)
    }
  })

  intensity_adj <- reactive({
    intensity <- (100 + input$intensity)
    return(intensity)
  })

  # Calculated values
  modelled_growth <- reactive({
    df <- data_final_a %>%
      # true filter
      filter(
        type == "A&E" & models == tolower(input$ae_growth) |
          type == "Elective" & models == tolower(input$elective_growth) |
          type == "Emergency" & models == tolower(input$emergency_growth) |
          type == "Prescribing" & models == tolower(input$prescribing_growth) |
          type == "Outpatients" & models == tolower(input$outpatients_growth) |
          type == "General Practice" & models == tolower(input$general_practice_growth) |
          type == "Community" & models == tolower(input$community_growth) |
          type == "Specialised" & models == tolower(input$specialised_growth) |
          type == "Mental Health" & models == tolower(input$mental_health_growth) |
          type == "Maternity" & models == tolower(input$maternity_growth) |
          type == "Other"
      ) %>%
      mutate(val = baseline * modelled_growth) %>%
      group_by(fyear) %>%
      summarise(val = sum(val, na.rm = T))

    growth <- as.numeric(((df %>% filter(fyear == max(fyear)))$val /
      (df %>% filter(fyear == min(fyear)))$val)^(1 / length(unique(data_final_a$fyear))))

    return(growth)
  })
  
  mhcomm_growth <- reactive({
    df <- data_final_a %>%
      filter(type != 'Mental Health') %>%
      # true filter
      filter(
        type == "A&E" & models == tolower(input$ae_growth) |
          type == "Elective" & models == tolower(input$elective_growth) |
          type == "Emergency" & models == tolower(input$emergency_growth) |
          type == "Prescribing" & models == tolower(input$prescribing_growth) |
          type == "Outpatients" & models == tolower(input$outpatients_growth) |
          type == "General Practice" & models == tolower(input$general_practice_growth) |
          type == "Community" & models == tolower(input$community_growth) |
          type == "Specialised" & models == tolower(input$specialised_growth) |
          type == "Maternity" & models == tolower(input$maternity_growth) |
          type == "Other"
      ) %>%
      mutate(val = baseline * modelled_growth) %>%
      group_by(fyear) %>%
      summarise(val = sum(val, na.rm = T))
    
    growth <- as.numeric(((df %>% filter(fyear == max(fyear)))$val /
                            (df %>% filter(fyear == min(fyear)))$val)^(1 / length(unique(data_final_a$fyear)))) - 1
    
    return(growth)
  })

  data_final <- reactive({
    data_final2 <- rbind(
      data_final_a,
      expand_grid(
        "type" = "Other",
        "measure" = "cwa",
        "models" = "Other",
        "fyear" = c(base_year:max_year),
        "modelled_growth" = 1,
        "baseline" = 11314650700,
        "w" = 0,
        "d" = 0,
        "r" = 0
      ) %>%
        left_join(., FINAL_deflator %>%
          rename(
            real_deflator = "deflator",
            deflator = "index_deflator"
          ),
        by = "fyear"
        ) %>%
        mutate(modelled_growth = modelled_growth()^(fyear - base_year))
    )
    return(data_final2)
  })

  pay_name <- reactive({
    if(input$pay_growth_scenario == 'Recovery to 2018'){
      '5-year'
    } else if(input$pay_growth_scenario == 'Recovery to 2010'){
      '10-year'
    } else {
      input$pay_growth_scenario
    }
  })
  
  base_data1 <- reactive({
    
    data.table::setDT(FINAL_deflator)
    
    data_final() %>%
      CreateShock(.,
        shock_type = input$shock_type,
        shock_range = input$range,
        val_prod = input$prod,
        inten = intensity_adj()
      ) %>%
      group_by(type, measure, models) %>%
      mutate(
        pay = (100 + input$pay) / 100,
        drug = (100 + input$drug) / 100) %>%
      {if(input$growth_scenarios == T)
              select(.,!c(pay,prod)) %>%
                left_join(.,
                        growth_scenarios %>%
                          ungroup() %>%
                          select(fyear,scenario,prod) %>%
                          filter(scenario == input$productivity_growth_scenario) %>%
                          select(!scenario),
                        by=c('fyear')) %>%
                left_join(.,
                          growth_scenarios %>%
                            select(fyear,scenario,pay) %>%
                            filter(scenario == pay_name()) %>%
                            ungroup() %>%
                            select(!scenario),
                          by=c('fyear'))
        else .} %>%
      mutate(
        pay = case_when(models == 'Policy: Recovery (5-year)' & fyear <= 2030 & fyear != 2018 ~ 1+((pay-1) * 1.0625),
                        T ~ pay)) %>%
      mutate(
        drug = case_when(fyear == min(fyear) ~ 1,
                         T ~ drug),
        prod = case_when(fyear == min(fyear) ~ 1,
                         T ~ prod),
        pay = case_when(fyear==min(fyear) ~ 1,
                        T ~ pay),
        val_drug = cumprod(drug),
        val_deflator = (FINAL_deflator[fyear == input$base_yr,index_deflator]/FINAL_deflator[fyear == base_year,index_deflator]),
        val_prod = cumprod(prod),
        val_pay = cumprod(pay)) %>%
      rowwise() %>%
      mutate(
        index = CreateIndex(
          w = w,
          d = d,
          r = r,
          prod = val_prod,
          pay = val_pay,
          drug = val_drug
        ),
        pay_index = CreatePayIndex(
          w = w,
          d = d,
          r = r,
          prod = val_prod,
          pay = val_pay,
          drug = val_drug
        ),
        drugs_index = CreateDrugIndex(
          w = w,
          d = d,
          r = r,
          prod = val_prod,
          pay = val_pay,
          drug = val_drug
        )
      ) %>%
      CreateCommunityMHData(.,mhcomm_growth()) %>%
      mutate(
        modelled_growth_inverse = case_when(
          modelled_growth - 1 == 0 ~ 1,
          T ~ modelled_growth - 1),
        final_value = baseline * (modelled_growth) * index * val_deflator,
        final_value_no_cost_index = baseline * (modelled_growth_inverse) * val_deflator,
        final_value_no_cost_index2 = baseline * (modelled_growth) * val_deflator,
        final_cost_value = final_value - final_value_no_cost_index2,
        final_drugs_value =  (baseline * (modelled_growth) * (drugs_index-1) * val_deflator),
        final_pay_value = (baseline * (modelled_growth) * (pay_index-1) * val_deflator),
        final_prod_value = final_cost_value - final_drugs_value - final_pay_value
      )
  })

  base_data <- reactive({
    base_data1() %>%
      filter(
        type == "A&E" & models == (input$ae_growth) |
          type == "Elective" & models == (input$elective_growth) |
          type == "Emergency" & models == (input$emergency_growth) |
          type == "Prescribing" & models == (input$prescribing_growth) |
          type == "Outpatients" & models == (input$outpatients_growth) |
          type == "General Practice" & models == (input$general_practice_growth) |
          type == "Community" & models == (input$community_growth) |
          type == "Specialised" & models == (input$specialised_growth) |
          type == "Mental Health" & models == (input$mental_health_growth) |
          type == "Maternity" & models == (input$maternity_growth) |
          type == "Other"
      )
  })
  
  # data for index
  data_index <- reactive({
    base_data() %>%
      ungroup() %>%
      select(fyear,val_prod, val_drug, val_pay, val_deflator) %>%
      left_join(., supplement %>% filter(fyear %in% data_final()$fyear), by = c("fyear")) %>%
      rename(
        Productivity = "val_prod",
        `Hospital Drugs` = "val_drug",
        Pay = "val_pay",
        Receipts = "receipt_index",
        GDP = "gdp_index",
        Deflator = "val_deflator"
      ) %>%
      pivot_longer(cols = !c(fyear), names_to = "names", values_to = "values") %>%
      filter(names %in% input$index_names) %>%
      left_join(.,pop_adj() %>% select(fyear,index),by='fyear') %>%
      mutate(values = values/(index/100))
  })

  # data for baseline
  data_model <- reactive({
    data_model1 <- base_data() %>%
      group_by(fyear) %>%
      summarise(final_value = sum(final_value, na.rm = T)) %>%
      left_join(., pop_adj(), by = "fyear") %>%
      mutate(final_value = final_value / values)

    data_model2 <- data_model1 %>%
      mutate(
        budget_index = final_value / (data_model1 %>% filter(fyear == 2018))$final_value
      )

    return(data_model2)
  })

  baseline_data <- reactive({
    
    data_baseline %>%
      left_join(.,pop_adj(),by='fyear') %>%
      mutate(final_value_base = final_value/values)
    
  })
  
  # area in model variation from baseline
  data_ribbon <- reactive({
    baseline_data() %>%
      select(fyear, final_value_base) %>%
      left_join(., data_model() %>% 
                  select(fyear, final_value),
                by = c("fyear")
      )
  })

  growth <- reactive({
    
    gro_df <- data.table::setDT(data_model())
    round(100 * ((gro_df[fyear == max(fyear)]$final_value)/gro_df[fyear == min(gro_df$fyear)]$final_value)^(1/(length(unique(gro_df$fyear))-1)) - 100, 2)
  })

  # data for model
  data_waterfall <- reactive({
    
    df1 <- base_data1() %>%
      # true filter
      mutate(
        Pay = (final_pay_value),
        Drugs = (final_drugs_value),
        Productivity = (final_prod_value)
      ) %>%
      ungroup()
  })
  
  base_waterfall <- reactive({
    
    df1 <- data_waterfall() %>%
      ungroup() %>%
      filter(fyear == input$water_year) %>%
      filter(
        case_when(input$water_type == 'Total' ~ type %in% type,
                  T ~ type == input$water_type)) %>%
      select(type,models,fyear,Pay,Drugs,Productivity,final_value_no_cost_index)
    
    df2 <- df1 %>%
      filter(
        type == "A&E" & models == (input$ae_growth) |
          type == "Elective" & models == (input$elective_growth) |
          type == "Emergency" & models == (input$emergency_growth) |
          type == "Prescribing" & models == (input$prescribing_growth) |
          type == "Outpatients" & models == (input$outpatients_growth) |
          type == "General Practice" & models == (input$general_practice_growth) |
          type == "Community" & models == (input$community_growth) |
          type == "Specialised" & models == (input$specialised_growth) |
          type == "Mental Health" & models == (input$mental_health_growth) |
          type == "Maternity" & models == (input$maternity_growth) |
          type == "Other"
      ) %>%
      select(type,fyear,Pay,Drugs,Productivity) %>%
      pivot_longer(cols=!c('type','fyear'),names_to='models',values_to='final_value_no_cost_index')
    
    df3 <- rbind(df1 %>% select(type,fyear,models,final_value_no_cost_index),df2) %>%
      select(!fyear) %>%
      mutate(final_value_no_cost_index = round(final_value_no_cost_index / 1e9,2))
    
    filter_val_tot <- function(x){
      case_when(
        x == "A&E" ~ (input$ae_growth),
        x == "Elective" ~ (input$elective_growth),
        x == "Emergency" ~ (input$emergency_growth),
        x == "Prescribing" ~ (input$prescribing_growth),
        x == "Outpatients" ~ (input$outpatients_growth),
        x == "General Practice" ~ (input$general_practice_growth),
        x == "Community" ~ (input$community_growth),
        x == "Specialised" ~ (input$specialised_growth),
        x == "Mental Health" ~ (input$mental_health_growth),
        x == "Maternity" ~ (input$maternity_growth),
        T ~ "Other"
    )}
    
    df4 <- lapply(
      unique(df3$type),
      function(x){

        df3 %>%
          filter(type == x) %>%
          filter(
            case_when(
              grepl(pattern = "Log", filter_val_tot(x)) == T ~ grepl(pattern = "Linear|Recovery", models) == F,
              grepl(pattern = "Linear", filter_val_tot(x)) == T ~ grepl(pattern = "Log|Recovery", models) == F,
              filter_val_tot(x) == 'Policy: Recovery (10-year)' ~ models %in% c('Demography','Morbidity','Policy: Recovery (10-year)',"Pay", "Productivity", "Drugs"),
              filter_val_tot(x) == 'Policy: Recovery (5-year)' ~ models %in% c('Demography','Morbidity','Policy: Recovery (5-year)',"Pay", "Productivity", "Drugs"),
              filter_val_tot(x) == 'Policy: Recovery' ~ grepl(pattern = "Log|Linear", models) == F,
              grepl(pattern = "Demography", filter_val_tot(x)) == T ~ models %in% c('Demography',"Pay", "Productivity", "Drugs"),
              grepl(pattern = "Morbidity", filter_val_tot(x)) == T ~ models %in% c('Demography','Morbidity',"Pay", "Productivity", "Drugs"),
              grepl(pattern = "medium", filter_val_tot(x)) == T ~ models %in% c('lower','medium',"Pay", "Productivity", "Drugs"),
              grepl(pattern = "upper", filter_val_tot(x)) == T ~ models %in% c('lower','medium',"Pay", "Productivity", "Drugs"),
              grepl(pattern = "lower", filter_val_tot(x)) == T ~ models %in% c('lower',"Pay", "Productivity", "Drugs"),
              T ~ models %in% c('Resilience','Other',"Pay", "Productivity", "Drugs")
            )
          ) %>%
          mutate(
            group = case_when(
              models %in% c("Pay", "Productivity", "Drugs",'Resilience') ~ 'ungroup',
              T ~ 'group'
          )) %>%
          group_by(type,group) %>%
          arrange(final_value_no_cost_index) %>%
          mutate(
            lag_val = lag(final_value_no_cost_index,default = 0),
            final_value_no_cost_index_a =  final_value_no_cost_index - lag_val) %>%
          mutate(
            final_value_no_cost_index = case_when(
              group == 'ungroup' ~ final_value_no_cost_index,
              T ~ final_value_no_cost_index_a))
      }) %>%
      data.table::rbindlist()
    
    df5 <- df4 %>%
      mutate(models = case_when(
        models == 'Demography' ~ 'Demographics',
        models %in% c('Log growth','Linear growth') ~ 'Policy',
        models %in% c('lower','upper','medium') ~ 'Policy',
        models %in% c('Policy: Recovery','Policy: Recovery (5-year)','Policy: Recovery (10-year)') ~ 'Policy',
        T ~ models
      )) %>%
      group_by(models) %>%
      summarise(final_value_no_cost_index=sum(final_value_no_cost_index,na.rm=T))
    
    return(df5)
    
  })
  
  type_waterfall <- reactive({
    base_data() %>%
      filter(fyear == input$water_year) %>%
      filter(
        type == "A&E" & models == (input$ae_growth) |
          type == "Elective" & models == (input$elective_growth) |
          type == "Emergency" & models == (input$emergency_growth) |
          type == "Prescribing" & models == (input$prescribing_growth) |
          type == "Outpatients" & models == (input$outpatients_growth) |
          type == "General Practice" & models == (input$general_practice_growth) |
          type == "Community" & models == (input$community_growth) |
          type == "Specialised" & models == (input$specialised_growth) |
          type == "Mental Health" & models == (input$mental_health_growth) |
          type == "Maternity" & models == (input$maternity_growth) |
          type == "Other"
      ) %>%
      # NOTE TO SELF: THIS MAY BE MUCKING THINGS UP. CHECK
      tidyr::drop_na() %>%
      select(type, final_value) %>%
      mutate(final_value = round(final_value / 1e9, 2))
  })
  
  # OUTPUTS
  
  output$waterfall_graph <- renderPlot({
    waterfalls::waterfall(base_waterfall(), calc_total = TRUE, total_rect_color = "orange", rect_text_size = 2,total_rect_text_color = "black") +
      theme_bw(base_size = 16) +
      xlab("") +
      ylab("") +
      THFstyle::scale_fill_THF() +
      ggtitle(paste0("Growth in ",input$water_type,' in ',input$water_year,' relative to 2018')) +
      theme(axis.text=element_text(size=14))
  }) %>%
    bindEvent(input$run)
  
  output$waterfall_graph2 <- renderPlot({
    waterfalls::waterfall(type_waterfall(), calc_total = TRUE, total_rect_color = "orange", rect_text_size = 2,total_rect_text_color = "black")+
      theme_bw(base_size = 16) +
      xlab("") +
      ylab("") +
      THFstyle::scale_fill_THF() +
      ggtitle(paste0('Total NHSE budget by POD in ',input$water_year)) +
      theme(axis.text=element_text(size=14))
  })%>%
    bindEvent(input$run)

  # graphs and outputs
  output$maingraph <- plotly::renderPlotly({
    plotly::ggplotly(
      ggplot() +
        ggtitle(paste0("NHSE Budget (£) under a ", growth(), "% growth rate")) +
        geom_line(data = baseline_data(), aes(x = fyear, y = final_value_base / 1e9), linetype = 2, col = "#dd0031", alpha = 1) +
        geom_line(data = data_model(), aes(x = fyear, y = final_value / 1e9), col = "#dd0031") +
        geom_ribbon(data = data_ribbon(), aes(x = fyear, ymin = final_value_base / 1e9, ymax = final_value / 1e9), fill = "#dd0031", alpha = 0.1) +
        THFstyle::scale_colour_THF() +
        theme_bw(base_size = base_font) +
        xlab("") +
        ylab("Budget (£bn)") +
        labs(fill = "POD") +
        theme(legend.position = "bottom")
    )
  })%>%
    bindEvent(input$run)

  output$download <-  downloadHandler(
    filename = 'data.csv',
    content = function(file){
      write.csv(base_data(), file)
    }
  )

  output$growth_table <- renderTable({
    
    df <- data.table::setDT(data_model())
    
    data <- tidyr::expand_grid(
      'Start Year' = as.integer(c(2018,2024,2029,2034)),
      'End Year' = as.integer(c(2018,2024,2029,2034))
    ) %>%
      filter(`Start Year` < `End Year`) %>%
      rowwise() %>%
      mutate(
        `Growth Rate` = 
          100*((df[fyear == `End Year`,final_value] / df[fyear ==  `Start Year`,final_value]) ^ (1/( `End Year`- `Start Year`)) - 1)
      )
    
  })
  
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "",
      text = tags$span(
        tableOutput("growth_table")
      ),
      html = T,
      type = "info",
      width = '25%'
    )
  })
  
  observeEvent(input$scen_a,{
    updateSelectInput(
      inputId = 'ae_growth',
      selected = 'Linear growth'
    )
    updateSelectInput(
      inputId = 'elective_growth',
      selected = 'Linear growth'
    )
    updateSelectInput(
      inputId = 'outpatients_growth',
      selected = 'Linear growth'
    )
    updateSelectInput(
      inputId = 'emergency_growth',
      selected = 'Linear growth'
    )
    updateSelectInput(
      inputId = 'general_practice_growth',
      selected = 'Linear growth'
    )
    updateSelectInput(
      inputId = 'prescribing_growth',
      selected = 'Linear growth'
    )
    updateSelectInput(
      inputId = 'specialised_growth',
      selected = 'medium'
    )
    updateSelectInput(
      inputId = 'mental_health_growth',
      selected = 'Morbidity'
    )
    updateSelectInput(
      inputId = 'maternity_growth',
      selected = 'Linear growth'
    )
    updateSelectInput(
      inputId = 'community_growth',
      selected = 'Morbidity'
    )
    updateCheckboxGroupButtons(
      inputId = 'productivity_growth_scenario',
      selected = 'Central'
    )
    updateCheckboxGroupButtons(
      inputId = 'pay_growth_scenario',
      selected = 'Central'
    )
    updateNumericInput(
      inputId = 'drug',
      value = 2.3
    )
  })
  
  observeEvent(input$scen_b,{
    updateSelectInput(
      inputId = 'ae_growth',
      selected = 'Log growth'
    )
    updateSelectInput(
      inputId = 'elective_growth',
      selected = 'Policy: Recovery (10-year)'
    )
    updateSelectInput(
      inputId = 'outpatients_growth',
      selected = 'Policy: Recovery (10-year)'
    )
    updateSelectInput(
      inputId = 'emergency_growth',
      selected = 'Linear growth'
    )
    updateSelectInput(
      inputId = 'general_practice_growth',
      selected = 'Policy: Recovery'
    )
    updateSelectInput(
      inputId = 'prescribing_growth',
      selected = 'Linear growth'
    )
    updateSelectInput(
      inputId = 'specialised_growth',
      selected = 'lower'
    )
    updateSelectInput(
      inputId = 'mental_health_growth',
      selected = 'Policy: Recovery'
    )
    updateSelectInput(
      inputId = 'maternity_growth',
      selected = 'Linear growth'
    )
    updateSelectInput(
      inputId = 'community_growth',
      selected = 'Policy: Recovery'
    )
    updateCheckboxGroupButtons(
      inputId = 'productivity_growth_scenario',
      selected = '10-year'
    )
    updateCheckboxGroupButtons(
      inputId = 'pay_growth_scenario',
      selected = 'Recovery to 2010'
    )
    updateNumericInput(
      inputId = 'drug',
      value = 2.3
    )
  })
  
  output$subgraph <- plotly::renderPlotly({
    plotly::ggplotly(
      ggplot2::ggplot() +
        ggtitle(paste0("Supplementary Index of values")) +
        geom_line(data = data_index(), aes(x = fyear, y = values, col = names)) +
        geom_point(data = data_index(), aes(x = fyear, y = values, col = names)) +
        geom_line(data = data_model(), aes(x = fyear, y = budget_index), linetype = 2, col = "black") +
        THFstyle::scale_colour_THF() +
        theme_bw(base_size = base_font) +
        theme(panel.background = element_blank()) +
        xlab("") +
        ylab("Index (2017 = 100)") +
        labs(col = "Measure") +
        theme(legend.position = "bottom")
    )
  })%>%
    bindEvent(input$run)
}

shinyApp(ui = ui, server = server, options = list(height = 2000))
