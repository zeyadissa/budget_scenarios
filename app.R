library(tidyverse)
library(shinythemes)
library(shiny)
library(shinyWidgets)
library(devtools)
library(bslib)
library(bsicons)

devtools::install_github("THF-evaluative-analytics/THFstyle")

source("glob.R")
source("sidebarUI.R")
source("activityUI.R")
source("indexUI.R")

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
    sidebarPanel(
      titlePanel(h4(strong("BUDGET SCENARIOS"), icon("house", class = "about-icon fa-pull-left"))),
      hr(),
      sidebarUI("var"),
      width = 3
    ),
    mainPanel(
      fluidRow(
        column(6, activityUI("ba")),
        column(6, indexUI("ba"))
      ),
      br(),
      fluidRow(
        tabsetPanel(
          tabPanel("Time-series",
            icon = icon("house"),
            br(),
            fluidRow(
              column(12, plotly::plotlyOutput("maingraph")),
              column(12, plotly::plotlyOutput("subgraph"))
            )
          ),
          # waterfall
          tabPanel("Waterfall",
            icon = icon("water"),
            fluidRow(
              br(),
              column(12, plotOutput("waterfall_graph")),
              column(12, plotOutput("waterfall_graph2"))
            )
          )
        )
      ),
      width = 9
    )
  ),
  style = "font-size: 15px"
)

# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Base values
  deflator_adj <- reactive({
    if (input$real_flag == F) {
      deflator <- 1
    } else {
      deflator <- 0
    }
  })

  pop_adj <- reactive({
    if (input$per_capita == F) {
      pop_final %>%
        mutate(values = 1) %>%
        select(!c(metric, X))
    } else {
      pop_final %>%
        select(!c(metric, X)) %>%
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

  data_final <- reactive({
    data_final2 <- rbind(
      data_final_a,
      expand_grid(
        "type" = "Other",
        "measure" = "cwa",
        "models" = "Base",
        "fyear" = c(base_year:max_year),
        "modelled_growth" = 1,
        "baseline" = 13214650700,
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

  base_data <- reactive({
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
        drug = (100 + input$drug) / 100,
        val_drug = cumprod(drug),
        val_deflator = deflator^deflator_adj(),
        val_prod = cumprod(prod),
        val_pay = cumprod(pay)
      ) %>%
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
      CreateCommunityMHData(.) %>%
      mutate(
        final_value = baseline * (modelled_growth) * index * val_deflator,
        final_value_no_cost_index = baseline * (modelled_growth) * val_deflator,
        final_cost_value = baseline * (modelled_growth - 1) * (1 - index) * val_deflator,
        final_drugs_value = baseline * (modelled_growth - 1) * (1 - drugs_index) * val_deflator,
        final_pay_value = baseline * (modelled_growth - 1) * (1 - pay_index) * val_deflator,
        final_prod_value = final_cost_value - final_drugs_value - final_pay_value
      ) %>%
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
      select(fyear, val_prod, val_drug, val_pay, val_deflator) %>%
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
      filter(names %in% input$index_names)
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
    round(100 * (((data_model() %>%
      ungroup() %>%
      select(fyear, final_value) %>%
      filter(fyear == max(fyear)))[1, 2] -
      (data_model() %>%
        ungroup() %>%
        select(fyear, final_value) %>%
        filter(fyear == min(fyear)))[1, 2]) /
      (length(unique(data_model()$fyear)) * (data_model() %>%
        ungroup() %>%
        select(fyear, final_value) %>%
        filter(fyear == min(fyear)))[1, 2])), 2)
  })

  # data for model
  data_waterfall <- reactive({
    df1 <- base_data() %>%
      # true filter
      group_by(fyear, models, type) %>%
      mutate(
        final_value_no_cost_index = final_value_no_cost_index - baseline * (1) * val_deflator,
        final_value_no_cost_index = (final_value_no_cost_index),
        final_value = (final_value),
        pay_adjustment = -(final_pay_value),
        drug_cost_adjustment = -(final_drugs_value),
        productivity_adjustment = -(final_prod_value)
      ) %>%
      select(type, models, fyear, final_value_no_cost_index, pay_adjustment, drug_cost_adjustment, productivity_adjustment)

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
      select(!c(models, final_value_no_cost_index)) %>%
      pivot_longer(cols = !c(type, fyear), names_to = "models", values_to = "final_value_no_cost_index")

    df3 <- rbind(df1 %>% select(type, fyear, models, final_value_no_cost_index), df2)
  })

  filter_val_waterfall <- reactive({
    (data_waterfall() %>%
      filter(fyear == input$water_year) %>%
      filter(type == input$water_type) %>%
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
      ))$final_value_no_cost_index / 1e9
  })

  base_waterfall <- reactive({
    df1 <- data_waterfall() %>%
      filter(fyear == input$water_year) %>%
      filter(type == input$water_type) %>%
      mutate(final_value_no_cost_index = round(final_value_no_cost_index / 1e9, 2)) %>%
      ungroup() %>%
      select(models, final_value_no_cost_index)

    filter_val <- case_when(
      input$water_type == "A&E" ~ (input$ae_growth),
      input$water_type == "Elective" ~ (input$elective_growth),
      input$water_type == "Emergency" ~ (input$emergency_growth),
      input$water_type == "Prescribing" ~ (input$prescribing_growth),
      input$water_type == "Outpatients" ~ (input$outpatients_growth),
      input$water_type == "General Practice" ~ (input$general_practice_growth),
      input$water_type == "Community" ~ (input$community_growth),
      input$water_type == "Specialised" ~ (input$specialised_growth),
      input$water_type == "Mental Health" ~ (input$mental_health_growth),
      input$water_type == "Maternity" ~ (input$maternity_growth),
      T ~ "Other"
    )

    df2 <- df1 %>%
      filter(
        case_when(
          grepl(pattern = "Log", filter_val) == T ~ grepl(pattern = "Linear|Recovery", models) == F,
          grepl(pattern = "Linear", filter_val) == T ~ grepl(pattern = "Log|Recovery", models) == F,
          grepl(pattern = "Recovery", filter_val) == T ~ grepl(pattern = "Log|Linear", models) == F,
          T ~ final_value_no_cost_index <= filter_val_waterfall() | models %in% c("pay_adjustment", "productivity_adjustment", "drug_cost_adjustment")
        )
      )
    return(df2)
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
    waterfalls::waterfall(base_waterfall(), calc_total = TRUE, total_rect_color = "orange", rect_text_size = 1.5) +
      theme_bw(base_size = 16) +
      xlab("") +
      ylab("") +
      THFstyle::scale_fill_THF() +
      ggtitle("Growth within POD dissagregated by model type") +
      theme(text = element_text(size = 16))
  })

  output$waterfall_graph2 <- renderPlot({
    waterfalls::waterfall(type_waterfall(), calc_total = TRUE, total_rect_color = "orange", rect_text_size = 1.5) +
      theme_bw(base_size = 16) +
      xlab("") +
      ylab("") +
      THFstyle::scale_fill_THF() +
      ggtitle("Change in budget relative to baseline scenario") +
      theme(text = element_text(size = 16))
  })

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
  })

  output$download <-  downloadHandler(
    filename = 'data.csv',
    content = function(file){
      write.csv(data_model(), file)
    }
  )


  output$subgraph <- plotly::renderPlotly({
    plotly::ggplotly(
      ggplot2::ggplot() +
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
  })
}

shinyApp(ui = ui, server = server, options = list(height = 2000))
