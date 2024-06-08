sidebarUI <- function(id) {
  tagList(
    actionButton(label='Growth rates',
                 icon = icon('gear'),
                 inputId='info',
                 status = 'danger',
                 block = T,
                 style = 'width: 100%'),
    hr(),
    selectInput(
      inputId = "base_yr",
      label = "Select baseline year",
      choices = min_date:max_year,
      selected = 2024
    ),
    numericInput(
      inputId = "prod",
      label = "Custom productivity CAGR (%)",
      min = -100,
      value = 0.58,
      max = 100,
      step = 1
    ),
    numericInput(
      inputId = "pay",
      label = "Custom above-inflation pay growth (%)",
      min = -100,
      value = 0.8,
      max = 100,
      step = 1
    ),
    numericInput(
      inputId = "drug",
      label = "Custom drug growth (%)",
      min = -100,
      value = 4.89,
      max = 100,
      step = 1
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
      inputId = "intensity",
      label = "Shock intensity",
      min = -100,
      value = 0,
      max = 100,
      step = 1
    ),
    sliderTextInput(
      inputId = "range",
      label = "Choose shock range",
      choices = min_date:max_year,
      selected = c(2020, 2021)
    ),
    hr(),
    selectInput(
      inputId = "water_type",
      label = "Select POD for waterfall",
      choices = c(water_type,'Total'),
      selected = 'Total'
    ),
    selectInput(
      inputId = "water_year",
      label = "Select waterfall baseline year",
      choices = min_date:max_year,
      selected = 2035
    ),
    hr(),
    fluidRow(
      column(6, 
             actionButton("run",
                          icon = shiny::icon('play'),
                          "Run Scenario",
                          style = 'width: 100%')),
      column(6, 
             downloadButton(outputId = "download",
                            'Download File',
                            style = 'width: 100%'))
    )
  )
}