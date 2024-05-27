sidebarUI <- function(id) {
  tagList(
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
      choices = min_date:max_date,
      selected = c(2020, 2021)
    ),
    hr(),
    selectInput(
      inputId = "water_type",
      label = "Select POD for waterfall",
      choices = water_type
    ),
    selectInput(
      inputId = "water_year",
      label = "Select year for waterfall",
      choices = min_date:max_date
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