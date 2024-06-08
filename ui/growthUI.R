growthUI <- function(id){
  tagList(
    dropdown(
      tags$h4('Growth Assumptions'),
      circle = T,
      block = T,
      size='sm',
      width = '500px',
      style = 'unite',
      label = 'Select growth scenarios',
      status = 'danger',
      icon = icon('tree'),
      #item 1
      materialSwitch(
        inputId = "growth_scenarios",
        label = "Apply custom growth scenarios?",
        value = T,
        status = "primary"
      ),
      radioGroupButtons(
        inputId = "productivity_growth_scenario",
        label = "Select Productivity Scenario",
        choices = c("Central","10-year", "5-year"),
        individual = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle", 
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-circle-o", 
                      style = "color: steelblue"))
      ),
      radioGroupButtons(
        inputId = "pay_growth_scenario",
        label = "Select Pay Scenario",
        choices = c("Central","Recovery to 2010", "Recovery to 2018"),
        individual = TRUE,
        checkIcon = list(
          yes = tags$i(class = "fa fa-circle", 
                       style = "color: steelblue"),
          no = tags$i(class = "fa fa-circle-o", 
                      style = "color: steelblue"))
      )
      )
    )
}