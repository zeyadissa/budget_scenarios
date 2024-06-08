activityUI <- function(id){
  tagList(
    dropdown(
      tags$h4('Activity Inputs'),
      circle = T,
      style = 'unite',
      size='sm',
      block = T,
      width = '500px',
      label = 'Select model types',
      status = 'danger',
      icon = icon('hospital'),
      fluidRow(
        column(6,
      #item 1
      selectInput(
        inputId = "ae_growth",
        label = "A&E",
        choices = ae_activity_growth_type),
      selectInput(
        inputId = "elective_growth",
        label = "Elective",
        choices = elective_activity_growth_type),
      selectInput(
        inputId = "outpatients_growth",
        label = "Outpatients",
        choices = outpatients_activity_growth_type),
      selectInput(
        inputId = "emergency_growth",
        label = "Emergency",
        choices = emergency_activity_growth_type),
      selectInput(
        inputId = "general_practice_growth",
        label = "General Practice",
        choices = general_practice_activity_growth_type)),
      column(6,
      selectInput(
        inputId = "prescribing_growth",
        label = "Prescribing",
        choices = prescribing_activity_growth_type),
      selectInput(
        inputId = "specialised_growth",
        label = "Specialised Services",
        choices = specialised_activity_growth_type),
      selectInput(
        inputId = "mental_health_growth",
        label = "Mental Health",
        choices = mental_health_activity_growth_type),
      selectInput(
        inputId = "maternity_growth",
        label = "Maternity",
        choices = maternity_activity_growth_type),
      selectInput(
        inputId = "community_growth",
        label = "Community",
        choices = community_activity_growth_type)),
      column(12,
             selectInput(
               inputId = "iapt_growth",
               label = "IAPT",
               choices = iapt_activity_growth_type,
               width = '100%')),
      column(6, 
             actionButton("scen_a",
                          "Set Trend Scenario",
                          style = 'width: 100%')),
      column(6, 
             actionButton("scen_b",
                          "Set Policy Scenario",
                          style = 'width: 100%'))
      )
    ))}
