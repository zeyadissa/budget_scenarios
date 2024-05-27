indexUI <- function(id){
  tagList(
    dropdown(
      tags$h4('Activity Inputs'),
      circle = T,
      block = T,
      size='sm',
      style = 'unite',
      label = 'Select supplementary indices',
      status = 'danger',
      icon = icon('book'),
      width = '500px',
      #item 1
      selectInput(
        inputId = "index_names",
        label = "Select Indices",
        choices = c('Productivity','Hospital Drugs','Pay','Deflator','Receipts','GDP'),
        selected = c('GDP','Receipts'),
        multiple = T)))
    }