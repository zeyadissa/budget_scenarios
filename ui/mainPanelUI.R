mainPanelUI <- function() {
  tagList(
    mainPanel(
      width = 9,
      # row
      fluidRow(
        column(4, activityUI("ba")),
        column(4, indexUI("ba")),
        column(4, growthUI("ba"))),
        br(),
      fluidRow(
        # tab panels
        tabsetPanel(
          tabPanel("Time-series",
            icon = icon("house"),
            br(),
            column(12, plotly::plotlyOutput("maingraph",height='700px'))
          ),
          # waterfall
          tabPanel("Waterfall",
            icon = icon("water"),
            br(),
            column(12, plotOutput("waterfall_graph")),
            column(12, plotOutput("waterfall_graph2"))
          ),
          tabPanel("Supplement",
            icon = icon("earth"),
            br(),
            column(12, plotly::plotlyOutput("subgraph",height='700px'))
          )
        )
      )
    )
  )
}

