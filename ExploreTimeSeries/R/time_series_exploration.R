timeSeriesExploration <- function() {
  library(dplyr)
  library(lubridate)
  library(miniUI)
  library(shiny)
  library(forecast)
  library(plotly)




  ui <- miniPage(
    gadgetTitleBar("Time Series Visualization"),

    miniTabstripPanel(
      miniTabPanel("Data",
        icon = icon("table"),

        miniContentPanel(
          stableColumnLayout(
            selectInput(
              label = "Select your dataset:",
              inputId = "dataset",
              choices = c(search_df())
            )
          ),
          dataTableOutput("table")
        )
      ),

      miniTabPanel("Line plot",
        icon = icon("chart-line"),


        miniContentPanel(
          selectInput(
            label = "Select time variable",
            inputId = "time_column",
            choices = NULL
          ),
          selectizeInput(
            label = "Select your variables:",
            inputId = "line_columns",
            choices = NULL,
            multiple = TRUE
          ),



          miniContentPanel(plotlyOutput("line_plot"))
        )
      ),

      miniTabPanel("ScatterPlot",
        icon = icon("braille"),
        miniContentPanel(
          selectInput(
            label = "Select x variable",
            inputId = "x_variable",
            choices = NULL
          ),
          selectizeInput(
            label = "Select y variables:",
            inputId = "y_variables",
            choices = NULL,
            multiple = TRUE
          ),



          miniContentPanel(plotlyOutput("scatter_plot"))
        )
      ),



      miniTabPanel("Correlogram",
        icon = icon("bar-chart"),

        fillCol(
          miniContentPanel(
            selectInput(
              label = "select your variable",
              inputId = "corr_variable",
              choices = NULL
            ),

            sliderInput(inputId = "lag", label = "Lag variable", min = 12, max = 48, value = 24),
            fillRow(miniContentPanel(plotlyOutput("Correlogram")), miniContentPanel(plotlyOutput("PartialCorrelogram")))
          )
        )
      )
    )
  )





  server <- function(input, output, session) {
    data <- reactive({
      get(input$dataset, envir = .GlobalEnv)
    })


    time_columns <- reactive({
      data() %>%
        select_if(is.Date) %>%
        colnames()
    })

    numeric_columns <- reactive({
      data() %>%
        select_if(is.numeric) %>%
        colnames()
    })

    observe({
      updateSelectInput(
        session = session,
        inputId = "corr_variable",
        choices = numeric_columns(),
        selected = numeric_columns()[1]
      )
    })

    observe({
      updateSelectizeInput(
        session = session,
        inputId = "line_columns",
        choices = numeric_columns(),
        selected = numeric_columns()[1]
      )
    })

    observe({
      updateSelectInput(
        session = session,
        inputId = "time_column",
        choices = time_columns(),
        selected = time_columns()[1]
      )
    })


    observe({
      updateSelectInput(
        session = session,
        inputId = "x_variable",
        choices = numeric_columns(),
        selected = numeric_columns()[1]
      )
    })


    observe({
      updateSelectizeInput(
        session = session,
        inputId = "y_variables",
        choices = numeric_columns(),
        selected = numeric_columns()[2]
      )
    })

    output$Correlogram <- renderPlotly({
      req(input$corr_variable, input$lag)

      autocorr <- ggAcf(data()[, input$corr_variable], lag.max = input$lag, type = "correlation")
      ACF_chart <- ggplotly(autocorr + ggtitle("Autocorrelation function (ACF)"))
      ACF_chart
    })
    #
    #
    output$PartialCorrelogram <- renderPlotly({
      req(input$corr_variable, input$lag)

      parautocorr <- ggPacf(data()[, input$corr_variable], lag.max = input$lag)
      PACF_chart <- ggplotly(parautocorr + ggtitle("Partial Autocorrelation function (PACF)"))
      PACF_chart
    })


    output$line_plot <- renderPlotly({
      req(input$time_column, input$line_columns)


      chart <- plot_ly(data(), x = ~ get(input$time_column), y = ~ get(input$line_columns[1]), type = "scatter", mode = "lines", name = input$line_columns[1])

      if (length(input$line_columns) > 1) {
        more_cols <- input$line_columns[2:length(input$line_columns)]
        for (c in more_cols) {
          chart <- chart %>%
            add_lines(y = data()[[c]], type = "scatter", mode = "lines", name = c)
        }
      }

      chart %>%
        layout(
          xaxis = list(title = "Data"),
          yaxis = list(title = "Value"),
          legend = list(orientation = "h", xanchor = "center", yanchor = "top", x = 0.5, y = 1.2)
        ) %>%
        plotly::config(displaylogo = FALSE)
    })


    output$scatter_plot <- renderPlotly({
      req(input$x_variable, input$y_variables)

      chart <- plot_ly(data(), x = ~ get(input$x_variable), y = ~ get(input$y_variables[1]), type = "scatter", mode = "markers", name = input$y_variables[1]) %>%
        add_trace(y = ~ get(input$x_variable), type = "scatter", mode = "lines", line = list(color = "black"), name = input$x_variable)

      if (length(input$y_variables) > 1) {
        more_ys <- input$y_variables[2:length(input$y_variables)]
        for (c in more_ys) {
          chart <- chart %>%
            add_trace(y = ~ get(c), type = "scatter", mode = "markers", name = c)
        }
      }

      chart %>%
        layout(
          xaxis = list(title = "X"),
          yaxis = list(title = "Y"),
          legend = list(orientation = "h", xanchor = "center", yanchor = "top", x = 0.5, y = 1.2)
        ) %>%
        plotly::config(displaylogo = FALSE)
    })


    output$table <- renderDataTable({
      data <- data()
      data
    })

    observeEvent(input$done, {
      stopApp()
    })
  }




  viewer <- dialogViewer(
    dialogName = "Visualize Time Series",
    height = 1000,
    width = 1600
  )
  runGadget(ui, server, viewer = viewer)
}
