#' timeSeriesExploration
#' A plugin to make time series exploration easier
#'
#' @export
timeSeriesExploration <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("Time Series Visualization"),

    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("Data",
        icon = shiny::icon("table"),

        miniUI::miniContentPanel(
          stableColumnLayout(
            shiny::selectInput(
              label = "Select your dataset:",
              inputId = "dataset",
              choices = c(search_df())
            )
          ),
          shiny::dataTableOutput("table")
        )
      ),

      miniUI::miniTabPanel("Line plot",
        icon = shiny::icon("chart-line"),


        miniUI::miniContentPanel(
          shiny::selectInput(
            label = "Select time variable",
            inputId = "time_column",
            choices = NULL
          ),
          shiny::selectizeInput(
            label = "Select your variables:",
            inputId = "line_columns",
            choices = NULL,
            multiple = TRUE
          ),
          miniUI::miniContentPanel(plotly::plotlyOutput("line_plot"))
        )
      ),

      miniUI::miniTabPanel("ScatterPlot",
        icon = shiny::icon("braille"),
        miniUI::miniContentPanel(
          shiny::selectInput(
            label = "Select x variable",
            inputId = "x_variable",
            choices = NULL
          ),
          shiny::selectizeInput(
            label = "Select y variables:",
            inputId = "y_variables",
            choices = NULL,
            multiple = TRUE
          ),
          miniUI::miniContentPanel(plotly::plotlyOutput("scatter_plot"))
        )
      ),



      miniUI::miniTabPanel("Correlogram",
        icon = shiny::icon("bar-chart"),

        fillCol(
          miniUI::miniContentPanel(
            shiny::selectInput(
              label = "select your variable",
              inputId = "corr_variable",
              choices = NULL
            ),

            shiny::sliderInput(inputId = "lag", label = "Lag variable", min = 12, max = 48, value = 24),
            fillRow(
              miniUI::miniContentPanel(plotly::plotlyOutput("Correlogram")),
              miniUI::miniContentPanel(plotly::plotlyOutput("PartialCorrelogram"))
            )
          )
        )
      )
    )
  )


  server <- function(input, output, session) {
    data <- shiny::reactive({
      get(input$dataset, envir = .GlobalEnv)
    })


    time_columns <- shiny::reactive({
      data() %>%
        dplyr::select_if(is.Date) %>%
        colnames()
    })

    numeric_columns <- shiny::reactive({
      data() %>%
        dplyr::select_if(is.numeric) %>%
        colnames()
    })

    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "corr_variable",
        choices = numeric_columns(),
        selected = numeric_columns()[1]
      )
    })

    shiny::observe({
      shiny::updateSelectizeInput(
        session = session,
        inputId = "line_columns",
        choices = numeric_columns(),
        selected = numeric_columns()[1]
      )
    })

    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "time_column",
        choices = time_columns(),
        selected = time_columns()[1]
      )
    })


    shiny::observe({
      shiny::updateSelectInput(
        session = session,
        inputId = "x_variable",
        choices = numeric_columns(),
        selected = numeric_columns()[1]
      )
    })


    shiny::observe({
      shiny::updateSelectizeInput(
        session = session,
        inputId = "y_variables",
        choices = numeric_columns(),
        selected = numeric_columns()[2]
      )
    })

    output$Correlogram <- plotly::renderPlotly({
      req(input$corr_variable, input$lag)

      autocorr <- forecast::ggAcf(data()[, input$corr_variable], lag.max = input$lag, type = "correlation") +
        ggplot2::ggtitle("Autocorrelation function (ACF)")
      ACF_chart <- plotly::ggplotly(autocorr)
      ACF_chart
    })

    output$PartialCorrelogram <- plotly::renderPlotly({
      req(input$corr_variable, input$lag)

      parautocorr <- forecast::ggPacf(data()[, input$corr_variable], lag.max = input$lag) +
        ggplot2::ggtitle("Partial Autocorrelation function (PACF)")
      PACF_chart <- plotly::ggplotly(parautocorr)
      PACF_chart
    })

    output$line_plot <- plotly::renderPlotly({
      req(input$time_column, input$line_columns)


      chart <- plotly::plot_ly(data(),
        x = ~ get(input$time_column),
        y = ~ get(input$line_columns[1]),
        type = "scatter",
        mode = "lines",
        name = input$line_columns[1]
      )

      if (length(input$line_columns) > 1) {
        more_cols <- input$line_columns[2:length(input$line_columns)]
        for (c in more_cols) {
          chart <- chart %>%
            plotly::add_lines(y = data()[[c]], type = "scatter", mode = "lines", name = c)
        }
      }

      chart %>%
        plotly::layout(
          xaxis = list(title = "Data"),
          yaxis = list(title = "Value"),
          legend = list(orientation = "h", xanchor = "center", yanchor = "top", x = 0.5, y = 1.2)
        ) %>%
        plotly::config(displaylogo = FALSE)
    })


    output$scatter_plot <- plotly::renderPlotly({
      req(input$x_variable, input$y_variables)

      chart <- plotly::plot_ly(
        data(),
        x = ~ get(input$x_variable),
        y = ~ get(input$y_variables[1]),
        type = "scatter",
        mode = "markers",
        name = input$y_variables[1]
      ) %>%
        plotly::add_trace(
          y = ~ get(input$x_variable),
          type = "scatter",
          mode = "lines",
          line = list(color = "black"),
          name = input$x_variable
        )

      if (length(input$y_variables) > 1) {
        more_ys <- input$y_variables[2:length(input$y_variables)]
        for (c in more_ys) {
          chart <- chart %>%
            plotly::add_trace(y = ~ get(c), type = "scatter", mode = "markers", name = c)
        }
      }

      chart %>%
        plotly::layout(
          xaxis = list(title = "X"),
          yaxis = list(title = "Y"),
          legend = list(orientation = "h", xanchor = "center", yanchor = "top", x = 0.5, y = 1.2)
        ) %>%
        plotly::config(displaylogo = FALSE)
    })


    output$table <- shiny::renderDataTable({
      data <- data()
      data
    })

    shiny::observeEvent(input$done, {
      shiny::stopApp()
    })
  }


  viewer <- dialogViewer(
    dialogName = "Visualize Time Series",
    height = 1000,
    width = 1600
  )
  shiny::runGadget(ui, server, viewer = viewer)
}
