#' Easy and interactive visualization of time series
#'
#' An RStudio addin to visualize time series.
#' Time series are supposed to be contained into a \code{data.frame} object in the global environment,
#' with the following format:
#' - a column of type \code{Date}
#' - one of more numeric columns
#'
#' @importFrom forecast Acf
#' @export
tsviz <- function() {
  ui <- miniUI::miniPage(
    miniUI::gadgetTitleBar("tsviz - time series visualization", left = NULL, right = NULL),

    miniUI::miniTabstripPanel(
      miniUI::miniTabPanel("data",
        icon = shiny::icon("table"),

        miniUI::miniContentPanel(
          shiny::selectInput(
            label = "Dataset:",
            inputId = "dataset",
            choices = c(get_time_series_data_frames_in_env())
          ) %>% shinyhelper::helper(content = 'Dataset selection'),
          shiny::dataTableOutput("table")
        )
      ),

      miniUI::miniTabPanel("Line chart",
        icon = shiny::icon("chart-line"),
        miniUI::miniContentPanel(
          shiny::selectInput(
            label = "Time variable",
            inputId = "time_column",
            choices = NULL
          ),
          shiny::selectizeInput(
            label = "Chart ariables:",
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
            label = "X variable",
            inputId = "x_variable",
            choices = NULL
          ),
          shiny::selectizeInput(
            label = "Y variables:",
            inputId = "y_variables",
            choices = NULL,
            multiple = TRUE
          ),
          miniUI::miniContentPanel(plotly::plotlyOutput("scatter_plot"))
        )
      ),


      miniUI::miniTabPanel("Correlogram",
        icon = shiny::icon("bar-chart"),

        shiny::fillCol(
          miniUI::miniContentPanel(
            shiny::selectInput(
              label = "Variable",
              inputId = "corr_variable",
              choices = NULL
            ),
            shiny::sliderInput(inputId = "lag", label = "Lag variable", min = 10, max = 100, value = 30),
            shiny::fillRow(
              miniUI::miniContentPanel(plotly::plotlyOutput("correlogram")),
              miniUI::miniContentPanel(plotly::plotlyOutput("partial_correlogram"))
            )
          )
        )
      )
    )
  )


  server <- function(input, output, session) {
    data <- shiny::reactive({
      validate(
        need(input$dataset != "", "Looks like none of your data.frames contains a time series!")
      )
      get(input$dataset, envir = .GlobalEnv)
    })

    time_columns <- shiny::reactive(
      get_columns_by_type_match(data(), lubridate::is.Date)
    )

    numeric_columns <- shiny::reactive(
      get_columns_by_type_match(data(), is.numeric)
    )

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

    output$correlogram <- plotly::renderPlotly({
      req(input$corr_variable, input$lag)

      (forecast::ggAcf(data()[, input$corr_variable], lag.max = input$lag, type = "correlation") +
        ggplot2::ggtitle("Autocorrelation function (ACF)")) %>%
        plotly::ggplotly() %>%
        plotly::config(displaylogo = FALSE)
    })

    output$partial_correlogram <- plotly::renderPlotly({
      req(input$corr_variable, input$lag)

      (forecast::ggPacf(data()[, input$corr_variable], lag.max = input$lag) +
        ggplot2::ggtitle("Partial Autocorrelation function (PACF)")) %>%
        plotly::ggplotly() %>%
        plotly::config(displaylogo = FALSE)
    })

    output$line_plot <- plotly::renderPlotly({
      req(input$time_column, input$line_columns)

      chart <- plotly::plot_ly(
        data(),
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
          xaxis = list(title = "Date"),
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

    output$table <- shiny::renderDataTable(data())

    shinyhelper::observe_helpers()
  }


  viewer <- shiny::dialogViewer(
    dialogName = "Visualize Time Series",
    height = 700,
    width = 1000
  )


  shiny::runGadget(ui, server, viewer = viewer)
}
