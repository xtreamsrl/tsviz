createTimeSeriesCharts <- function() {

  library(rstudioapi)
  library(miniUI)
  library(shiny)
  library(ggplot2)
  library(forecast)
  library(plotly)
  #library(rlang)



  entered <- function(string) {
    !is.null(string) && nzchar(string)
  }


  search_df<- function(){

    # Container
    c <- c()

    # Function to tell which place an object has in the workspace
    w <- function(x) {
      ls <- ls(envir = .GlobalEnv)
      return(which(ls == x))
    }

    # Which object is a dataframe?
    for (data in ls(envir = .GlobalEnv)) {
      if (any(class(eval(parse(text = data))) == "data.frame")) {
        c[w(data)] <- data
      }
    }

    # Return all non-NA values
    return(c[!is.na(c)])

    # Delete the rest
    rm(w)
    rm(c)

  }

  ui <- miniPage(
    gadgetTitleBar("Visualize time series"),
    miniTabstripPanel(
      miniTabPanel( "Line plot", icon = icon("chart-line"),
                    fillRow(
                    miniContentPanel(
                      selectInput(label = "Select your dataset:",
                                  inputId = "dataset",
                                  choices = c("", search_df())),

                      uiOutput("time_choise"),
                      uiOutput("numerical_choise")),


            miniContentPanel(plotOutput("linePlot"))
      )
    ),
    miniTabPanel( "Scatter plot", icon = icon("braille"),
      fillRow(
        miniContentPanel(
          selectInput(label = "Select your dataset:",
                      inputId = "dataset_scatter",
                      choices = c("", search_df())),

          uiOutput("x_variable"),
          uiOutput("y_variable")),

        miniContentPanel(plotOutput("scatterPlot"))
    )
    ),
    miniTabPanel( "Correlogram", icon = icon("bar-chart"),
                  fillCol(
                    miniContentPanel(
                      selectInput(label = "Select your dataset:",
                                  inputId = "dataset_correlogram",
                                  choices = c("", search_df())),

                      uiOutput("var_correlogram"),

                      sliderInput(inputId = "lag", label ="Lag variable", min = 12, max = 48, value = 24),


                    miniContentPanel(plotlyOutput("Correlogram"))
                  )
         )
      ),
    miniTabPanel( "Partial Correlogram", icon = icon("bar-chart"),
                  fillCol(
                    miniContentPanel(
                      selectInput(label = "Select your dataset:",
                                  inputId = "dataset_partial_correlogram",
                                  choices = c("", search_df())),

                      uiOutput("var_partial_correlogram"),

                      sliderInput(inputId = "par_lag", label ="Lag variable", min = 12, max = 48, value = 24),


                      miniContentPanel(plotlyOutput("PartialCorrelogram"))
                    )
                  )
)
)
)
  server <- function(input, output, session){

    # rv <- reactiveValues(
    #   code = NULL
    # )
    # Was a dataset selected?
    data <- reactive({
      validate(
        need(input$dataset != "", "")
      )
      get(input$dataset)
    })

    data_scatter <- reactive({
      validate(
        need(input$dataset_scatter != "","")
      )
      get(input$dataset_scatter)
    })

    data_correlogram <- reactive({
      validate(
        need(input$dataset_correlogram != "","")
      )
      get(input$dataset_correlogram)
    })

    data_partial_correlogram <- reactive({
      validate(
        need(input$dataset_partial_correlogram != "","")
      )
      get(input$dataset_partial_correlogram)
    })

    time_columns <- reactive({
      # time_series_df() %>%
      #   select_if(is.Date) %>%
      #   colnames()
      input$time_columns
    })


    numeric_columns <- reactive({

      input$numeric_columns
    })

    x_var_scatter <- reactive({
      input$x_var_scatter
    })


    y_var_scatter <- reactive({
      input$y_var_scatter
    })


    var_corr <- reactive({
      input$var_corr
    })

    var_par_corr <- reactive({
      input$var_par_corr
    })


    output$time_choise <- renderUI({

        selectInput(inputId = "time_columns",
                    label = "Select time variable",
                    choices =c("",colnames(data())))


    })

    output$numerical_choise <- renderUI({

      selectInput(inputId = "numeric_columns",
                  label = "Select numerical variable",
                  choices =c("",colnames(data())))


    })


    output$x_variable <- renderUI({
      #col.names <- colnames(data())

      selectInput(inputId = "x_var_scatter",
                  label = "Select x variable",
                  choices =c("",colnames(data_scatter())))


    })


    output$y_variable <- renderUI({
      #col.names <- colnames(data())

      selectInput(inputId = "y_var_scatter",
                  label = "Select y variable",
                  choices =c("",colnames(data_scatter())))


    })



    output$var_correlogram <- renderUI({
      #col.names <- colnames(data())

      selectInput(inputId = "var_corr",
                  label = "Select correlogram variable",
                  choices =c("",colnames(data_correlogram())))


    })


    output$var_partial_correlogram <- renderUI({
      #col.names <- colnames(data())

      selectInput(inputId = "var_par_corr",
                  label = "Select correlogram variable",
                  choices =c("",colnames(data_partial_correlogram())))


    })


    output$linePlot <- renderPlot({
      req(input$time_columns, input$numeric_columns)
      time_col = as.name(input$time_columns)
      numeric_col = as.name(input$numeric_columns)
      g<-ggplot(data = data()) + geom_line(aes(x=!!time_col,y=!!numeric_col))
      g
    })



    output$scatterPlot <- renderPlot({
      req(input$x_var_scatter, input$y_var_scatter)
      x_var = as.name(input$x_var_scatter)
      y_var = as.name(input$y_var_scatter)
      g<-ggplot(data = data_scatter(), aes(x=!!x_var,y=!!y_var)) + geom_point()
      g
    })


    output$Correlogram <- renderPlotly({
      req(input$var_corr,input$lag)

      autocorr <- ggAcf(data_correlogram()[, input$var_corr], lag.max = input$lag, type = "correlation")
      ACF_chart <- ggplotly(autocorr + ggtitle("Autocorrelation function (ACF)"))
      ACF_chart

    })


    output$PartialCorrelogram <- renderPlotly({
      req(input$var_par_corr,input$lag)

      parautocorr <- ggPacf(data_partial_correlogram()[, input$var_par_corr], lag.max = input$par_lag)
      ACF_chart <- ggplotly(parautocorr + ggtitle("Partial Autocorrelation function (PACF)"))
      ACF_chart

    })

    observeEvent(input$done, {

      stopApp()
    })

  }

  viewer <- dialogViewer(dialogName = "Visualize Time Series",
                         height = 1000,
                         width = 1600)
  runGadget(ui, server, viewer = viewer)


 # runGadget(shinyApp(ui, server), viewer = viewer)

}
