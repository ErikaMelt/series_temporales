# UI function
tab_0_ui <- function(id) {
  ns <- NS(id)
  
  fluidPage(
    fluidRow(
      column(
        width = 12,
        h4("Display",
           style = "color: #333; font-weight: bold; margin-bottom: 10px;"),
        radioButtons(
          ns("disp"),
          label="Display",
          choices = c(Head = "head", All = "all"),
          selected = "head"
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        dataTableOutput(ns("contents"))
      )
    ),
    fluidRow(
      column(
        width = 12,
        h4("Gráfico de Serie Temporal",
           style = "color: #333; font-weight: bold; margin-bottom: 10px;"),
        plotlyOutput(ns("grafico"))
      )
    ),
    tags$hr(),
    fluidRow(
      column(
        width = 4,
        box(
          numericInput(
            ns("p"),
            label = "p (Orden AR)",
            value = 1,
            min = 0
          ),
          width = 12
        )
      ),
      column(
        width = 5,
        box(
          numericInput(
            ns("d"),
            label = "d (Orden de diferenciación)",
            value = 0,
            min = 0
          ),
          width = 12
        )
      ),
      column(
        width = 4,
        box(
          numericInput(
            ns("q"),
            label = "q (Orden MA)",
            value = 1,
            min = 0
          ),
          width = 12
        )
      ),
      column(
        width = 4,
        box(
          numericInput(
            ns("horizon"),
            label = "Horizonte de Previsión:",
            value = 10
          ),
          width = 12
        )
      )
    ),
    fluidRow(
      column(
        width = 6,
        box(
          selectInput(
            ns("sel1"),
            choices = c("ARIMA", "ARIMA Multiplicativo"),
            label = "Elija su modelo"
          ),
          width = 12
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        actionButton(
          ns("predictButton"),
          label = "Generar Predicción",
          style = "background-color: #337ab7; color: #fff; border-color: #337ab7; margin-top: 10px;"
        )
      )
    ),
    tags$hr(),
    fluidRow(
      column(
        width = 12,
        box(
          title = "Resultado de la predicción ARIMA",
          width = 12,
          verbatimTextOutput(ns("forecast_result"))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = "MAE - MSE y BIC",
          width = 12,
          verbatimTextOutput(ns("errorMetricsOutput")),
          verbatimTextOutput(ns("bicOutput"))
          
        )
      )
    )
  )
}



# Server function
# Server function
tab_0_server <- function(input, output, session) {
  
  dataset <- function() {
    # Copia el dataframe original para mantenerlo intacto
    df_clean <- df
    
    # Convertir la columna "Fecha" al formato adecuado
    df_clean$Fecha <- as.Date(paste0(df_clean$Fecha, "01"), format = "%YM%m%d")
    
    # Eliminar filas con valores nulos
    df_clean <- df_clean[complete.cases(df_clean), ]
    
    return(df_clean)
  }
  
  output$contents <- DT::renderDT({
    if (input$disp == "head") {
      datos <- head(dataset())
    } else {
      datos <- dataset()
    }
    
    # Customization options
    opciones <- list(
      dom = 't',
      paging = TRUE,
      ordering = TRUE,
      searching = TRUE,
      info = FALSE
    )
    
    # Create the table
    datatable(datos, options = opciones,
              class = 'cell-border stripe')
  })
  
  output$grafico <- renderPlotly({
    # Crear el gráfico de serie temporal utilizando ggplot2 y convertirlo a plotly
    p <- ggplot(dataset(), aes(x = Fecha, y = Serie)) +
      geom_line() +
      labs(x = "Fecha", y = "Serie") +
      theme_bw()
    
    ggplotly(p)
  })
  
  predict_arima <- eventReactive(input$predictButton, {
    req(dataset())
    
    if (input$p <= 0 || input$q <= 0 || input$horizon < 1) {
      stop("Valores de entrada no válidos. Por favor, ingrese parámetros válidos.")
    }
    
    if (input$sel1 == "ARIMA") {
      # Ajustar el modelo ARIMA utilizando el conjunto de datos completo
      arima_model <- arima(dataset()$Serie, order = c(input$p, input$d, input$q))
      
      # Obtener las predicciones para el horizonte especificado
      forecast_result <- predict(arima_model, n.ahead = input$horizon)
      
      # Obtener los residuos del modelo
      residuals <- arima_model$residuals
      
      # Calcular el número de parámetros estimados en el modelo ARIMA
      num_params <- length(arima_model$coef)
      
      # Calcular el BIC manualmente
      n <- length(dataset()$Serie)
      bic <- -2 * logLik(arima_model) + num_params * log(n)
      
      # Mostrar el valor del BIC
      cat("BIC:", bic, "\n")
      
      # Calcular los errores utilizando los valores reales y las predicciones
      actual_values <- tail(dataset()$Serie, input$horizon)
      errors <- actual_values - forecast_result$pred
      mae <- mean(abs(errors))
      mse <- mean(errors^2)
      
      # Agregar las métricas de error al resultado de la predicción
      forecast_result$MAE <- mae
      forecast_result$MSE <- mse
    } else if (input$sel1 == "ARIMA Multiplicativo") {
      # Ajustar el modelo ARIMA multiplicativo utilizando el conjunto de datos completo
      arima_model <- auto.arima(dataset()$Serie, seasonal = TRUE, lambda = "auto")
      
      # Obtener las predicciones para el horizonte especificado
      forecast_result <- forecast(arima_model, h = input$horizon)
      
      # Obtener los residuos del modelo
      residuals <- arima_model$residuals
      
      # Calcular el número de parámetros estimados en el modelo ARIMA
      num_params <- length(arima_model$coef)
      
      # Calcular el BIC manualmente
      n <- length(dataset()$Serie)
      bic <- -2 * logLik(arima_model) + num_params * log(n)
      
      # Mostrar el valor del BIC
      cat("BIC:", bic, "\n")
      
      # Calcular los errores utilizando los valores reales y las predicciones
      actual_values <- tail(dataset()$Serie, input$horizon)
      errors <- actual_values - forecast_result$mean
      mae <- mean(abs(errors))
      mse <- mean(errors^2)
      
      # Agregar las métricas de error al resultado de la predicción
      forecast_result$MAE <- mae
      forecast_result$MSE <- mse
    }
    
    return(list(arima_model = arima_model, forecast_result = forecast_result))
  })
  
  output$forecast_result <- renderPrint({
    predict_arima_result <- predict_arima()
    predict_arima_result$forecast_result
  })
  
  output$errorMetricsOutput <- renderText({
    predict_arima_result <- predict_arima()
    forecast_result <- predict_arima_result$forecast_result
    
    if (is.null(forecast_result$MAE) || is.null(forecast_result$MSE)) {
      return(NULL)
    }
    
    mae <- forecast_result$MAE
    mse <- forecast_result$MSE
    
    paste("MAE:", sprintf("%.2f", mae), "\n", "MSE:", sprintf("%.2f", mse))
  })
  
  output$bicOutput <- renderPrint({
    predict_arima_result <- predict_arima()
    arima_model <- predict_arima_result$arima_model
    
    # Calcular el BIC manualmente
    n <- length(dataset()$Serie)
    bic <- -2 * logLik(arima_model) + length(arima_model$coef) * log(n)
    
    # Mostrar el valor del BIC
    cat("BIC:", bic, "\n")
  })
}

