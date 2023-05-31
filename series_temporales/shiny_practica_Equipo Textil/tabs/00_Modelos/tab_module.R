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
        plotlyOutput(ns("grafico_inicial"))
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
          dateInput(ns("dateInput"), "Fecha de inicio:"),
          selectInput(
            ns("sel1"),
            choices = c("ARIMA", "ARIMA Multiplicativo"),
            label = "Elija su modelo"
          ),
          width = 12
        ),
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
          title = "Coeficientes del modelo ARIMA",
          width = 12,
          verbatimTextOutput(ns("arima_coefs"))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = "Predicciones",
          width = 12,
          verbatimTextOutput(ns("arima_predictions"))
        )
      )
    ),
    fluidRow(
      column(
        width = 12,
        box(
          title = "BIC",
          width = 12,
          verbatimTextOutput(ns("bicOutput"))
        )
      )
    ),
  )
}


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
  
  output$grafico_inicial <- renderPlotly({
    # Crear el gráfico de serie temporal utilizando ggplot2 y convertirlo a plotly
    p <- ggplot(dataset(), aes(x = Fecha, y = Serie)) +
      geom_line() +
      labs(y = "Valor de la Serie") 
    
    p <- p + scale_x_date(
      date_breaks = "8 months",  # Intervalo de 6 meses entre las etiquetas de fecha
      date_labels = "%b %Y",  # Formato de las etiquetas de fecha (por ejemplo, "Jun 2023")
      expand = c(0.01, 0)  # Ajuste los márgenes del eje x para evitar solapamientos
    )
    
    
    # Ejemplo: Agregar una línea de referencia en el valor medio de la serie
    mean_value <- mean(dataset()$Serie)
    p <- p + geom_hline(yintercept = mean_value, linetype = "dashed", color = "red")
    
    p <- p + theme_minimal()
    p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    # Convertir a plotly y agregar características interactivas
    p <- ggplotly(p) %>% 
      layout(
        hovermode = "x",  # Información emergente al pasar el cursor
        dragmode = "zoom",  # Habilitar la función de zoom
        selectdirection = "h" 
      )
  })
  
  predict_arima <- eventReactive(input$predictButton, {
    req(dataset())
    
    if (input$p <= 0 || input$q <= 0 || input$horizon < 1) {
      stop("Valores de entrada no válidos. Por favor, ingrese parámetros válidos.")
    }
    
    if (is.null(input$dateInput)) {
      stop("Por favor, ingrese una fecha válida.")
    }
    
    # Convertir la fecha ingresada por el usuario en un objeto de fecha en R
    input_date <- as.Date(input$dateInput)
    
    if (input$sel1 == "ARIMA") {
      # Filtrar el conjunto de datos hasta la fecha ingresada por el usuario
      filtered_data <- dataset()[dataset()$Fecha <= input_date, ]
      
      # Ajustar el modelo ARIMA utilizando el conjunto de datos filtrado
      arima_model <- arima(filtered_data$Serie, order = c(input$p, input$d, input$q))
      
      # Obtener las predicciones para el horizonte especificado
      forecast_result <- predict(arima_model, n.ahead = input$horizon)
      
      # Obtener los residuos del modelo
      residuals <- arima_model$residuals
      
      # Calcular el número de parámetros estimados en el modelo ARIMA
      num_params <- length(arima_model$coef)
      
      # Calcular el BIC manualmente
      n <- length(filtered_data$Serie)
      bic <- -2 * logLik(arima_model) + num_params * log(n)
      
      # Mostrar el valor del BIC
      cat("BIC:", bic, "\n")
      
      # Calcular los errores utilizando los valores reales y las predicciones
      actual_values <- tail(filtered_data$Serie, input$horizon)
      errors <- actual_values - forecast_result$pred
      mae <- mean(abs(errors))
      mse <- mean(errors^2)
      
      # Agregar las métricas de error al resultado de la predicción
      forecast_result$BIC <- bic
      forecast_result$MAE <- mae
      forecast_result$MSE <- mse
      
    } else if (input$sel1 == "ARIMA Multiplicativo") {
      # Filtrar el conjunto de datos hasta la fecha ingresada por el usuario
      filtered_data <- dataset()[dataset()$Fecha <= input_date, ]
      
      # Ajustar el modelo ARIMA multiplicativo utilizando el conjunto de datos filtrado
      arima_model <- auto.arima(filtered_data$Serie, seasonal = TRUE, lambda = "auto")
      
      # Obtener las predicciones para el horizonte especificado
      forecast_result <- forecast(arima_model, h = input$horizon)
      
      # Obtener los residuos del modelo
      residuals <- arima_model$residuals
      
      # Calcular el número de parámetros estimados en el modelo ARIMA
      num_params <- length(arima_model$coef)
      
      # Calcular el BIC manualmente
      n <- length(filtered_data$Serie)
      bic <- -2 * logLik(arima_model) + num_params * log(n)
      
      # Mostrar el valor del BIC
      cat("BIC:", bic, "\n")
      
      # Calcular los errores utilizando los valores reales y las predicciones
      actual_values <- tail(filtered_data$Serie, input$horizon)
      errors <- actual_values - forecast_result$mean
      mae <- mean(abs(errors))
      mse <- mean(errors^2)
      
      # Agregar las métricas de error al resultado de la predicción
      forecast_result$BIC <- bic
      forecast_result$MAE <- mae
      forecast_result$MSE <- mse
    }
    
    return(list(arima_model = arima_model, forecast_result = forecast_result))
  })
  
  
  output$plot <- renderPlotly({
    # Obtener la fecha de inicio ingresada por el usuario
    fecha_inicio <- input$fecha_inicio
    
    # Obtener los resultados de la predicción ARIMA
    predict_arima_result <- predict_arima()
    
    # Generar el gráfico de predicción de serie temporal
    plt_prediction_serie(dataset, predict_arima_result$pred, predict_arima_result, fecha_inicio)
  })
  
  
  output$arima_coefs <- renderPrint({
    predict_arima_result <- predict_arima()
    predict_arima_result$arima_model$coef
  })
  
  output$arima_predictions <- renderPrint({
    predict_arima_result <- predict_arima()
    predict_arima_result$forecast_result
  })
  
  output$bicOutput <- renderPrint({
    predict_arima_result <- predict_arima()
    predict_arima_result$forecast_result$BIC
  })
  
}

