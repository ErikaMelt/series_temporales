import pandas as pd
from statsmodels.tsa.arima.model import ARIMA

def fit_arima(file_path):
    # Leer el archivo CSV
    data = pd.read_csv(file_path, sep=';')
    
    # Convertir la columna de fecha a datetime
    data['Fecha'] = pd.to_datetime(data['Fecha'], format='%YM%m')
    
    # Ordenar los datos por fecha
    data = data.sort_values('Fecha')
    
    # Crear la serie temporal a partir de los datos
    ts_data = pd.Series(data['Serie'].values, index=data['Fecha'])
    
    # Ajustar el modelo ARIMA
    model = ARIMA(ts_data, order=(p, d, q))  # Reemplazar p, d, q con los valores apropiados
    arima_model = model.fit()
    
    # Realizar la predicción
    forecast_data = arima_model.predict(start='2023-01-01', end='2023-12-31')

    return forecast_data
