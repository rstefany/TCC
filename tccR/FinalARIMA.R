
pacotes <- c("ggplot2","quantmod","forecast","tidyverse","prophet","gridExtra","data.table","ggseas","knitr","zoo","TSA","tseries")


if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

# Criando a base de dados
getSymbols("ITUB4.SA", from = "2019-01-01")

stock_df.train.window <- window(ITUB4.SA$ITUB4.SA.Close, start = "2022-06-01", end="2022-12-31")
stock_df.test.window <- window(ITUB4.SA$ITUB4.SA.Close, start = "2023-01-01", end="2023-03-31")
stock_df.train <- ts(na.omit(stock_df.train.window))
stock_df.test <- ts(na.omit(stock_df.test.window))


ts(ITUB4.SA)

autoplot(stock_df.train) +
  ylab("Preço fechamento") +
  xlab("Dia")

autoplot(stock_df.test) +
  ylab("Preço fechamento") +
  xlab("Dia")

arima_model <- auto.arima(stock_df.train)

# Generate forecasts for the test set
forecast_arima <- forecast(arima_model, h = length(stock_df.test))


# Evaluate the accuracy of the forecasts using a performance metric such as RMSE
accuracy(forecast_arima, stock_df.test.window)


# ARIMA
autoplot(stock_df.train, series=" Dados históicos") +
  autolayer(forecast_arima$mean, series=" ARIMA Forecast") +
  ggtitle(" ARIMA forecasting") +
  theme(plot.title = element_text(size=8)) +
  ylab("Preço do ativo") +
  xlab("Dia")



# Avaliação dos modelos

# Modelo de previsão
forecast_arima['model'] #ARIMA

# Acurácia do Modelo
accuracy(forecast_arima) #ARIMA


residuals <- residuals(model)


autoplot(residuals)+
  ggtitle("ITUB4 Resíduos X Dia") +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylab("Resíduos") +
  xlab("Dia")


hist(residuals, freq = FALSE, breaks = 20, main = "Histograma dos Resíduos e Normalidade ITUB4"
     , xlab = "Resíduos", ylab = "Densidade")
mu <- mean(residuals)
sigma <- sd(residuals)
curve(dnorm(x, mean = mu, sd = sigma), add = TRUE, col = "blue", lwd = 2)


# assuming you have already fitted an ARIMA model and created a test data set
# extract the forecasted values for the dates corresponding to the test data
forecast_values <- forecast(arima_model, h = length(stock_df.test))$mean

# create a data frame with the test data and the forecasted values
comparison_data <- data.frame(stock_df.test, forecast_values)

# compare the actual and forecasted values for each date
comparison_data$comparison <- comparison_data$ITUB4.SA.Close - comparison_data$forecast_values

# Create a dataframe with the comparison data
comparison_df <- data.frame(Date = index(comparison_data),
                            Actual = comparison_data$ITUB4.SA.Close,
                            Forecast = comparison_data$forecast_values)

# Create a line plot of the actual and forecast values
ggplot(comparison_df, aes(x = Date, y = Actual)) +
  geom_line(color = "blue") +
  geom_line(aes(y = Forecast), color = "red") +
  
  # Add a title and axis labels
  ggtitle("ITUB4: Valor Real vs Forecast") +
  xlab("Dia") +
  ylab("Fechamento") +
  
  # Customize the theme
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5,size = 20, face = "bold"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.text = element_text(size = 12))

