library(readxl)
Airlines <- read_excel("D:/Data science videos/R Codes/Assignments docs/Forecasting/Airlines+Data (3).xlsx")
View(Airlines)

# Data exploration
summary(Airlines)
str(Airlines)
class(Airlines)

# Pre Processing
# Creating dummies for 12 months
X <- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 )
colnames(X) <- month.abb # Assigning month names 
View(X)

Airlinesdata <- cbind(Airlines,X)
View(Airlinesdata)
colnames(Airlinesdata)

# input t
Airlinesdata["t"] <- c(1:96)

Airlinesdata["log_passengers"] <- log(Airlinesdata["Passengers"])

Airlinesdata["t_square"] <- Airlinesdata["t"]*Airlinesdata["t"]

View(Airlinesdata)
## Preprocesing completed

attach(Airlinesdata)

# Partitioning
train <-Airlinesdata[1:80,]
test <- Airlinesdata[81:96,]

########################### LINEAR MODEL #############################
linear_model <- lm(Passengers ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =test))
linear_pred
rmse_linear <- sqrt(mean((test$Passengers-linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential #################################
expo_model <- lm(log_passengers ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Passengers-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ####################################
Quad_model <- lm(Passengers ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Passengers-Quad_pred$fit)^2, na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################
sea_add_model <- lm(Passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Passengers-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################
Add_sea_Quad_model <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Passengers - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################
multi_sea_model <- lm(log_passengers ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

# Preparing table on model and it's RMSE values 
table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic has least RMSE value

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############
Add_sea_Quad_model_final <- lm(Passengers ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = Airlinesdata)
summary(Add_sea_Quad_model_final)


####################### Predicting new data #############################
pred_new <- predict(Add_sea_Quad_model_final, newdata = test, interval = 'predict')
pred_new <- as.data.frame(pred_new)
View(pred_new)

plot(Add_sea_Quad_model_final)

plot(Airlines, type = "l")

# take all residual value of the model built & plot ACF plot
acf(Add_sea_Quad_model_final$residuals, lag.max = 10) 

# take all residual value of the model built & plot ACF plot
pacf(Add_sea_Quad_model_final$residuals, lag.max = 10) 

library(forecast)

# Checking the values of p,d,q using auto.arima
model <- auto.arima(Passengers)
model

# Using the ARIMA model to forecast
A <- arima(Add_sea_Quad_model_final$residuals, order = c(1,1,3))
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 10)

library(forecast)

# predicting next 12 months errors using arima( order =c(1,1,3))
errors_12 <- forecast(A, h = 12)
future_errors <- data.frame(errors_12)

head(future_errors)

class(future_errors)
future_errors <- future_errors$Point.Forecast

# predicted values for new data + future error values 
predicted_new_values <- pred_new + future_errors
head(predicted_new_values)
predicted_new_values <- round(predicted_new_values$fit, digits = 0)

e <- cbind(predicted_new_values,test$Passengers)
head(e)

# Checking the correlation
cor(predicted_new_values,test$Passengers)
