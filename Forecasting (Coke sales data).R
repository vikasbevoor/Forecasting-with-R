library(readxl)

Coke <- read_excel("D:/Data science videos/R Codes/Assignments docs/Forecasting/CocaCola_Sales_Rawdata (3).xlsx")
View(Coke)

attach(Coke)
library(forecast)

# Data exploration
summary(Coke)
str(Coke)

# Pre Processing
# Creating dummies for 4 Quarters
Quarter <- as.data.frame(c(1:42))

qr <- Quarter - (Quarter %/% 4)*4
summary(qr)

qr1 <- qr + 4*(qr < 1)
summary(qr1) 

qr1 <- as.data.frame(qr1)
View(qr1)

colnames(qr1) <-  "Quarter1"
attach(qr1)

Q1 <- ifelse(Quarter1 == 1, 1,0)
Q1 <- as.data.frame(Q1)

Q2 <- ifelse(Quarter1 == 2, 1,0)
Q2 <- as.data.frame(Q2)

Q3 <- ifelse(Quarter1 == 3, 1,0)
Q3 <- as.data.frame(Q3)

Q4 <- ifelse(Quarter1 == 4, 1,0)
Q4 <- as.data.frame(Q4)

Coke1 <- cbind(Coke,Q1,Q2, Q3,Q4)
View(Coke1)
attach(Coke1)


# input t
Coke1["t"] <- c(1:42)

Coke1["log_sales"] <- log(Coke1["Sales"])

Coke1["t_square"] <- Coke1["t"]*Coke1["t"]
View(Coke1)
## Preprocesing completed

attach(Coke1)

# partitioning
train <-Coke1[1:32,]
test <- Coke1[33:42,]

########################### LINEAR MODEL #############################
linear_model <- lm(Sales ~ t, data = train)
summary(linear_model)
linear_pred <- data.frame(predict(linear_model, interval='predict', newdata =test))
linear_pred
rmse_linear <- sqrt(mean((test$Sales-linear_pred$fit)^2, na.rm = T))
rmse_linear

######################### Exponential #################################
expo_model <- lm(log_sales ~ t, data = train)
summary(expo_model)
expo_pred <- data.frame(predict(expo_model, interval='predict', newdata = test))
rmse_expo <- sqrt(mean((test$Sales-exp(expo_pred$fit))^2, na.rm = T))
rmse_expo

######################### Quadratic ####################################
Quad_model <- lm(Sales ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Sales-Quad_pred$fit)^2, na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################
sea_add_model <- lm(Sales ~ Q1+Q2+Q3+Q4, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Sales-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################
Add_sea_Quad_model <- lm(Sales ~ t+t_square+ Q1+Q2+Q3+Q4, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Sales - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################
multi_sea_model <- lm(log_sales ~  Q1+Q2+Q3+Q4, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

# Preparing table on model and it's RMSE values 
table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
View(table_rmse)

# Additive seasonality with Quadratic has least RMSE value

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############
Add_sea_Quad_model_final <- lm(Sales ~ t+t_square+ Q1+Q2+Q3+Q4, data = Coke1)
summary(Add_sea_Quad_model_final)


####################### Predicting new data #############################
pred_new <- predict(Add_sea_Quad_model_final, newdata = test, interval = 'predict')
pred_new <- as.data.frame(pred_new)
View(pred_new)

plot(Add_sea_Quad_model_final)

plot(Sales, type = "l")

# take all residual value of the model built & plot ACF plot
acf(Add_sea_Quad_model_final$residuals, lag.max = 10)  

# take all residual value of the model built & plot ACF plot
pacf(Add_sea_Quad_model_final$residuals, lag.max = 10) 

# Checking the values of p,d,q using auto.arima
model <- auto.arima(Sales)
model

# Using the ARIMA model to forecast
A <- arima(Add_sea_Quad_model_final$residuals, order = c(1,1,1))
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 10)

library(forecast)

# predicting next 4 quarters errors using arima( order =c(1,1,1))
errors_12 <- forecast(A, h = 8)

future_errors <- data.frame(errors_12)
future_errors
class(future_errors)
future_errors <- future_errors$Point.Forecast


# predicted values for new data + future error values 
predicted_new_values <- pred_new + future_errors
head(predicted_new_values)
predicted_new_values <- round(predicted_new_values$fit, digits = 0)

plot(predicted_new_values)

e <- cbind(predicted_new_values,test$Sales)
head(e)

# Checking the correlation
cor(predicted_new_values,test$Sales)
