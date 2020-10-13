library(readr)
Plastic <- read.csv("D:/Data science videos/R Codes/Assignments docs/Forecasting/PlasticSales (3).csv")
View(Plastic)

library(moments)

# Data exploration
summary(Plastic)
str(Plastic)
class(Plastic)

attach(Plastic)

# Graphical exploration
hist(Sales)
summary(Sales)
boxplot(Sales)
skewness(Sales)
kurtosis(Sales)

# Pre Processing
# So creating 12 dummy variables 
X <- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 )# Creating dummies for 12 months
colnames(X) <- month.abb # Assigning month names 
head(X)

plasticdata <- cbind(Plastic,X)
View(plasticdata)
colnames(plasticdata)

# input t
plasticdata["t"] <- c(1:60)

plasticdata["log_sales"] <- log(plasticdata["Sales"])

plasticdata["t_square"] <- plasticdata["t"]*plasticdata["t"]

View(plasticdata)
## Preprocesing completed

attach(plasticdata)

# partitioning the data
train <-plasticdata[1:45,]
test <- plasticdata[46:60,]

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

######################## Quadratic ####################################
Quad_model <- lm(Sales ~ t + t_square, data = train)
summary(Quad_model)
Quad_pred <- data.frame(predict(Quad_model, interval='predict', newdata=test))
rmse_Quad <- sqrt(mean((test$Sales-Quad_pred$fit)^2, na.rm=T))
rmse_Quad

######################### Additive Seasonality #########################
sea_add_model <- lm(Sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(sea_add_model)
sea_add_pred <- data.frame(predict(sea_add_model, newdata=test, interval = 'predict'))
rmse_sea_add <- sqrt(mean((test$Sales-sea_add_pred$fit)^2, na.rm = T))
rmse_sea_add

######################## Additive Seasonality with Quadratic #################
Add_sea_Quad_model <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred <- data.frame(predict(Add_sea_Quad_model, interval='predict', newdata=test))
rmse_Add_sea_Quad <- sqrt(mean((test$Sales - Add_sea_Quad_pred$fit)^2, na.rm=T))
rmse_Add_sea_Quad

######################## Multiplicative Seasonality #########################
multi_sea_model <- lm(log_sales ~ Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = train)
summary(multi_sea_model)
multi_sea_pred <- data.frame(predict(multi_sea_model, newdata=test, interval='predict'))
rmse_multi_sea <- sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2, na.rm = T))
rmse_multi_sea

# Preparing table on model and it's RMSE values 
table_rmse <- data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea))
colnames(table_rmse) <- c("model","RMSE")
table_rmse

# Additive seasonality with Quadratic has least RMSE value

############### Combining Training & test data to build Additive seasonality using Quadratic Trend ############
Add_sea_Quad_model_final <- lm(Sales ~ t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov, data = plasticdata)
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

# Bulilding Arima model     
A <- arima(Add_sea_Quad_model_final$residuals, order = c(1,1,3))
A$residuals

ARerrors <- A$residuals

acf(ARerrors, lag.max = 10)

library(forecast)

# predicting next 12 months errors using arima
errors_12 <- forecast(A, h = 12)

future_errors <- data.frame(errors_12)

head(future_errors)
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
