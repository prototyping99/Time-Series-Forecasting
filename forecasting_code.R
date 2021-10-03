library(forecast)
library(Metrics)

# reading data
data = read.csv("load_data_transformed.csv")

# splitting data into train and valid sets
train = main_dataset[1:187]
valid = main_dataset[188:nrow(data)]

# removing "Month" column
train$Month = NULL

# training model
model = auto.arima(train)

# model summary
summary(model)

# forecasting
forecast = predict(model,47)

# evaluation
rmse(valid, forecast$pred)
