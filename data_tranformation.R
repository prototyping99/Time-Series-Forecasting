data_load=read.csv("dataset.csv")
main_dataset<-ts(data = data_load$Consumption,frequency = 12,start = c(2002,01),end = c(2021,06))
#write.csv(main_dataset,"load_data_transformed.csv")

plot(main_dataset,ylab="Load Consumption Gigawhatts ",main="Time Series plot Electricity consumption in South Africa")
acf(main_dataset)
pacf(main_dataset)

#dataset with 1st dffrencing

diff_dataset=diff(main_dataset)
plot(diff_dataset,ylab="Load Consumption Gigawhatts ",main="Time Series plot Electricity consumption in South Africa (diffrenced)")
acf(main_dataset)
pacf(main_dataset)

