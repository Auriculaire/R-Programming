data <- read.csv("hw1_data.csv")
names(data)
data[1:2,]
nrow(data)
tail(data,2)
data[47,]
length(which(is.na(data$Ozone)))
mean(data$Ozone, na.rm=TRUE)
data.s <- subset(data,data$Ozone>31 & Temp>90)
mean(data.s$Solar.R)
data.s <-subset(data, data$Month==6)
mean(data.s$Temp)
data.s <-subset(data, data$Month==5)
max(data.s$Ozone, na.rm=TRUE)