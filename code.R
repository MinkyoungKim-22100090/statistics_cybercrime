
install.packages("forecast")
install.packages("tseries")
install.packages("dplyr")
library(forecast)
library(tseries)
library(dplyr)

setwd("your path")
data <- read.csv('해킹범죄건수.csv')

occurrence_data <- data[data$구분 == '발생건수',]
clearance_data <- data[data$구분 == '검거건수',]

occurrence_data <- occurrence_data[,-which(names(occurrence_data) == '구분')]
rownames(occurrence_data) <- occurrence_data$연도
occurrence_data <- occurrence_data[,-which(names(occurrence_data) == '연도')]

clearance_data <- clearance_data[,-which(names(clearance_data) == '구분')]
rownames(clearance_data) <- clearance_data$연도
clearance_data <- clearance_data[,-which(names(clearance_data) == '연도')]

occurrence_matrix <- as.matrix(occurrence_data)

plot(1:nrow(occurrence_matrix), occurrence_matrix[,1], type='l', col=rainbow(ncol(occurrence_matrix))[1],
     main='Trends in Hacking Incidents (Occurrences)',
     xlab='Year', ylab='Number of Incidents', xaxt='n')

for(i in 2:ncol(occurrence_matrix)) {
  lines(1:nrow(occurrence_matrix), occurrence_matrix[,i], col=rainbow(ncol(occurrence_matrix))[i], type='l')
}

legend("topright", legend=colnames(occurrence_data), col=rainbow(ncol(occurrence_matrix)), lty=1)

clearance_matrix <- as.matrix(clearance_data)

plot(1:nrow(clearance_matrix), clearance_matrix[,1], type='l',
     col=rainbow(ncol(clearance_matrix))[1],
     main='Trends in Hacking Incidents (Clearances)',
     xlab='Year', ylab='Number of Clearances', xaxt='n')

for(i in 2:ncol(clearance_matrix)) {
  lines(1:nrow(clearance_matrix), clearance_matrix[,i], type='l', col=rainbow(ncol(clearance_matrix))[i])
}

legend("topright", legend=colnames(clearance_data), col=rainbow(ncol(clearance_matrix)), lty=1)

years <- as.numeric(rownames(occurrence_data))
print(colnames(occurrence_data))

hacking_occurrences <- occurrence_data$`해킹(계정도용)`

years <- as.numeric(rownames(occurrence_data))

print(length(hacking_occurrences))
print(head(hacking_occurrences))

print(years)
print(min(years))
print(max(years))
print(colnames(occurrence_data))

hacking_occurrences <- occurrence_data$해킹계정도용

print(length(hacking_occurrences))
print(head(hacking_occurrences))
print(colnames(occurrence_data))

index <- which(colnames(occurrence_data) == '해킹(계정도용)')
hacking_occurrences <- occurrence_data[, index]

print(length(hacking_occurrences))
print(head(hacking_occurrences))

hacking_occurrences_ts <- ts(hacking_occurrences, start=min(years), end=max(years), frequency=1)

hacking_occurrences_ts

print(colnames(occurrence_data))

hacking_occurrences <- occurrence_data[, 3]

print(head(hacking_occurrences))

hacking_occurrences_ts <- ts(hacking_occurrences, start=2014, end=2020, frequency=1)
print(head(hacking_occurrences_ts))

fit <- auto.arima(hacking_occurrences_ts)
summary(fit)

forecast(fit, h=5)  

plot(forecast(fit, h=5), main='ARIMA Forecast of Hacking Incidents')

