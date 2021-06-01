prepareData <- function(data){
  data$id <- seq_len(nrow(data))
  data$datetime <- as.POSIXct(data$datetime, format="%Y-%m-%d %H:%M:%OS")
  data$weekday <- as.factor(lubridate::wday(data$datetime,week_start=1))
  data$month <- as.factor(lubridate::month(data$datetime))
  data$hour <- as.factor(lubridate::hour(data$datetime))
  data$season <- as.factor(data$season)
  data$holiday <- as.factor(data$holiday)
  data$workingday <- as.factor(data$workingday)
  data$weather <- as.factor(data$weather)
  data$temp <- as.numeric(data$temp)
  data$atemp <- as.numeric(data$atemp)
  data$windspeed <- as.numeric(data$windspeed)
  data$casual <- NULL
  data$registered <- NULL
  
  return(data)
}