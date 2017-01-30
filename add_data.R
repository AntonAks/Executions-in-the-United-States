library(readr)
library(ggplot2)

data_set <- read_csv("~/R/Executions-in-the-United-States/data/database.csv")

data_set$Year <-  as.integer(substr(data_set$Date,7,10))

p <- ggplot(data = data_set, aes(x = data_set$Age)) + 
  geom_histogram()

p1 <- ggplot(data = data_set, aes(x = data_set$Year, y = data_set$`Victim Count`)) + 
  geom_point()
p1
