library(readr)
library(ggplot2)
library(dplyr)

data_set <- read_csv("~/R/Executions-in-the-United-States/data/database.csv")

data_set$Year <-  as.integer(substr(data_set$Date,7,10))
data_set <- data_set%>%
  filter(`Victim Count`<50)


attach(data_set)

p <- ggplot(data = data_set, aes(x = Age)) + 
  geom_histogram(bins=30, color="black",fill="lightblue")
p


p1 <- ggplot(data = data_set, aes(x = Year, y = `Victim Count`)) + 
  geom_jitter() +
  facet_grid(Method ~ .)
p1
