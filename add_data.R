source("f.R")
library(readr)
library(ggplot2)
library(dplyr)

# Load the dataset
dp_data <- read_csv("~/R/Executions-in-the-United-States/data/database.csv"
                    , col_types = cols(Date = col_date(format = "%m/%d/%Y")))

dp_data <- 
  dp_data%>%
  filter(Name!='Timothy McVeigh*')


dp_data$Year <- as.integer(substr(dp_data$Date,1,4)) 

summary(dp_data)

hist_1 <- 
ggplot(dp_data, aes(x = Year, fill = Method) ) +
  ggtitle("Number of executions by years and methods") +
  labs(x = "Years", y = NULL) +
  scale_x_continuous(breaks=seq(min(dp_data$Year), max(dp_data$Year), 1)) + 
  scale_y_continuous(breaks=seq(0, 100, 10)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  geom_histogram(bins = max(dp_data$Year) - min(dp_data$Year)
                 , binwidth = 1
                 , colour = "black")

hist_1.1 <- 
  ggplot(dp_data, aes(x = Year, fill = Method) ) +
  ggtitle("Share of executions by years and methods") +
  labs(x = "Years", y = NULL) +
  scale_x_continuous(breaks=seq(min(dp_data$Year), max(dp_data$Year), 1)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  geom_histogram(bins = max(dp_data$Year) - min(dp_data$Year)
                 , binwidth = 1
                 , colour = "black"
                 , position = "fill")

hist_2 <- 
  ggplot(dp_data, aes(x = Year, fill = Method) ) +
  ggtitle("Number of executions by Regions") +
  labs(x = "Years", y = NULL) +
  scale_x_continuous(breaks=seq(min(dp_data$Year), max(dp_data$Year), 1)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  geom_histogram(bins = max(dp_data$Year) - min(dp_data$Year)
                 , binwidth = 1
                 , colour = "black") + 
  facet_wrap(~ Region)


  ggplot(dp_data, aes(x = State, fill = Method) ) +
  ggtitle("Number of executions by Regions") +
  labs(x = "Years", y = NULL) +
  scale_x_continuous(breaks=seq(min(dp_data$Year), max(dp_data$Year), 1)) + 
  theme(axis.text.x = element_text(angle = 90)) + 
  geom_histogram(bins = max(dp_data$Year) - min(dp_data$Year)
                 , binwidth = 1
                 , colour = "black") + 
  facet_wrap(~ Region)





num_exec <- 
  dp_data%>%
  group_by(State, Region, Method)%>%
  summarise("num_e" = n_distinct(Name))

bar_1 <- 
ggplot(num_exec, aes(x = reorder(Method, -num_e) , y = num_e, fill = Region)) + 
  geom_bar(stat = "identity", position = "fill") #+ 
#  facet_grid(~ Method)


multiplot(hist_1, hist_1.1)



