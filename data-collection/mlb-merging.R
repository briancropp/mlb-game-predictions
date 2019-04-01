library(readr)
library(openxlsx)
library(dplyr)

data <- read_csv("/Users/Brian/Google\ Drive/denison/classes-current/da-401/mlb-game-predictions/data-collection/2015_2018mlb.csv")

data$singles <- ifelse(data$hit==1, 1, 0)
data$doubles <- ifelse(data$hit==2, 1, 0)
data$triples <- ifelse(data$hit==3, 1, 0)
data$hrs <- ifelse(data$hit==4, 1, 0)

data %>% gro
