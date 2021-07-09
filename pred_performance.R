library(tidyverse)

#Change default colours
theme_set(theme_minimal())
ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set1")

data <- read.csv("~/Work/covid/dates.csv", sep=",", header = TRUE)

data[[1]] <- as.POSIXct(strptime(data[[1]], "%Y-%m-%d"))
data[[2]] <- as.POSIXct(strptime(data[[2]], "%Y-%m-%d"))

chDate <- c(NA, round(diff.Date(x = data[[2]])/86400))

data["change"] <- chDate

data %>%
ggplot(aes(x=RunDate, y=PredDate)) +
  geom_line(size = 1, col = "red") +
  labs(x = "Fecha predicción", y = "Fecha predicha", title = "Predicción España")

data %>%
  filter(RunDate >= "2021-05-14") %>%
  ggplot(aes(x=RunDate, y=PredDate)) +
  geom_line(size = 1, col = "red") +
  labs(x = "Fecha predicción", y = "Fecha predicha", title = "Predicción España")
