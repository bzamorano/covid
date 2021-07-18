library(tidyverse)

#Change default colours
theme_set(theme_minimal())
ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set1")

data2 <- read.csv("~/Work/covid/owid-covid-data.csv", sep=",", header = TRUE)

#Fix dates
data2$date <- as.POSIXct(strptime(data2$date, "%Y-%m-%d"))
data2 <- data2[data2$location == "Spain",c("date", "people_fully_vaccinated_per_hundred")]

data <- read.csv("~/Work/covid/dates.csv", sep=",", header = TRUE)
data[[1]] <- as.POSIXct(strptime(data[[1]], "%Y-%m-%d"))
data[[2]] <- as.POSIXct(strptime(data[[2]], "%Y-%m-%d"))

#data <- merge(x = data, y = data2, by.x = "RunDate", by.y = "date")
#rm(data2)

chDate <- c(NA, round(diff.Date(x = data[[2]])/86400))
data["change"] <- chDate

data %>%
ggplot(aes(x=RunDate)) +
  geom_line(aes(y=PredDate), size = 1, col = "red") +
  labs(x = "Fecha predicción", y = "Fecha predicha", title = "Predicción España")

data2 %>%
#  filter(date >= "2021-03-24") %>%
  filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y=people_fully_vaccinated_per_hundred), size = 1, col = "blue") +
  labs(x = "Fecha predicción", y = "Pauta completa (%)", title = "Evolución España")

data %>%
  filter(RunDate >= "2021-05-14") %>%
  ggplot(aes(x=RunDate, y=PredDate)) +
  geom_line(size = 1, col = "red") +
  labs(x = "Fecha predicción", y = "Fecha predicha", title = "Predicción España")
