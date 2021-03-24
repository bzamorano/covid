library(tidyverse)
library(scales)
library(mosaic)

#Change default colours
theme_set(theme_minimal())
ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set1")

data <- read.csv("~/Work/covid/owid-covid-data.csv", sep=",", header = TRUE)

#Fix dates
data$date <- as.POSIXct(strptime(data$date, "%Y-%m-%d"))
data$total_cases <- as.numeric(data$total_cases)

spain <- data[data$location == "Spain",]

spain %>%
  filter(date > "2021-01-01") %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y = people_vaccinated_per_hundred, colour = "1 dosis"), size = 1) + 
  geom_line(aes(y = people_fully_vaccinated_per_hundred, colour = "2 dosis"), size = 1) +
  labs(x = "Fecha", y = "Vacunados", colour="Dosis")

x <- spain %>%
  filter(date > "2021-01-01")
x <- x[, c(4,42)]

ddays <- integer(length(x[,1]))
for (i in 1:length(x[,1]) ) {
  ddays[i] <- as.integer(x[i,1]-as.POSIXct("2021-01-01"))
}
x["Days"] <- ddays

f <- fitModel(people_fully_vaccinated_per_hundred ~ (A*Days+B)+C*sin( (Days-D)/E ), data = x, 
              start=list(A=0.065, B=-1, C=0.35, D=30, E =7))

plotPoints(people_fully_vaccinated_per_hundred ~ Days, data = x, 
           xlab="Días desde 1 enero",
           ylab="Personas totalmente vacunadas (%)")
plotFun(f, xlim=range(18,80), col = "red", add=TRUE)
