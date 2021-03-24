library(tidyverse)
library(scales)
library(mosaic)

country <- "Spain"

#Change default colours
theme_set(theme_minimal())
ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set1")

data <- read.csv("~/Work/covid/owid-covid-data.csv", sep=",", header = TRUE)

#Fix dates
data$date <- as.POSIXct(strptime(data$date, "%Y-%m-%d"))
data$total_cases <- as.numeric(data$total_cases)

dd <- data[data$location == country,]

dd %>%
  filter(date > "2021-01-01") %>%
  ggplot(aes(x=date)) +
  geom_line(aes(y = people_vaccinated_per_hundred, colour = "1 dosis"), size = 1) + 
  geom_line(aes(y = people_fully_vaccinated_per_hundred, colour = "2 dosis"), size = 1) +
  labs(x = "Fecha", y = "Vacunados", colour="Dosis")

x <- dd %>%
  filter(date > "2021-01-01")
x <- x[, c(4,42)]

ddays <- integer(length(x[,1]))
for (i in 1:length(x[,1]) ) {
  ddays[i] <- as.integer(x[i,1]-as.POSIXct("2021-01-01"))
}
x["Days"] <- ddays

if (country == "United States") {
  # Este modelo funciona en USA
  f <- fitModel(people_fully_vaccinated_per_hundred ~ A + B*Days^C, data = x,
                start=list(A=-0.4, B=0.003, C=2))
}else if(country == "United Kingdom" | country == "Germany") {
  # Este modelo funciona en UK
  f <- fitModel(people_fully_vaccinated_per_hundred ~ A + B*Days^C, data = x,
                start=list(A=0.65, B=1.2e-9, C=5))
}else {
  # Modelo para España
  f <- fitModel(people_fully_vaccinated_per_hundred ~ (A*Days+B)+C*sin( (Days-D)/E ), data = x, 
                start=list(A=0.065, B=-1, C=0.35, D=30, E =7))
}

plotPoints(people_fully_vaccinated_per_hundred ~ Days, data = x, 
           xlab="Días desde 1 enero",
           ylab="Personas totalmente vacunadas (%)",
           main=country)
plotFun(f, xlim=range(18,80), col = "red", add=TRUE)

#Predecimos fecha para el 70% de vacunación:
tv <- 0
i <- 0
while (tv < 70) {
  i <- i+1
  tv <- f(i)
}

print(paste("El 70% se obtiene el", as.Date("2021-01-01")+i))
