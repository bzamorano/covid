library(tidyverse)
library(scales)
library(mosaic)
library(lubridate)

#Change default colours
theme_set(theme_minimal())

data <- read.csv("~/Work/covid/owid-covid-data.csv", sep=",", header = TRUE)

data %>%
  group_by(location) %>%
  filter(continent != "") %>%
  summarise(mortality = sum(new_deaths[(length(new_deaths)-13):length(new_deaths)], 
                            na.rm = TRUE)*100. / 
              sum(new_cases[(length(new_cases)-13):length(new_cases)], na.rm = TRUE),
            vaccination = max(people_fully_vaccinated_per_hundred, na.rm = TRUE)) %>%
  filter(vaccination > 5) %>%
  filter(mortality < 10) %>%
  filter(mortality > 0) %>%
  ggplot(aes(x = vaccination, y = mortality)) +
  geom_point(show.legend = FALSE) +
  labs(x = "People fully vaccinated (%)", y = "14 days mortality rate") +
  geom_lm() +
  ggsave("mortality_vs_vaccines.png")

#Fix dates
data$date <- as.POSIXct(strptime(data$date, "%Y-%m-%d"))
data$total_cases <- as.numeric(data$total_cases)

countries <- c("Israel", "United Kingdom", "United States", "Spain", "Mexico",
               "Portugal", "Italy", "Brazil", "France", "Japan",
               "Belgium", "Germany", "India", "Sweden", "Canada",
               "United Arab Emirates", "Cuba", "Singapore", "Chile",
               "Cambodia", "South Korea", "Uruguay", "Turkey", "Iran",
               "Vietnam", "Russia", "Morocco", "China", "Switzerland", "Czechia",
               "Myanmar", "Colombia", "Argentina", "Ukraine", "Poland", "Austria",
               "South Africa", "Indonesia", "Pakistan", "Bangladesh",
               "Philippines", "Australia", "Egypt", "Ethiopia", "Nigeria")

continents <- c("World", "European Union",
               "Asia", "Africa", "Europe", "North America", "South America", "Oceania")

#Shrink data
data <- data[data$location %in% c(countries, continents),]

population <- data %>% 
  group_by(location) %>%
  summarise(population = last(population))

get_date <- function(country){

  dd <- data[data$location == country,]
  
  x <- dd %>%
    filter(date > "2021-01-01")
  x <- x[, c("date","people_fully_vaccinated_per_hundred")]
  
  ddays <- integer(length(x[,1]))
  for (i in 1:length(x[,1]) ) {
    ddays[i] <- i
  }
  x["Days"] <- ddays
  
  x2 <- x %>%
    filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
    slice_tail(n = 7)
  
  nmindays <- 8
  while( (range(x2$people_fully_vaccinated_per_hundred)[2] - 
        range(x2$people_fully_vaccinated_per_hundred)[1] < 1.0) & 
        (nmindays <= sum(!is.na(x$people_fully_vaccinated_per_hundred))) ) {
    x2 <- x %>%
      filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
      slice_tail(n = nmindays)
      nmindays = nmindays +1
  }
  
  ll <- lm(formula = people_fully_vaccinated_per_hundred ~ Days + 1, data = x2)
  f2 <- makeFun(ll)
  
  slope <- ll$coefficients[[2]]
  Dslope <- summary(ll)$coefficients[2,2]
  if(is.na(Dslope)){Dslope = slope}
  error <- round(3*(80-max(x2$people_fully_vaccinated_per_hundred)) * Dslope / (slope * slope))
  if(error < 1){error <- 1}

  p <- plotPoints(people_fully_vaccinated_per_hundred ~ Days, data = x, 
             xlab="Days since 1st Jan 2021",
             ylab="People fully vaccinated (%)",
             main=country)

  #Predecimos fecha para el 80% de vacunación:
  tv <- 0
  i2 <- 0
  while (tv < 80) {
    i2 <- i2+1
    tv <- f2(i2)
    if (is.na(tv)) {
      tv <- 0
    }
  }
  
  theDay  <- as.Date("2021-01-01") + i2
  
  theMax <- max(x$people_fully_vaccinated_per_hundred, na.rm = TRUE)
  
  if(theMax >= 80){
    theDay <- as.Date("2021-01-01") + 
      min(x[x$people_fully_vaccinated_per_hundred >= 80,]$Days, na.rm = TRUE)
    error <- 0
  }
  
  print(paste("80% reached in", country,"on", theDay, "+/-", error))
  
  return(list(p, theDay, theMax, f2, error))
}

dTime <- matrix(nrow = length(countries), ncol = 5)
dTime2 <- matrix(nrow = length(continents), ncol = 5)

for (country in countries) {
  plotAndFun <- get_date(country)
  
  FirstNAindex <- min( which(is.na(dTime[,1])) )
  
  dTime[FirstNAindex, 1] <- country
  dTime[FirstNAindex, 2] <- as.character(plotAndFun[[2]])
  dTime[FirstNAindex, 3] <- 12 * log10(1 + 
      population$population[population$location == country]*10./max(population$population) )
  dTime[FirstNAindex, 4] <- plotAndFun[[3]]
  dTime[FirstNAindex, 5] <- plotAndFun[[5]]
  
  plot(plotAndFun[[1]])
  plot(plotFun(plotAndFun[[4]], col = "darkgreen", add = TRUE))
}

for (continent in continents) {
  plotAndFun <- get_date(continent)
  
  FirstNAindex <- min( which(is.na(dTime2[,1])) )
  
  dTime2[FirstNAindex, 1] <- continent
  dTime2[FirstNAindex, 2] <- as.character(plotAndFun[[2]])
  dTime2[FirstNAindex, 3] <- 12 * log10(1 + 
                                         population$population[population$location == country]*10./max(population$population) )
  dTime2[FirstNAindex, 4] <- plotAndFun[[3]]
  dTime2[FirstNAindex, 5] <- plotAndFun[[5]]
  
  plot(plotAndFun[[1]])
  plot(plotFun(plotAndFun[[4]], col = "darkgreen", add = TRUE))
}

# Fix format
dTime <- as.data.frame(dTime)
names(dTime) <- c("Country", "Date", "Position", "Total", "Error")
dTime <- dTime[with(dTime, order(Date)), ]
dTime$Date <- ymd(dTime$Date)
dTime$Position <- as.numeric(dTime$Position)
dTime$Total <- as.numeric(dTime$Total)
dTime$Error <- as.numeric(dTime$Error)

# Fix format for continents
dTime2 <- as.data.frame(dTime2)
names(dTime2) <- c("Continent", "Date", "Position", "Total", "Error")
dTime2 <- dTime2[with(dTime2, order(Date)), ]
dTime2$Date <- ymd(dTime2$Date)
dTime2$Position <- as.numeric(dTime2$Position)
dTime2$Total <- as.numeric(dTime2$Total)
dTime2$Error <- as.numeric(dTime2$Error)

# Bar plot for countries
dTime %>%
  arrange(desc(Total)) %>%
  ggplot(aes(x = reorder(Country, Total), weight = Total)) +
  geom_bar(aes(fill = Total)) +
  geom_text(aes(x = reorder(Country, Total), y = as.numeric(Total) +3,
                            label = paste0(round(Total, digits = 1), "%"),
                            col = "darkred"), 
            size=3, show.legend = FALSE) +
  scale_fill_gradient2( high = "darkred") +
  guides(fill = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(x = "Country", y = "People fully vaccinated (%)")
ggsave("bars_countries.png")


# Bar plot for continents
dTime2 %>%
  arrange(desc(Total)) %>%
  ggplot(aes(x = reorder(Continent, Total), weight = Total)) +
  geom_bar(aes(fill = Total)) +
  geom_text(aes(x = reorder(Continent, Total), y = as.numeric(Total) +3,
                label = paste0(round(Total, digits = 1), "%")),
            size=3, show.legend = FALSE) +
  scale_fill_gradient2( high = "darkgreen") +
  guides(fill = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(x = "Continent", y = "People fully vaccinated (%)")
ggsave("bars_continents.png")

# Print the last data entry for Spain
print("Last data entry for Spain:")
print(data %>%
  filter(location == "Spain") %>%
  filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
  summarise(LastDay = max(date), Total_vacc = max(people_fully_vaccinated_per_hundred)))

# X-wing chart
LastDay <- max(data$date)

dTime %>%
  filter(Country != "Ethiopia") %>%
  filter(Country != "Nigeria") %>%
  ggplot(aes(col = Country, 
             y=rank(Date, ties.method = "first"), 
             x=as.Date(Date) ))  +
  geom_point(show.legend = FALSE) +
  geom_errorbarh(aes(xmin = as.Date(Date)-Error, xmax = as.Date(Date)+Error), show.legend = FALSE) +
  geom_text(aes(x = as.Date(Date)+Error+100+6*nchar(Country), label=paste0(Country)), show.legend = FALSE ) +
  labs(x = "Date", y = "Country)") +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_vline(xintercept = as.Date(LastDay), col = "red", size = 1) +
  annotate("text", x=as.Date("2025-01-01"), y=9, label= "Predicted date for 80% population fully vaccinated") +
  annotate("text", x=as.Date("2025-01-01"), y=7.75, label= paste("Using data up to", LastDay)) +
  annotate("text", x=as.Date("2025-01-01"), y=6.5, label= "Model by Bruno Zamorano") +
  annotate("text", x=as.Date("2025-01-01"), y=5.25, label= "Data taken from https://ourworldindata.org") +
ggsave("xwing_countries.png")

dTime2 %>%
  ggplot(aes(col = Continent, 
             y=rank(Date, ties.method = "first"), 
             x=as.Date(Date) ))  +
  geom_point(show.legend = FALSE) +
  geom_errorbarh(aes(xmin = as.Date(Date)-Error, xmax = as.Date(Date)+Error), show.legend = FALSE) +
  geom_text(aes(x = as.Date(Date)+Error+50+5*nchar(Continent), label=paste0(Continent)), show.legend = FALSE ) +
  labs(x = "Date", y = "Continent)") +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_vline(xintercept = as.Date(LastDay), col = "red", size = 1) +
  annotate("text", x=as.Date("2024-01-01"), y=2.5, label= "Predicted date for 80% population fully vaccinated") +
  annotate("text", x=as.Date("2024-01-01"), y=2, label= paste("Using data up to", LastDay)) +
  annotate("text", x=as.Date("2024-01-01"), y=1.5, label= "Model by Bruno Zamorano") +
  annotate("text", x=as.Date("2024-01-01"), y=1, label= "Data taken from https://ourworldindata.org") +
ggsave("xwing_continents.png")

