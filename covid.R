library(utils)
library(httr)
library(ggplot2)
library(tidyverse)
library(tidyr)
#theme_set(theme_bw())

GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
    authenticate(":", ":", type="ntlm"),
    write_disk(tf <- "./covid_data.csv", overwrite = TRUE))

data <- read.csv2(tf, sep=",", header = TRUE)

# Fix the data first
data[[1]] <- as.POSIXct(strptime(data[[1]], "%d/%m/%Y"))

nrows <- nrow(data)

weeknew <- integer(nrows)
tot_sum <- integer(nrows)
weekdeath <- integer(nrows)
death_sum <- integer(nrows)

countries <- unique(data$countriesAndTerritories)

for (country in countries) {
  irows <- (1:nrows)[data$countriesAndTerritories == country]
  n_rows <- length(irows)
  
  for (i in irows ) {
    min_index <- min(i+7, irows[n_rows])
    
    weeknew[i] <- sum(data$cases[i:min_index])
    tot_sum[i] <- sum(data$cases[i:irows[n_rows]])
    weekdeath[i] <- sum(data$deaths[i:min_index])
    death_sum[i] <- sum(data$deaths[i:irows[n_rows]])
  }
}

data['weekNew'] <- weeknew
data['totCases'] <- tot_sum
data['weekDeath'] <- weekdeath
data['totDeath'] <- death_sum

dt_tot <- acumulados  %>%
  group_by(Fecha) %>%
  summarise(Casos = sum(Casos),
            Fallecidos = sum(Fallecidos),
            Hospitalizados = sum(Hospitalizados),
            Recuperados = sum(Recuperados),
            UCI = sum(UCI)
  )

data%>%
  group_by(dateRep) %>%
  summarise(Cases = sum(totCases)) %>%
  ggplot(aes(x=dateRep, y=Cases)) +
  geom_line() +
  scale_y_log10() +
  labs(x = "Date", y = "Total cases")

# This can be changed to account for more countries
countries <- unique(data$countriesAndTerritories[data$totDeath > 1000 
                                    | data$countriesAndTerritories == "South_Korea"])
small <- data[data$countriesAndTerritories %in% countries,]

small%>%
  ggplot(aes(x=dateRep, y=cases, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "Daily cases", colour = "Country")

small%>%
  ggplot(aes(x=dateRep, y=totCases, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_y_log10() +
  labs(x = "Date", y = "Total cases", colour = "Country")

small%>%
  ggplot(aes(x=dateRep, y=deaths, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "Daily deaths", colour = "Country")

small%>%
  ggplot(aes(x=dateRep, y=totDeath, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_y_log10() +
  labs(x = "Date", y = "Total deaths", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y=totDeath *100. / totCases, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "Deaths / cases (%)", colour = "Country")

library(scales)

# Check a couple versions for deaths (test)
small[(small$totCases > 100), ]%>%
  ggplot(aes(x=totCases, y=weekDeath, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total cases", y = "Weekly deaths", colour = "Country")

small[(small$totDeath > 100), ]%>%
  ggplot(aes(x=totDeath, y=weekDeath, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total deaths", y = "Weekly deaths", colour = "Country")


# But the real deal is this:

small[(small$totCases > 100), ]%>%
  ggplot(aes(x=totCases, y=weekNew, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total cases", y = "Weekly new cases", colour = "Country")