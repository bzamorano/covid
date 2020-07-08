library(utils)
library(httr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(scales)
#theme_set(theme_bw())

setwd("~/Work/covid")

GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
    authenticate(":", ":", type="ntlm"),
    write_disk(tf <- "./covid_data.csv", overwrite = TRUE))

data <- read.csv2(tf, sep=",", header = TRUE)

# # Population (from Wikipedia)
# pop <- read.csv("./population.csv")
# data<-merge(x=data, y=pop, by.x = "countriesAndTerritories",  by.y = "Country",all.x=TRUE)

# Fix the data first
data[[1]] <- as.POSIXct(strptime(data[[1]], "%d/%m/%Y"))
data <- data %>% arrange(desc(dateRep))

nrows <- nrow(data)

weeknew <- integer(nrows)
tot_sum <- integer(nrows)
weekdeath <- integer(nrows)
death_sum <- integer(nrows)

countries <- unique(data$countriesAndTerritories)

for (country in countries) {
  irows <- (1:nrows)[data$countriesAndTerritories == country]
  dt <- data[irows,]

  n_rows <- nrow(dt)
  
  for (i in 1:n_rows ) {
    min_index <- min(i+7, n_rows)
    
    weeknew[irows[i]] <- sum(dt$cases[i:min_index])
    tot_sum[irows[i]] <- sum(dt$cases[i:n_rows])
    weekdeath[irows[i]] <- sum(dt$deaths[i:min_index])
    death_sum[irows[i]] <- sum(dt$deaths[i:n_rows])
  }
}

data['weekNew'] <- weeknew
data['totCases'] <- tot_sum
data['weekDeath'] <- weekdeath
data['totDeath'] <- death_sum

data%>%
  group_by(dateRep) %>%
  summarise(Cases = sum(totCases)) %>%
  ggplot(aes(x=dateRep, y=Cases)) +
  geom_line() +
  scale_y_log10() +
  labs(x = "Date", y = "Total cases")

# These two charts show that higher mortality rates are most likely linked to the
# ability to count them efficiently than to any wrongdoing. Hence the overrepresentation
# of small (easy to handle) and affluent (resourceful) countries in the top tier, and
# big and poor countries in the lowest death rates

# Top Cases
data %>%
  group_by(countriesAndTerritories) %>%
  summarise(Cases = sum(cases)) %>%
  arrange(desc(Cases)) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(countriesAndTerritories, Cases), weight = Cases)) +
  geom_bar(fill = "darkblue", colour = "darkblue") +
  coord_flip() +
  labs(x = "Country", y = "Cases")

# Top deaths
data %>%
  group_by(countriesAndTerritories) %>%
  summarise(Deaths = sum(deaths)) %>%
  arrange(desc(Deaths)) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(countriesAndTerritories, Deaths), weight = Deaths)) +
  geom_bar(fill = "darkred", colour = "darkred") +
  coord_flip() +
  labs(x = "Country", y = "Deaths")

# Top death rate
data %>%
  group_by(countriesAndTerritories) %>%
  filter(max(popData2019) > 5000) %>%
  filter(sum(deaths) > 100) %>%
  summarise(Deaths = sum(deaths),
            Population = max(popData2019),
            Death_rate = sum(deaths) * 1.e6 / max(popData2019) ) %>%
  arrange(desc(Death_rate)) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(countriesAndTerritories, -Death_rate), weight = Death_rate)) +
  geom_bar(fill = "red", colour = "darkred") +
  coord_flip() +
  annotate("text", x=14.8, y=625, label= "Countries with pop. > 5000") +
  annotate("text", x=14, y=625, label= "and at least 100 deaths") +
  labs(x = "Country", y = "Deaths per million inhabitants")

# Bottom death rate
data %>%
  group_by(countriesAndTerritories) %>%
  filter(max(popData2019) > 5000) %>%
  filter(sum(deaths) > 100) %>%
  summarise(Deaths = sum(deaths),
            Population = max(popData2019),
            Death_rate = sum(deaths) * 1.e6 / max(popData2019) ) %>%
  arrange((Death_rate)) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(countriesAndTerritories, -Death_rate), weight = Death_rate)) +
  geom_bar(fill = "blue", colour = "darkblue") +
  coord_flip() +
  annotate("text", x=14.8, y=5.5, label= "Countries with pop. > 5000") +
  annotate("text", x=14, y=5.5, label= "and at least 100 deaths") +
  labs(x = "Country", y = "Deaths per million inhabitants")

# By continent
data %>%
  group_by(countriesAndTerritories) %>%
  summarise(Deaths = sum(deaths),
            Population = max(popData2019),
            Continent = first(continentExp)) %>%
  group_by(Continent) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE)) %>%
  filter(Population > 5000) %>%
  ggplot(aes(x = Population, y = Deaths)) +
  geom_point(aes(color = Continent), size = 7 ) +
  labs(x = "Population", y = "Deaths")

# By continent (log-scale)
data %>%
  group_by(countriesAndTerritories) %>%
  summarise(Deaths = sum(deaths),
            Population = max(popData2019),
            Continent = first(continentExp)) %>%
  group_by(Continent) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE)) %>%
  filter(Population > 5000) %>%
  ggplot(aes(x = Population, y = Deaths)) +
  geom_point(aes(color = Continent), size = 7 ) +
  geom_smooth(aes(x = Population, y = Deaths), method = "glm", col = "brown") +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Population", y = "Deaths")

# This can be changed to account for more countries. W or W/O South Korea
# countries <- unique(data$countriesAndTerritories[data$totDeath > 10000 
#                                     | data$countriesAndTerritories == "South_Korea"])
countries <- unique(data$countriesAndTerritories[data$totDeath > 10000])
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

# Check a version for deaths (test)
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

# A bit of Shiny!
library(shiny)

dt <- (data %>%
  group_by(countriesAndTerritories) %>%
  summarise(Deaths = sum(deaths)/1e3,
            Population = max(popData2019)/1e6,
            Continent = first(continentExp)) %>%
  arrange(desc(Deaths)) )

ui <- fluidPage(
  # Input
  sliderInput(inputId = "population",
              label = "Population in million",
              value = c(10,350), min = 0, max = 1500, step = 10),
  sliderInput(inputId = "deaths",
              label = "Deaths in thousand",
              value = c(1,150), min = 0, max = 150),
  # Output
  plotOutput(outputId = "graph", hover = hoverOpts(id = "plot_hover")),
  verbatimTextOutput("hover_info")
)

server <- function(input, output) {
 
  get_plot <- function(population, deaths){
    p1 <- dt %>%
      filter(Population >= population[1]) %>%
      filter(Population <= population[2]) %>%
      filter(Deaths >= deaths[1]) %>%
      filter(Deaths <= deaths[2]) %>%
      ggplot(aes(x = Population*1e6, y = Deaths*1e3, col = Continent)) + 
      geom_point(size = 3) + 
      geom_smooth(aes(x = Population*1e6, y = Deaths*1e3), method = "glm", col = "brown") +
      scale_y_log10(labels = comma_format(big.mark = " ")) +
      scale_x_log10(labels = comma_format(big.mark = " ")) +
      labs(x = "Population", y = "Deaths")
    
    return(p1)
  }
  
  output$graph <- renderPlot( { 
    get_plot(input$population, input$deaths)
  } )
  
  output$hover_info <- renderPrint({
    hover <- input$plot_hover
    dist = sqrt((hover$x*1e-3-dt$Population*1e3)^2+(hover$y-dt$Deaths*1e3)^2)
    
    igood <- which.min(dist)
    dx <- hover$x*1e-3 - dt$Population[igood] * 1e3
    dy <- hover$y - dt$Deaths[igood]* 1e3
    
    dmin <- sqrt((dx*dx+dy*dy)/(dt$Population[igood]*dt$Population[igood]*1e6 + 
                                  dt$Deaths[igood]*dt$Deaths[igood]* 1e6))
    if(dmin < 3){
      cat("Country: ")
      cat(format(dt$countriesAndTerritories[igood]))
      cat("\nRanking: ")
      cat(which.min(dist))
      cat("\nPopulation: ")
      cat(dt$Population[igood] * 1e6)
      cat("\nDeaths: ")
      cat(dt$Deaths[igood]* 1e3)
    }
  })
  
}

shinyApp(ui = ui, server = server)