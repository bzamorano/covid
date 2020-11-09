library(utils)
library(httr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(scales)
theme_set(theme_minimal())

# Some style definitions #
blank_theme <- theme_minimal()+
  theme(
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )
my_percent <- label_percent( accuracy = 1)

# Download the data
setwd("~/Work/covid")

GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
    authenticate(":", ":", type="ntlm"),
    write_disk(tf <- "./covid_data.csv", overwrite = TRUE))

data <- read.csv2(tf, sep=",", header = TRUE)

# Replace some names to make plots nicer
data[data=="United_States_of_America"]<-"USA"
data[data=="United_Kingdom"]<-"UK"
data[data=="Democratic_Republic_of_the_Congo"]<-"DR_Congo"

spain <- data[data$countriesAndTerritories == "Spain",]

# # Population (from Wikipedia)
# pop <- read.csv("./population.csv")
# data<-merge(x=data, y=pop, by.x = "countriesAndTerritories",  by.y = "Country",all.x=TRUE)

# Fix the data first
data[[1]] <- as.POSIXct(strptime(data[[1]], "%d/%m/%Y"))
data <- data %>% arrange(desc(dateRep))

nrows <- nrow(data)

fortnightnew <- integer(nrows)
weeknew <- integer(nrows)
tot_sum <- integer(nrows)
fortnightdeath <- integer(nrows)
weekdeath <- integer(nrows)
death_sum <- integer(nrows)

countries <- unique(data$countriesAndTerritories)

for (country in countries) {
  irows <- (1:nrows)[data$countriesAndTerritories == country]
  dt <- data[irows,]

  n_rows <- nrow(dt)
  
  for (i in 1:n_rows ) {
    min_index <- min(i+7, n_rows)
    min_index2 <- min(i+14, n_rows)
    
    fortnightnew[irows[i]] <- sum(dt$cases[i:min_index2])
    weeknew[irows[i]] <- sum(dt$cases[i:min_index])
    tot_sum[irows[i]] <- sum(dt$cases[i:n_rows])
    weekdeath[irows[i]] <- sum(dt$deaths[i:min_index])
    fortnightdeath[irows[i]] <- sum(dt$deaths[i:min_index2])
    death_sum[irows[i]] <- sum(dt$deaths[i:n_rows])
  }
}

data['totCases'] <- tot_sum
data['fortnightNew'] <- fortnightnew
data['weekNew'] <- weeknew
data['totDeath'] <- death_sum
data['fortnightDeath'] <- fortnightdeath
data['weekDeath'] <- weekdeath


rm(dt, countries, country, death_sum, i, irows, min_index, min_index2, n_rows, tf, 
   tot_sum, weekdeath, weeknew, fortnightnew, nrows)

data%>%
  group_by(dateRep) %>%
  summarise(Cases = sum(totCases)) %>%
  ggplot(aes(x=dateRep, y=Cases)) +
  geom_line(colour = "red", size = 1.25) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
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
  geom_bar(aes(fill = Cases)) +
  scale_fill_gradient2( high = "darkblue") +
  theme_minimal() +
  guides(fill = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(x = "Country", y = "Cases")

# Top deaths
data %>%
  group_by(countriesAndTerritories) %>%
  summarise(Deaths = sum(deaths)) %>%
  arrange(desc(Deaths)) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(countriesAndTerritories, Deaths), weight = Deaths)) +
  geom_bar(aes(fill = Deaths)) +
  scale_fill_gradient2(high = "red") +
  theme_minimal() +
  guides(fill = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(x = "Country", y = "Deaths")

# Top cases rate
data %>%
  group_by(countriesAndTerritories) %>%
  filter(max(popData2019) > 5000) %>%
  filter(sum(deaths) > 100) %>%
  summarise(Cases = sum(cases),
            Population = max(popData2019),
            Cases_rate = sum(cases) * 1.e3 / max(popData2019) ) %>%
  arrange(desc(Cases_rate)) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(countriesAndTerritories, -Cases_rate), weight = Cases_rate)) +
  geom_bar(fill = "orange", colour = "orange") +
  coord_flip() +
  annotate("text", x=14.8, y=40, label= "Countries with pop. > 5000") +
  annotate("text", x=14, y=40, label= "and at least 100 deaths") +
  labs(x = "Country", y = "Cases per thousand inhabitants")

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
  geom_bar(fill = "darkred", colour = "darkred") +
  coord_flip() +
  annotate("text", x=14.8, y=900, label= "Countries with pop. > 5000") +
  annotate("text", x=14, y=900, label= "and at least 100 deaths") +
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
  annotate("text", x=14.8, y=10, label= "Countries with pop. > 5000") +
  annotate("text", x=14, y=10, label= "and at least 100 deaths") +
  labs(x = "Country", y = "Deaths per million inhabitants")

# Continent timeline
data_continents <- data %>%
  group_by(continentExp, dateRep) %>%
  summarise(Deaths = sum(deaths),
            Cases = sum(cases))

nrows <- nrow(data_continents)
death_sum <- integer(nrows)
cases_sum <- integer(nrows)

continents <- unique(data_continents$continentExp)

for (continent in continents) {
  irows <- (1:nrows)[data_continents$continentExp == continent]
  dt <- data_continents[irows,]
  
  n_rows <- nrow(dt)
  
  for (i in 1:n_rows ) {
    death_sum[irows[i]] <- sum(dt$Deaths[1:i])
    cases_sum[irows[i]] <- sum(dt$Cases[1:i])
  }
}

data_continents['totDeath'] <- death_sum
data_continents['totCases'] <- cases_sum

rm(dt, cases_sum, continent, continents, death_sum, i, irows, n_rows, nrows)

#Timelines by continent
data_continents  %>%
  group_by(dateRep, continentExp) %>%
  filter(continentExp != "Other") %>%
  ggplot(aes(x = dateRep, y = totCases, colour = continentExp)) +
  geom_line(size=1) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Date", y = "Total cases", colour = "Continent")

data_continents  %>%
  group_by(dateRep, continentExp) %>%
  filter(continentExp != "Other") %>%
  ggplot(aes(x = dateRep, y = totDeath, colour = continentExp)) +
  geom_line(size=1) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Date", y = "Total deaths", colour = "Continent")

# Fraction of cases vs time
data_continents  %>%
  group_by(dateRep, continentExp) %>%
  summarise(n = sum(totCases)) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = dateRep, y = percentage, fill = continentExp)) +
  geom_area() +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Date", y = "Fraction of cases", fill = "Continent")

# Fraction of deaths vs time
data_continents  %>%
  group_by(dateRep, continentExp) %>%
  summarise(n = sum(totDeath)) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = dateRep, y = percentage, fill = continentExp)) +
  geom_area() +
  scale_fill_brewer(palette = "Set3") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Date", y = "Fraction of deaths", fill = "Continent")

# Get data by continent
data_continents <- data %>%
  group_by(countriesAndTerritories) %>%
  summarise(Deaths = sum(deaths),
            Population = max(popData2019),
            Continent = first(continentExp)) %>%
  group_by(Continent)

TotalWorldDeaths <- sum(data_continents$Deaths, na.rm = TRUE)
TotalWorldPop <- sum(data_continents$Population, na.rm = TRUE)

# World population
data_continents %>%
  group_by(Continent) %>%
  summarise(Population = max(Population, na.rm = TRUE)) %>%
  filter(Population > 1) %>%
  ggplot(aes(x = "", y = Population, fill = Continent)) +
  geom_bar(width = 1, stat = "identity" ) +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette = "Set3") +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(label = my_percent(Population/sum(Population))), position = position_stack(vjust = 0.5), check_overlap = TRUE) +
  labs(x = "", y = paste("Total World population:", TotalWorldPop) )

#Death distribution pie
data_continents %>%
  group_by(Continent) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>%
  filter(Deaths > 10) %>%
  ggplot(aes(x = "", y = Deaths, fill = Continent)) +
  geom_bar(width = 1, stat = "identity" ) +
  coord_polar("y", start=0) +
  scale_fill_brewer(palette = "Set3") +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(label = my_percent(Deaths/sum(Deaths))), position = position_stack(vjust = 0.5), check_overlap = TRUE) +
  labs(x = "", y = paste("Total covid-19 deaths:", TotalWorldDeaths) )

# By continent (log-scale)
data_continents %>%
  group_by(Continent) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE)) %>%
  filter(Population > 5000) %>%
  ggplot(aes(x = Population, y = Deaths)) +
  geom_point(aes(color = Continent), size = 7 ) +
  geom_smooth(aes(x = Population, y = Deaths), method = "glm", col = "brown") +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_colour_brewer(palette = "Set3") +
  labs(x = "Population", y = "Deaths")

rm(TotalWorldDeaths, TotalWorldPop)

# This can be changed to account for more countries. W or W/O South Korea
# countries <- unique(data$countriesAndTerritories[data$totDeath > 10000 
#                                     | data$countriesAndTerritories == "South_Korea"])
countries <- unique(data$countriesAndTerritories[data$totDeath > 25000])
small <- data[data$countriesAndTerritories %in% countries,]
rm(countries)

# Reorder by case appearance
small$countriesAndTerritories <- with(small, reorder(countriesAndTerritories, desc(totCases)))

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y = weekNew*(weekNew>0), colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Date", y = "Weekly cases", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  filter(countriesAndTerritories == "Spain") %>%
  ggplot(aes(x=dateRep, y = weekNew*(weekNew>0), colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "Weekly cases", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y = fortnightNew*(fortnightNew>0), colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Date", y = "14-days cases", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  filter(countriesAndTerritories == "Spain") %>%
  ggplot(aes(x=dateRep, y = fortnightNew*(fortnightNew>0), colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "14-days cases", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y=totCases, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Date", y = "Total cases", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y = weekDeath*(weekDeath>0), colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "Weekly deaths", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  filter(countriesAndTerritories == "Spain") %>%
  ggplot(aes(x=dateRep, y = weekDeath*(weekDeath>0), colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "Weekly deaths", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y = fortnightDeath*(fortnightDeath>0), colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "14-days deaths", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  filter(countriesAndTerritories == "Spain") %>%
  ggplot(aes(x=dateRep, y = fortnightDeath*(fortnightDeath>0), colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "14-days deaths", colour = "Country")

### Summary for Spain since summer ###
# First without scaling
small[(small$dateRep > "2020-02-29"), ]%>%
  filter(countriesAndTerritories == "Spain") %>%
  ggplot(aes(x = dateRep)) +
  geom_line(aes(y = fortnightNew*(fortnightNew > 0)), colour = "blue", size = 1) + 
  geom_line(aes(y = fortnightDeath*(fortnightDeath > 0)), colour = "red", size = 1) +
  labs(x = "Date", y = "Fortnightly cases and deaths")

# And now scaling (focusing on second surge)
small[(small$dateRep > "2020-07-01"), ]%>%
  filter(countriesAndTerritories == "Spain") %>%
  ggplot(aes(x = dateRep)) +
  geom_line(aes(y = weekNew*(weekNew > 0)), colour = "blue", size = 1) + 
  geom_line(aes(y = 10*weekDeath*(weekDeath > 0)), colour = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 10, name = "Weekly deaths")) +
  theme( axis.line.y.right = element_line(color = "red"), 
         axis.ticks.y.right = element_line(color = "red"),
         axis.text.y.right = element_text(color = "red"),
         axis.title.y.right = element_text(color = "red")) +
  theme( axis.line.y.left = element_line(color = "blue"), 
         axis.ticks.y.left = element_line(color = "blue"),
         axis.text.y.left = element_text(color = "blue"),
         axis.title.y.left = element_text(color = "blue")) +
  labs(x = "Date", y = "Weekly cases")

### Animation! ###
library(gganimate)

p <- small[(small$dateRep > "2020-02-29"), ]%>%
  filter(countriesAndTerritories == "Spain") %>%
  ggplot(aes(x = dateRep)) +
  geom_line(aes(y = weekNew*(weekNew > 0)), colour = "blue", size = 1) + 
  geom_line(aes(y = 10*weekDeath*(weekDeath > 0)), colour = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 10, name = "Weekly deaths")) +
  theme( axis.line.y.right = element_line(color = "red"), 
         axis.ticks.y.right = element_line(color = "red"),
         axis.text.y.right = element_text(color = "red"),
         axis.title.y.right = element_text(color = "red")) +
  theme( axis.line.y.left = element_line(color = "blue"), 
         axis.ticks.y.left = element_line(color = "blue"),
         axis.text.y.left = element_text(color = "blue"),
         axis.title.y.left = element_text(color = "blue")) +
  labs(x = "Date", y = "Weekly cases")

p
p + transition_reveal(dateRep)
anim_save(filename = "spain_evolution.gif")
rm(p)

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y=totDeath, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Date", y = "Total deaths", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y=totDeath *100. / totCases, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "Deaths / cases (%)", colour = "Country")

# Evolution of the pandemic
small[(small$totCases > 100), ]%>%
  ggplot(aes(x=totCases, y=weekNew, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  geom_line((aes(x=totCases, y=totCases)), colour = "grey" ) +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total cases", y = "Weekly new cases", colour = "Country")

# And in terms of deaths
small[(small$totDeath > 100), ]%>%
  ggplot(aes(x=totDeath, y=weekDeath, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  geom_line((aes(x=totDeath, y=totDeath)), colour = "grey" ) +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total deaths", y = "Weekly deaths", colour = "Country")


#### Interactive graphic ####
# A bit of Shiny!
library(shiny)
dt <- (data %>%
  group_by(countriesAndTerritories) %>%
  summarise(Deaths = sum(deaths)/1e3,
            Population = max(popData2019)/1e6,
            Continent = first(continentExp)) %>%
  arrange(desc(Deaths)) %>%
  filter(Deaths > 0))

ui <- fluidPage(
  # Input
  sliderInput(inputId = "population",
              label = "Population in million",
              value = c(0,350), min = 0, max = 1500, step = 10),
  sliderInput(inputId = "deaths",
              label = "Deaths in thousand",
              value = c(0,300), min = 0, max = 500, step = 10),
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