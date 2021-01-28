# Libraries
library(utils)
library(httr)
library(tidyverse)
library(scales)

# Style
theme_set(theme_minimal())
ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set1")
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
data[data=="Bosnia_and_Herzegovina"]<-"Bosnia_Herz"

# # Population (from Wikipedia)
# pop <- read.csv("./population.csv")
# data<-merge(x=data, y=pop, by.x = "countriesAndTerritories",  by.y = "Country",all.x=TRUE)

# Fix the data first
data[[1]] <- as.POSIXct(strptime(data[[1]], "%d/%m/%Y"))
data <- data %>% arrange(desc(dateRep))
data[[10]] <- as.numeric(data[[10]])
colnames(data)[colnames(data) == "notification_rate_per_100000_population_14.days"] <- "incidence"

nrows <- nrow(data)

tot_sum <- integer(nrows)
death_sum <- integer(nrows)

countries <- unique(data$countriesAndTerritories)

for (country in countries) {
  irows <- (1:nrows)[data$countriesAndTerritories == country]
  dt <- data[irows,]

  n_rows <- nrow(dt)
  
  for (i in 1:n_rows ) {
    tot_sum[irows[i]] <- sum(dt$cases_weekly[i:n_rows])
    death_sum[irows[i]] <- sum(dt$deaths_weekly[i:n_rows])
  }
}

data['totCases'] <- tot_sum
data['totDeaths'] <- death_sum

spain <- data[data$countriesAndTerritories == "Spain",]

rm(dt, countries, country, death_sum, i, irows, n_rows, tf, tot_sum, nrows)

dd <- data%>%
  group_by(dateRep) %>%
  summarise(Cases = sum(totCases), Deaths = sum(totDeaths))

TotWorldCases <- max(dd$Cases)
TotWorldDeaths <- max(dd$Deaths)
LastDay <- max(dd$dateRep)
rm(dd)

#Global summary

data%>%
  group_by(dateRep) %>%
  summarise(Cases = sum(totCases), Deaths = sum(totDeaths)) %>%
  ggplot(aes(x=dateRep)) +
  geom_line(aes(y=Cases), colour = "blue", size = 1.25) +
  geom_line(aes(y=Deaths), colour = "red", size = 1.25) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Date", y = "Total cases") +
  annotate("text", x=LastDay-86400*60, y=15000, label= paste("Total cases:", TotWorldCases)) +
  annotate("text", x=LastDay-86400*60, y=3000, label= paste("Total deaths:", TotWorldDeaths))

# These two charts show that higher mortality rates are most likely linked to the
# ability to count them efficiently than to any wrongdoing. Hence the overrepresentation
# of small (easy to handle) and affluent (resourceful) countries in the top tier, and
# big and poor countries in the lowest death rates

# Top Cases
data %>%
  group_by(countriesAndTerritories) %>%
  summarise(Cases = max(totCases)) %>%
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
  summarise(Deaths = max(totDeaths)) %>%
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
  filter(max(totDeaths) > 100) %>%
  summarise(Cases = max(totCases),
            Population = max(popData2019),
            Cases_rate = max(totCases) * 1.e3 / max(popData2019) ) %>%
  arrange(desc(Cases_rate)) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(countriesAndTerritories, -Cases_rate), weight = Cases_rate)) +
  geom_bar(fill = "orange", colour = "orange") +
  coord_flip() +
  annotate("text", x=14.8, y=80, label= "Countries with pop. > 5000") +
  annotate("text", x=14, y=80, label= "and at least 100 deaths") +
  labs(x = "Country", y = "Cases per thousand inhabitants")

# Top death rate
data %>%
  group_by(countriesAndTerritories) %>%
  filter(max(popData2019) > 5000) %>%
  filter(max(totDeaths) > 100) %>%
  summarise(Deaths = max(totDeaths),
            Population = max(popData2019),
            Death_rate = max(totDeaths) * 1.e6 / max(popData2019) ) %>%
  arrange(desc(Death_rate)) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(countriesAndTerritories, -Death_rate), weight = Death_rate)) +
  geom_bar(fill = "darkred", colour = "darkred") +
  coord_flip() +
  annotate("text", x=14.8, y=1570, label= "Countries with pop. > 5000") +
  annotate("text", x=14, y=1570, label= "and at least 100 deaths") +
  labs(x = "Country", y = "Deaths per million inhabitants")

# Bottom death rate
data %>%
  group_by(countriesAndTerritories) %>%
  filter(max(popData2019) > 5000) %>%
  filter(max(totDeaths) > 100) %>%
  summarise(Deaths = max(totDeaths),
            Population = max(popData2019),
            Death_rate = max(totDeaths) * 1.e6 / max(popData2019) ) %>%
  arrange((Death_rate)) %>%
  slice(1:15) %>%
  ggplot(aes(x = reorder(countriesAndTerritories, -Death_rate), weight = Death_rate)) +
  geom_bar(fill = "blue", colour = "darkblue") +
  coord_flip() +
  annotate("text", x=14.8, y=12, label= "Countries with pop. > 5000") +
  annotate("text", x=14, y=12, label= "and at least 100 deaths") +
  labs(x = "Country", y = "Deaths per million inhabitants")

# Continent timeline
data_continents <- data %>%
  group_by(continentExp, dateRep) %>%
  summarise(Deaths = sum(deaths_weekly),
            Cases = sum(cases_weekly))

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
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Date", y = "Fraction of cases", fill = "Continent")

# Fraction of deaths vs time
data_continents  %>%
  group_by(dateRep, continentExp) %>%
  summarise(n = sum(totDeath)) %>%
  mutate(percentage = n / sum(n)) %>%
  ggplot(aes(x = dateRep, y = percentage, fill = continentExp)) +
  geom_area() +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Date", y = "Fraction of deaths", fill = "Continent")

# Get data by continent
data_continents <- data %>%
  group_by(countriesAndTerritories) %>%
  summarise(Deaths = sum(deaths_weekly),
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
  scale_fill_brewer(palette = "Set1") +
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
  scale_fill_brewer(palette = "Set1") +
  blank_theme +
  theme(axis.text.x=element_blank()) +
  geom_text(aes(label = my_percent(Deaths/sum(Deaths))), position = position_stack(vjust = 0.5), check_overlap = TRUE) +
  labs(x = "", y = paste("Total covid-19 deaths:", TotalWorldDeaths) )

# Mortality rate by contintents
data_continents %>%
  group_by(Continent) %>%
  summarise(Deaths = sum(Deaths, na.rm = TRUE),
            Population = sum(Population, na.rm = TRUE),
            Death_rate = Deaths * 1.e6 / Population ) %>%
  filter(Population > 0) %>%
  arrange(desc(Death_rate)) %>%
  ggplot(aes(x = reorder(Continent, -Death_rate), weight = Death_rate )) +
  geom_bar(fill = "darkred", colour = "darkred") +
  coord_flip() +
  labs(x = "Continent", y = "Deaths per million inhabitants")

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
  labs(x = "Population", y = "Deaths")

rm(TotalWorldDeaths, TotalWorldPop, data_continents)

# This can be changed to account for more countries
 countries <- unique(data$countriesAndTerritories[data$totDeath > 60000 
                                     | data$countriesAndTerritories == "Spain"])
small <- data[data$countriesAndTerritories %in% countries,]
rm(countries)

# Reorder by case appearance
small$countriesAndTerritories <- with(small, reorder(countriesAndTerritories, desc(totCases)))

# Incidence rate
small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, 
             y = incidence,
             colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "14-days cases per 100k inhabitants", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y = cases_weekly*(cases_weekly>0), colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Date", y = "Weekly cases", colour = "Country")

spain[(spain$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y = cases_weekly*(cases_weekly>0))) +
  geom_line(size=1, colour = "red") +
  labs(x = "Date", y = "Weekly cases", title = "Spain")

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y=totCases, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Date", y = "Total cases", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y = deaths_weekly*(deaths_weekly>0), colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "Weekly deaths", colour = "Country")

spain[(spain$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y = deaths_weekly*(deaths_weekly>0))) +
  geom_line(size=1, colour = "red") +
  labs(x = "Date", y = "Weekly deaths", title = "Spain")

### Summary for Spain since summer ###
# First without scaling
spain[(spain$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x = dateRep)) +
  geom_line(aes(y = cases_weekly*(cases_weekly > 0)), colour = "blue", size = 1) + 
  geom_line(aes(y = deaths_weekly*(deaths_weekly > 0)), colour = "red", size = 1) +
  labs(x = "Date", y = "Fortnightly cases and deaths")

# And now scaling (focusing on second surge)
spain[(spain$dateRep > "2020-07-01"), ]%>%
  ggplot(aes(x = dateRep)) +
  geom_line(aes(y = cases_weekly*(cases_weekly > 0)), colour = "blue", size = 1) + 
  geom_line(aes(y = 10*deaths_weekly*(deaths_weekly > 0)), colour = "red", size = 1) +
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

p <- spain[(spain$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x = dateRep)) +
  geom_line(aes(y = cases_weekly*(cases_weekly > 0)), colour = "blue", size = 1) + 
  geom_line(aes(y = 10*deaths_weekly*(deaths_weekly > 0)), colour = "red", size = 1) +
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

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y=totDeaths, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Date", y = "Total deaths", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y=totDeaths *100. / totCases, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "Deaths / cases (%)", colour = "Country")

# Evolution of the pandemic
small[(small$totCases > 100), ]%>%
  ggplot(aes(x=totCases, y=cases_weekly, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  geom_line((aes(x=totCases, y=totCases)), colour = "grey" ) +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total cases", y = "Weekly new cases", colour = "Country")

# Evolution of the pandemic in Spain only
spain[(spain$totCases > 100), ]%>%
  ggplot(aes(x=totCases, y=cases_weekly, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  geom_line((aes(x=totCases, y=totCases)), colour = "grey" ) +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total cases", y = "Weekly new cases", colour = "Country")

# And in terms of deaths
small[(small$totDeaths > 100), ]%>%
  ggplot(aes(x=totDeaths, y=deaths_weekly, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  geom_line((aes(x=totDeaths, y=totDeaths)), colour = "grey" ) +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total deaths", y = "Weekly deaths", colour = "Country")

#### Interactive graphic ####
# A bit of Shiny!
library(shiny)
dt <- (data %>%
  group_by(countriesAndTerritories) %>%
  summarise(Deaths = sum(deaths_weekly)/1e3,
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
              value = c(0,500), min = 0, max = 500, step = 10),
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