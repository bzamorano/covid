library(utils)
library(httr)
library(ggplot2)
library(tidyverse)
library(tidyr)
library(scales)
theme_set(theme_minimal())

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
  annotate("text", x=14.8, y=9.5, label= "Countries with pop. > 5000") +
  annotate("text", x=14, y=9.5, label= "and at least 100 deaths") +
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

#Timelines by continent
data_continents  %>%
  group_by(dateRep, continentExp) %>%
  filter(continentExp != "Other") %>%
  ggplot(aes(x = dateRep, y = totCases, colour = continentExp)) +
  geom_line(size=1) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Date", y = "Total cases")

data_continents  %>%
  group_by(dateRep, continentExp) %>%
  filter(continentExp != "Other") %>%
  ggplot(aes(x = dateRep, y = totDeath, colour = continentExp)) +
  geom_line(size=1) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Date", y = "Total deaths")


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

blank_theme <- theme_minimal()+
  theme(
    panel.border = element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

my_percent <- label_percent( accuracy = 1)

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

## Animation!
#date_list <- seq.Date(from = as.Date("2020-01-01"), to = Sys.Date()-2, by = "day")

#for (date in date_list) {
#  date <- as.Date(date, origin = "1970-01-01")
#  anim <- data[data$dateRep<= date,] %>%
#    group_by(countriesAndTerritories) %>%
#    summarise(Deaths = sum(deaths),
#              Population = max(popData2019),
#              Continent = first(continentExp)) %>%
#    group_by(Continent)
  
#  TotalWorldDeaths <- sum(anim$Deaths, na.rm = TRUE)

#  p <- anim %>%
#    summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>%
#    ggplot(aes(x = "", y = Deaths, fill = Continent)) +
#    geom_bar(width = 1, stat = "identity" ) +
#    coord_polar("y", start=0) +
#    scale_fill_brewer(palette = "Set1") +
#    blank_theme +
#    theme(axis.text.x=element_blank()) +
#    geom_text(aes(label = my_percent(Deaths/sum(Deaths))), position = position_stack(vjust = 0.5), check_overlap = TRUE) +
#    labs(x = "", y = paste("Total covid-19 deaths:", TotalWorldDeaths) )
  
#  plot(p)
#  ggsave(filename = paste0("fig/fig_", date,".png"))
#}

#library(magick)
#library(animation)
#library(purrr)

#list.files(path = "./fig", pattern = "*.png", full.names = TRUE) %>% 
#  map(image_read) %>% # reads each path file
#  image_join() %>% # joins image
#  image_animate(fps=5) %>% # animates
#  image_write("animation.gif") # write to current dir

# This can be changed to account for more countries. W or W/O South Korea
# countries <- unique(data$countriesAndTerritories[data$totDeath > 10000 
#                                     | data$countriesAndTerritories == "South_Korea"])
countries <- unique(data$countriesAndTerritories[data$totDeath > 10000])
small <- data[data$countriesAndTerritories %in% countries,]

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y = weekNew*(weekNew>0), colour = countriesAndTerritories)) +
  geom_line(size=1) +
  labs(x = "Date", y = "Weekly cases", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  filter(countriesAndTerritories == "Spain") %>%
  ggplot(aes(x=dateRep, y = weekNew*(weekNew>0), colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_y_log10() +
  labs(x = "Date", y = "Weekly cases", colour = "Country")

small[(small$dateRep > "2020-02-29"), ]%>%
  ggplot(aes(x=dateRep, y=totCases, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  scale_y_log10() +
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
  geom_line((aes(x=totDeath, y=totDeath)), colour = "grey" ) +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total deaths", y = "Weekly deaths", colour = "Country")

# But the real deal is this:
small[(small$totCases > 100), ]%>%
  ggplot(aes(x=totCases, y=weekNew, colour = countriesAndTerritories)) +
  geom_line(size=1) +
  geom_line((aes(x=totCases, y=totCases)), colour = "grey" ) +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total cases", y = "Weekly new cases", colour = "Country")


#### Interactive graphic ####
# A bit of Shiny!
library(shiny)

ui2 <- fluidPage(
  dateInput("date1", "Date:", value = "2020-07-26"),
  
  plotOutput(outputId = "pie")
)

server2 <- function(input, output) {
  
  get_plot <- function(date1){
    data_continents <- data[data$dateRep<=date1,] %>%
      group_by(countriesAndTerritories) %>%
      summarise(Deaths = sum(deaths),
                Population = max(popData2019),
                Continent = first(continentExp)) %>%
      group_by(Continent)
    
    TotalWorldDeaths <- sum(data_continents$Deaths, na.rm = TRUE)
    TotalWorldPop <- sum(data_continents$Population, na.rm = TRUE)
    
    pie1 <- data_continents %>%
      group_by(Continent) %>%
      summarise(Deaths = sum(Deaths, na.rm = TRUE)) %>%
      ggplot(aes(x = "", y = Deaths, fill = Continent)) +
      geom_bar(width = 1, stat = "identity" ) +
      coord_polar("y", start=0) +
      scale_fill_brewer(palette = "Set1") +
      theme(axis.text.x=element_blank()) +
      labs(x = "", y = paste("Total deaths:", TotalWorldDeaths) )
    
    return(pie1)
  }
  
  output$pie <- renderPlot( { 
    get_plot(input$date1)
  } )
  
}

shinyApp(ui = ui2, server = server2)


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
              value = c(1,180), min = 0, max = 180),
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