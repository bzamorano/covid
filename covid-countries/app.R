#library(utils)
library(httr)
#library(ggplot2)
library(tidyverse)
#library(tidyr)
library(scales)
#library(shiny)

GET("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv", 
    authenticate(":", ":", type="ntlm"),
    write_disk(tf <- "covid_data.csv", overwrite = TRUE))

data <- read.csv2(tf, sep=",", header = TRUE)

dt <- (data %>%
         group_by(countriesAndTerritories) %>%
         summarise(Deaths = sum(deaths)/1e3,
                   Population = max(popData2019)/1e6,
                   Continent = first(continentExp)) %>%
         arrange(desc(Deaths)) )

rm(data)

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