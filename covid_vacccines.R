library(tidyverse)
library(scales)
library(mosaic)
library(lubridate)

#Change default colours
theme_set(theme_minimal())

data <- read.csv("~/Work/covid/owid-covid-data.csv", sep=",", header = TRUE)

#Fix dates
data$date <- as.POSIXct(strptime(data$date, "%Y-%m-%d"))
data$total_cases <- as.numeric(data$total_cases)

countries <- c("Israel", "United Kingdom", "United States", "Sweden", "Italy", "Spain",
               "France", "Germany", "Belgium", "Portugal", "Brazil")

get_date <- function(country){

  dd <- data[data$location == country,]
  
#  dd %>%
#    filter(date > "2021-01-01") %>%
#    ggplot(aes(x=date)) +
#    geom_line(aes(y = people_vaccinated_per_hundred, colour = "1 dosis"), size = 1) + 
#    geom_line(aes(y = people_fully_vaccinated_per_hundred, colour = "2 dosis"), size = 1) +
#    labs(x = "Fecha", y = "Vacunados", colour="Dosis")
  
  x <- dd %>%
    filter(date > "2021-01-01")
  x <- x[, c(4,42)]
  
  ddays <- integer(length(x[,1]))
  for (i in 1:length(x[,1]) ) {
#    ddays[i] <- as.integer(x[i,1]-as.POSIXct("2021-01-01"))
    ddays[i] <- i
  }
  x["Days"] <- ddays
    
  if (country == "United States" | country == "Germany"  | country == "Sweden") {
    # Este modelo funciona en USA
    f <- fitModel(people_fully_vaccinated_per_hundred ~ A + B*Days^C, data = x,
                  start=list(A=-0.4, B=0.003, C=2))
  }else if(country == "United Kingdom") {
    # Este modelo funciona en UK
    f <- fitModel(people_fully_vaccinated_per_hundred ~ A + B*Days^C, data = x,
                  start=list(A=0.7, B=1.5e-11, C=6))
  }else if(country == "Spain" || country == "Portugal" || country == "Italy"){
    # Modelo para España
    f <- fitModel(people_fully_vaccinated_per_hundred ~ (A*Days+B)^E+C*sin( (Days-D)/7 ), data = x, 
                  start=list(A=0.065, B=-1, C=0.35, D=30, E =1))
  }else if(country == "Israel"){
    f <- fitModel(people_fully_vaccinated_per_hundred ~ A + B*Days^C, data = x,
                  start=list(A=-10, B=2, C=0.8))
  }else{
    f <- fitModel(people_fully_vaccinated_per_hundred ~ (A*Days+B)+C*sin( (Days-D)/7 ), data = x, 
                  start=list(A=0.065, B=-1, C=0.35, D=30))
  }

  p <- plotPoints(people_fully_vaccinated_per_hundred ~ Days, data = x, 
             xlab="Días desde 1 enero",
             ylab="Personas totalmente vacunadas (%)",
             main=country)

  #Predecimos fecha para el 70% de vacunación:
  tv <- 0
  i <- 0
  while (tv < 70) {
    i <- i+1
    tv <- f(i)
    if (is.na(tv)) {
      tv <- 0
    }
  }
  
  theDay <- as.Date("2021-01-01")+i
  
  print(paste("El 70% se obtiene en", country,"el", theDay))
  return(list(p, f, theDay))
}

dTime <- matrix(nrow = length(countries), ncol = 3)

for (country in countries) {
  plotAndFun <- get_date(country)
  
  FirstNAindex <- min( which(is.na(dTime[,1])) )
  
  dTime[FirstNAindex, 1] <- country
  dTime[FirstNAindex, 2] <- as.character(plotAndFun[[3]])
  dTime[FirstNAindex, 3] <- length(countries) *1./FirstNAindex
  
  plot(plotAndFun[[1]])
  plot(plotFun(plotAndFun[[2]], col = "red", add = TRUE))
}

# Timeline
dTime <- as.data.frame(dTime)
names(dTime) <- c("Country", "Date", "Position")

dTime <- dTime[with(dTime, order(Date)), ]
dTime$Date <- ymd(dTime$Date)
dTime$Position <- as.numeric(dTime$Position)

month_buffer <- 60

month_date_range <- seq.Date(min(dTime$Date) - month_buffer, 
                        max(dTime$Date) + month_buffer, by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

year_date_range <- seq(min(dTime$Date) - month_buffer, 
                       max(dTime$Date) + month_buffer, by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)

# Timeline plot
timeline_plot<-ggplot(dTime, aes(x=Date,y=0))
timeline_plot<-timeline_plot+geom_segment(data=dTime, aes(y=Position -0.3, 
                                                          yend=0, xend=Date, col = Country), 
                                          size=0.2, show.legend = FALSE)
timeline_plot<-timeline_plot+geom_text(data=dTime, aes(y=Position,
                                                       label=Country, col = Country), 
                                       size=3, show.legend = FALSE)

# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(data=dTime, aes(x = Date, y=0, col = Country),
                                        size=2, show.legend = FALSE)

# Don't show axes, appropriately position legend
timeline_plot<-timeline_plot+theme_classic()
timeline_plot<-timeline_plot+theme(axis.line.y=element_blank(),
                                   axis.text.y=element_blank(),
                                   axis.title.x=element_blank(),
                                   axis.title.y=element_blank(),
                                   axis.ticks.y=element_blank(),
                                   axis.text.x =element_blank(),
                                   axis.ticks.x =element_blank(),
                                   axis.line.x =element_blank(),
                                   legend.position = "bottom"
)

# Plot horizontal black line for timeline
timeline_plot<-timeline_plot+geom_hline(yintercept=0, 
                                        color = "black", size=0.3)

# Show text for each month
timeline_plot<-timeline_plot+geom_text(data=month_df, 
                                       aes(x=month_date_range,y=-0.5,label=month_format),
                                       size=2.5,vjust=0.5, color='black', angle=90)
# Show year text
timeline_plot<-timeline_plot+geom_text(data=year_df,
                                       aes(x=year_date_range,y=-1.5,label=year_format,
                                           fontface="bold"),size=2.5, color='black')

LastDay <- max(data$date)

timeline_plot <- timeline_plot + annotate("text", x=as.Date("2024-01-01"), y=8, label= "Forecast: date of 70% population fully vaccinated")
timeline_plot <- timeline_plot + annotate("text", x=as.Date("2024-01-01"), y=7.5, label= paste("Predicted using data up to",LastDay ) )
print(timeline_plot)
