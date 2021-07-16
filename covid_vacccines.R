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

countries <- c("Israel", "United Kingdom", "United States", "Spain", "Mexico",
               "Portugal", "Italy", "Brazil", "France", "Japan",
               "Belgium", "Germany", "India", "Sweden")

populations <- c(9347117, 66796807, 331695937, 47351567, 126014024,
                 10295909, 59226539, 213154869, 67406000, 125410000,
                 11560220, 83190556, 1377123716, 10389806)

get_date <- function(country){

  dd <- data[data$location == country,]
  
  x <- dd %>%
    filter(date > "2021-01-01")
  x <- x[, c(4,42)]
  
  ddays <- integer(length(x[,1]))
  for (i in 1:length(x[,1]) ) {
    ddays[i] <- i
  }
  x["Days"] <- ddays
    
  if(country == "Italy"
           | country == "France"
           | country == "Germany" | country == "Portugal")
    {
    # En realidad es el modelo más genérico
    f <- fitModel(people_fully_vaccinated_per_hundred ~ A + B*Days^C, data = x,
                  start=list(A=0.3, B=8.3e-8, C=4))
  }else if(country == "Spain"){
    # Modelo para España
    f <- fitModel(people_fully_vaccinated_per_hundred ~ (A*Days+B)^E+C*sin( (Days-D)/7 ), data = x, 
                  start=list(A=0.002, B=1, C=0.5, D=33, E =12))
  }else if(country == "Israel"){
    f <- fitModel(people_fully_vaccinated_per_hundred ~ A*log(B*(Days+C)), data = x, 
                  start=list(A=25, B=0.074, C=5.3))
  }else if(country == "Japan"){
    f <- fitModel(people_fully_vaccinated_per_hundred ~ A + B*Days^C, data = x,
                  start=list(A=-0.03, B=3.0e-15, C=7))
  }else if(country == "Belgium" ){
    f <- fitModel(people_fully_vaccinated_per_hundred ~ A + B*Days^C, data = x,
                  start=list(A=-0.1, B=2.7e-8, C=4))
  }else if(country == "India" | country == "Sweden" 
           | country == "Mexico"){
    f <- fitModel(people_fully_vaccinated_per_hundred ~ A + B*Days^C, data = x,
                  start=list(A=-0.3, B=2.7e-5, C=2.6))
  }else{
    f <- fitModel(people_fully_vaccinated_per_hundred ~ A + B*Days^C, data = x,
                  start=list(A=-0.4, B=0.003, C=2))
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
  
  theMax <- max(x$people_fully_vaccinated_per_hundred, na.rm = TRUE)
  
  print(paste("El 70% se obtiene en", country,"el", theDay))
  
  return(list(p, f, theDay, theMax))
}

dTime <- matrix(nrow = length(countries), ncol = 4)

for (country in countries) {
  plotAndFun <- get_date(country)
  
  FirstNAindex <- min( which(is.na(dTime[,1])) )
  
  dTime[FirstNAindex, 1] <- country
  dTime[FirstNAindex, 2] <- as.character(plotAndFun[[3]])
  dTime[FirstNAindex, 3] <- 12 * log10(1 + populations[FirstNAindex]*10./max(populations) )
  dTime[FirstNAindex, 4] <- round(plotAndFun[[4]])
  
  plot(plotAndFun[[1]])
  plot(plotFun(plotAndFun[[2]], col = "red", add = TRUE))
}

# Timeline
dTime <- as.data.frame(dTime)
names(dTime) <- c("Country", "Date", "Position", "Total")

dTime <- dTime[with(dTime, order(Date)), ]
dTime$Date <- ymd(dTime$Date)
dTime$Position <- as.numeric(dTime$Position)

# Dropping India of the chart until it gets closer
dTime <- dTime %>% filter(Country != "Mexico")
dTime <- dTime %>% filter(Country != "India")
dTime <- dTime %>% filter(Country != "Brazil")

month_buffer <- 30

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
timeline_plot <- ggplot(dTime, aes(x=Date,y=0)) +
              geom_segment(data=dTime, aes(y=Position -0.3, 
                            yend=0, xend=Date, col = Country), 
                            size=0.2, show.legend = FALSE) +
              geom_text(data=dTime, aes(y=Position+0.5,
                            label = paste0(Country, "\n(", Total, "%)"),
                            col = Country), 
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
if(length(month_date_range) >= 12){
  timeline_plot<-timeline_plot+geom_text(data=year_df,
                                         aes(x=year_date_range,y=-1.5,label=year_format,
                                             fontface="bold"), size=2.5, color='black')
}

LastDay <- max(data$date)

timeline_plot <- timeline_plot + annotate("text", x=as.Date("2021-08-15"), y=10, label= "Forecast: date of 70% population fully vaccinated")
timeline_plot <- timeline_plot + annotate("text", x=as.Date("2021-08-15"), y=9.25, label= paste("Predicted using data up to",LastDay ) )
timeline_plot <- timeline_plot + annotate("text", x=as.Date("2021-08-15"), y=8.5, label= "Percentage shows current number" )
print(timeline_plot)

# Print the last data entry for Spain
print("Last data entry for Spain:")
data %>%
  filter(location == "Spain") %>%
  filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
  summarise(LastDay = max(date), Total_vacc = max(people_fully_vaccinated_per_hundred))
