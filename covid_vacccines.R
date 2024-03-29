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
               "Belgium", "Germany", "India", "Sweden", "Canada")

populations <- c(9347117, 66796807, 331695937, 47351567, 126014024,
                 10295909, 59226539, 213154869, 67406000, 125410000,
                 11560220, 83190556, 1377123716, 10389806, 38341866)

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
  while(range(x2$people_fully_vaccinated_per_hundred)[2] - 
        range(x2$people_fully_vaccinated_per_hundred)[1] < 1.0) {
    x2 <- x %>%
      filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
      slice_tail(n = nmindays)
      nmindays = nmindays +1
  }
  
  ll <- lm(formula = people_fully_vaccinated_per_hundred ~ Days + 1, data = x2)
  f2 <- makeFun(ll)
  
  slope <- ll$coefficients[[2]]
  Dslope <- summary(ll)$coefficients[2,2]
  error <- round(3*(80-max(x2$people_fully_vaccinated_per_hundred)) * Dslope / (slope * slope))
  if(error < 1){error <- 1}

  if(country == "Israel"){
    f <- fitModel(people_fully_vaccinated_per_hundred ~ A*log(B*(Days+C)), data = x, 
                  start=list(A=20, B=0.12, C=1.2))
  }else if(country == "Mexico"){
    f <- fitModel(people_fully_vaccinated_per_hundred ~ A + B*Days^C, data = x,
                  start=list(A=-0.3, B=2.7e-5, C=2.6))
  }else{
    f <- fitModel(people_fully_vaccinated_per_hundred ~ A + B*Days^C, data = x, nls.control(maxiter = 100),
                  start=list(A=-1, B=0.03, C=2))
  }
  
  p <- plotPoints(people_fully_vaccinated_per_hundred ~ Days, data = x, 
             xlab="Días desde 1 enero 2021",
             ylab="Población totalmente vacunada (%)",
             main=country)

  #Predecimos fecha para el 70% de vacunación:
  tv <- 0
  i <- 0
  while (tv < 80) {
    i <- i+1
    tv <- f(i)
    if (is.na(tv)) {
      tv <- 0
    }
  }
  
  #Comparamos a ajuste lineal de los últimos 7 puntos
  tv <- 0
  i2 <- 0
  while (tv < 80) {
    i2 <- i2+1
    tv <- f2(i2)
    if (is.na(tv)) {
      tv <- 0
    }
  }
  
  theDay  <- as.Date("2021-01-01") + min(i, i2)
  theDay2 <- as.Date("2021-01-01") + max(i, i2)
  
  theMax <- max(x$people_fully_vaccinated_per_hundred, na.rm = TRUE)
  
  print(paste("El 80% se obtiene en", country,"el", theDay2, "+/-", error))
  
  return(list(p, f, theDay, theMax, f2, theDay2, error))
}

dTime <- matrix(nrow = length(countries), ncol = 6)

for (country in countries) {
  plotAndFun <- get_date(country)
  
  FirstNAindex <- min( which(is.na(dTime[,1])) )
  
  minDate <- data %>% filter(location == country) %>% summarise(LastDay = max(date))
  minDate <- as.Date(minDate[1,1])
  minDate <- max(minDate, plotAndFun[[3]]) + 1
  
  dTime[FirstNAindex, 1] <- country
  dTime[FirstNAindex, 2] <- as.character(minDate)
  dTime[FirstNAindex, 3] <- 12 * log10(1 + populations[FirstNAindex]*10./max(populations) )
  dTime[FirstNAindex, 4] <- plotAndFun[[4]]
  dTime[FirstNAindex, 5] <- as.character(plotAndFun[[6]])
  dTime[FirstNAindex, 6] <- plotAndFun[[7]]
  
  plot(plotAndFun[[1]])
  plot(plotFun(plotAndFun[[2]], col = "red", add = TRUE))
  plot(plotFun(plotAndFun[[5]], col = "darkgreen", add = TRUE))
}

# Timeline
dTime <- as.data.frame(dTime)
names(dTime) <- c("Country", "Date", "Position", "Total", "DateMax", "Error")

dTime <- dTime[with(dTime, order(Date)), ]
dTime$Date <- ymd(dTime$Date)
dTime$DateMax <- ymd(dTime$DateMax)
dTime$Position <- as.numeric(dTime$Position)
dTime$Total <- as.numeric(dTime$Total)
dTime$Error <- as.numeric(dTime$Error)

#Fix Portugal & Spain
dTime$Date[dTime$Country == "Portugal"] <- "2021-09-12"
dTime$DateMax[dTime$Country == "Portugal"] <- "2021-09-12"
dTime$Error[dTime$Country == "Portugal"] <- 0
dTime$Date[dTime$Country == "Spain"] <- "2021-11-07"
dTime$DateMax[dTime$Country == "Spain"] <- "2021-11-07"
dTime$Error[dTime$Country == "Spain"] <- 0

# Bar plot
dTime %>%
  arrange(desc(Total)) %>%
  ggplot(aes(x = reorder(Country, Total), weight = Total)) +
  geom_bar(aes(fill = Total)) +
  geom_text(aes(x = reorder(Country, Total), y = as.numeric(Total) +3,
                            label = paste0(round(Total, digits = 1), "%"),
                            col = "black"), 
            size=3, show.legend = FALSE) +
  scale_fill_gradient2( high = "darkred") +
  guides(fill = FALSE) +
  coord_flip() +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(x = "Country", y = "People fully vaccinated (%)")

#Timeline(s)
month_buffer <- 10

month_date_range <- seq.Date(min(dTime$DateMax) - month_buffer, 
                        max(dTime$DateMax) + month_buffer, by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

year_date_range <- seq(min(dTime$DateMax) - month_buffer, 
                       max(dTime$DateMax) + month_buffer, by='year')

if(max(dTime$DateMax) >= "2023-01-01"){
  year_date_range <- seq(min(dTime$DateMax) - month_buffer, 
                         max(dTime$DateMax) + 365, by='year')
}

year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)

# Timeline plot
timeline_plot <- ggplot(dTime, aes(x=DateMax,y=0)) +
              geom_segment(data=dTime, aes(y=Position -0.3, 
                            yend=0, xend=DateMax, col = Country), 
                            size=0.2, show.legend = FALSE) +
              geom_text(data=dTime, aes(y=Position+0.5,
                            label = paste0(Country, "\n(", round(Total), "%)"),
                            col = Country), 
                            size=3, show.legend = FALSE)

# Plot scatter points at zero and date
timeline_plot<-timeline_plot+geom_point(data=dTime, aes(x = DateMax, y=0, col = Country),
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
if(length(month_date_range) > 12){
  timeline_plot<-timeline_plot+geom_text(data=year_df,
                                         aes(x=year_date_range,y=-1.5,label=year_format,
                                             fontface="bold"), size=2.5, color='black')
}

LastDay <- max(data$date)

timeline_plot <- timeline_plot + annotate("text", x=as.Date("2022-05-15"), y=10.75, label= "Forecast: date of 80% population fully vaccinated")
timeline_plot <- timeline_plot + annotate("text", x=as.Date("2022-05-15"), y=10, label= paste("Predicted using data up to",LastDay ) )
timeline_plot <- timeline_plot + annotate("text", x=as.Date("2022-05-15"), y=9.25, label= "Percentage shows current number" )
print(timeline_plot)


# Print the last data entry for Spain
print("Last data entry for Spain:")
data %>%
  filter(location == "Spain") %>%
  filter(!is.na(people_fully_vaccinated_per_hundred)) %>%
  summarise(LastDay = max(date), Total_vacc = max(people_fully_vaccinated_per_hundred))

# X-wing chart
dTime %>%
  ggplot(aes(col = Country, 
             y=rank(DateMax), 
             x=as.Date(DateMax) ))  +
  geom_point(show.legend = FALSE) +
  geom_errorbarh(aes(xmin = as.Date(DateMax)-Error, xmax = as.Date(DateMax)+Error), show.legend = FALSE) +
  geom_text(aes(x = as.Date(DateMax)+Error+50+nchar(Country), label=paste0(Country)), show.legend = FALSE ) +
  labs(x = "Date", y = "Country)") +
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank()) +
  geom_vline(xintercept = as.Date(LastDay), col = "red", size = 1)



