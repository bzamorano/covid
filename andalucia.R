library(tidyverse)
library(scales)

#Change default colours
theme_set(theme_minimal())
ggplot <- function(...) ggplot2::ggplot(...) + scale_color_brewer(palette="Set1")

data <- read.csv2("~/Work/covid/cs_export.txt", sep=";", header = TRUE)

#Remove spurious last column
data <- data[,1:4]
#Empty data
data <- data[!is.na(data$Fecha),]
#Fix dates
data[[1]] <- as.POSIXct(strptime(data[[1]], "%d/%m/%Y"))

fechas      <- data$Fecha[data$Medida=="Total confirmados "]
provincia   <- data$Territorio[data$Medida=="Total confirmados "]
confirmados <- data$Valor[data$Medida=="Total confirmados "]
uci         <- data$Valor[data$Medida=="Total UCI"]
fallecidos  <- data$Valor[data$Medida=="Fallecidos"]

nrows <- length(fechas)

dd <- data[1:nrows,0]
dd['Fecha']       <- fechas
dd['Provincia']   <- provincia
dd['Confirmados'] <- confirmados
dd['UCI']         <- uci
dd['Fallecidos']  <- fallecidos
rm(fechas, provincia, confirmados, uci, fallecidos)

provincias <- unique(dd$Provincia)

weeknew        <- integer(nrows)
fortnightnew   <- integer(nrows)
weekUCI        <- integer(nrows)
fortnightUCI   <- integer(nrows)
weekdeath      <- integer(nrows)
fortnightdeath <- integer(nrows)

for (prov in provincias) {
  irows <- (1:nrows)[dd$Provincia == prov]
  dt <- dd[irows,]
  
  n_rows <- nrow(dt)
  
  for (i in 1:n_rows ) {
    min_index <- min(i+7, n_rows)
    min_index2 <- min(i+14, n_rows)
    
    weeknew[irows[i]] <- dt$Confirmados[i] - dt$Confirmados[min_index]
    fortnightnew[irows[i]] <- dt$Confirmados[i] - dt$Confirmados[min_index2]
    weekUCI[irows[i]] <- dt$UCI[i] - dt$UCI[min_index]
    fortnightUCI[irows[i]] <- dt$UCI[i] - dt$UCI[min_index2]
    weekdeath[irows[i]] <- dt$Fallecidos[i] - dt$Fallecidos[min_index]
    fortnightdeath[irows[i]] <- dt$Fallecidos[i] - dt$Fallecidos[min_index2]
  }
}

rm(prov, irows, n_rows, i, min_index, min_index2, dt)

dd['Confirmados_7d']  <- weeknew
dd['Confirmados_14d'] <- fortnightnew
dd['UCI_7d']          <- weekUCI
dd['UCI_14d']         <- fortnightUCI
dd['Fallecidos_7d']   <- weekdeath
dd['Fallecidos_14d']  <- fortnightdeath

rm(weeknew, fortnightnew, weekUCI, fortnightUCI, weekdeath, fortnightdeath)

TotCases <- max(data$Valor[data$Medida == "Total confirmados "], na.rm = TRUE)
TotCasesGRX <- max(data$Valor[data$Medida == "Total confirmados "
                              & data$Territorio == "Granada"], na.rm = TRUE)
TotUCI <- max(data$Valor[data$Medida == "Total UCI"], na.rm = TRUE)
TotUCIGRX <- max(data$Valor[data$Medida == "Total UCI"
                            & data$Territorio == "Granada"], na.rm = TRUE)
TotDeaths <- max(data$Valor[data$Medida == "Fallecidos"], na.rm = TRUE)
TotDeathsGRX <- max(data$Valor[data$Medida == "Fallecidos"
                               & data$Territorio == "Granada"], na.rm = TRUE)
LastDay <- max(data$Fecha, na.rm = TRUE)

# Casos totales
data%>%
  filter(Medida == "Total confirmados " ) %>%
  ggplot(aes(x=Fecha, y=Valor, colour=Territorio)) +
  geom_line(size = 1.25) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Fecha", y = "Casos totales") +
  annotate("text", x=LastDay-86400*45, y=300, label= LastDay) +
  annotate("text", x=LastDay-86400*45, y=150, label= paste("Casos totales:", TotCases)) +
  annotate("text", x=LastDay-86400*45, y=75, label= paste("Granada:", TotCasesGRX))

# Fallecidos totales
data%>%
  filter(Medida == "Fallecidos" ) %>%
  ggplot(aes(x=Fecha, y=Valor, colour=Territorio)) +
  geom_line(size = 1.25) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Fecha", y = "Fallecidos totales") +
  annotate("text", x=LastDay-86400*45, y=27, label= LastDay) +
  annotate("text", x=LastDay-86400*45, y=18, label= paste("Fallecidos totales:", TotDeaths)) +
  annotate("text", x=LastDay-86400*45, y=12, label= paste("Granada:", TotDeathsGRX))

# Confirmados 7 dias
dd%>%
  filter(Provincia != "Andalucía") %>%
  ggplot(aes(x=Fecha, y=Confirmados_7d, colour=Provincia)) +
  geom_line(size = 1.25) +
  labs(x = "Fecha", y = "Confirmados - 7 días")

# Confirmados 14 dias
dd%>%
  filter(Provincia != "Andalucía") %>%
  ggplot(aes(x=Fecha, y=Confirmados_14d, colour=Provincia)) +
  geom_line(size = 1.25) +
  labs(x = "Fecha", y = "Confirmados - 14 días")

# UCI 14 dias
dd%>%
  filter(Provincia != "Andalucía") %>%
  ggplot(aes(x=Fecha, y=UCI_14d, colour=Provincia)) +
  geom_line(size = 1.25) +
  labs(x = "Fecha", y = "UCI - 14 días")

# Fallecidos 7 dias
dd%>%
  filter(Provincia != "Andalucía") %>%
  ggplot(aes(x=Fecha, y=Fallecidos_7d, colour=Provincia)) +
  geom_line(size = 1.25) +
  labs(x = "Fecha", y = "Fallecidos - 7 días")

# Fallecidos 14 dias
dd%>%
  filter(Provincia != "Andalucía") %>%
  ggplot(aes(x=Fecha, y=Fallecidos_14d, colour=Provincia)) +
  geom_line(size = 1.25) +
  labs(x = "Fecha", y = "Fallecidos - 14 días")

WeekCasesGRX  <- first(dd$Confirmados_7d[dd$Provincia == "Granada"])
WeekUCIGRX  <- first(dd$UCI_7d[dd$Provincia == "Granada"])
WeekDeathsGRX <- first(dd$Fallecidos_7d[dd$Provincia == "Granada"])
FortnightCasesGRX  <- first(dd$Confirmados_14d[dd$Provincia == "Granada"])
FortnightUCIGRX <- first(dd$UCI_14d[dd$Provincia == "Granada"])
FortnightDeathsGRX <- first(dd$Fallecidos_14d[dd$Provincia == "Granada"])

#Summary for GRX
dd%>%
  filter(Provincia == "Granada") %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = Confirmados_7d*(Confirmados_7d > 0)), colour = "blue", size = 1) + 
  geom_line(aes(y = 50*Fallecidos_7d*(Fallecidos_7d > 0)), colour = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 50, name = "Fallecidos 7 días")) +
  theme( axis.line.y.right = element_line(color = "red"), 
         axis.ticks.y.right = element_line(color = "red"),
         axis.text.y.right = element_text(color = "red"),
         axis.title.y.right = element_text(color = "red")) +
  theme( axis.line.y.left = element_line(color = "blue"), 
         axis.ticks.y.left = element_line(color = "blue"),
         axis.text.y.left = element_text(color = "blue"),
         axis.title.y.left = element_text(color = "blue")) +
  geom_vline(xintercept = as.POSIXct("2020-12-24"), col = "green") +
  annotate("text", x=as.POSIXct("2020-04-15"), y=7300,
           label = paste("Granada", LastDay)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=6800, 
           label= paste("Casos 7 días:", WeekCasesGRX), color = "blue") +
  annotate("text", x=as.POSIXct("2020-04-15"), y=6300,
           label= paste("Total casos:", TotCasesGRX), color = "blue") +
  annotate("text", x=as.POSIXct("2020-04-15"), y=5600, 
           label= paste("Fallecidos 7 días:", WeekDeathsGRX), color = "red") +
  annotate("text", x=as.POSIXct("2020-04-15"), y=5100,
           label= paste("Total fallecidos:", TotDeathsGRX), color = "red") +
  labs(x = "Fecha", y = "Confirmados 7 días")

# Y a 14 días
dd%>%
  filter(Provincia == "Granada") %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = Confirmados_14d*(Confirmados_14d > 0)), colour = "blue", size = 1) + 
  geom_line(aes(y = 50*Fallecidos_14d*(Fallecidos_14d > 0)), colour = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 50, name = "Fallecidos 14 días")) +
  theme( axis.line.y.right = element_line(color = "red"), 
         axis.ticks.y.right = element_line(color = "red"),
         axis.text.y.right = element_text(color = "red"),
         axis.title.y.right = element_text(color = "red")) +
  theme( axis.line.y.left = element_line(color = "blue"), 
         axis.ticks.y.left = element_line(color = "blue"),
         axis.text.y.left = element_text(color = "blue"),
         axis.title.y.left = element_text(color = "blue")) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=13500,
           label = paste("Granada", LastDay)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=12000, 
           label= paste("Casos 14 días:", FortnightCasesGRX)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=11000, 
           label= paste("Fallecidos 14 días:", FortnightDeathsGRX)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=9500,
           label= paste("Total casos:", TotCasesGRX)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=8500,
           label= paste("Total fallecidos:", TotDeathsGRX)) +
  labs(x = "Fecha", y = "Confirmados 14 días")

# Casos y UCI
dd%>%
  filter(Provincia == "Granada") %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = Confirmados_14d*(Confirmados_14d > 0)), colour = "blue", size = 1) + 
  geom_line(aes(y = 100*UCI_14d*(UCI_14d > 0)), colour = "darkgreen", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 100, name = "UCI 14 días")) +
  theme( axis.line.y.right = element_line(color = "darkgreen"), 
         axis.ticks.y.right = element_line(color = "darkgreen"),
         axis.text.y.right = element_text(color = "darkgreen"),
         axis.title.y.right = element_text(color = "darkgreen")) +
  theme( axis.line.y.left = element_line(color = "blue"), 
         axis.ticks.y.left = element_line(color = "blue"),
         axis.text.y.left = element_text(color = "blue"),
         axis.title.y.left = element_text(color = "blue")) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=13500,
           label = paste("Granada", LastDay)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=12000, 
           label= paste("Casos 14 días:", FortnightCasesGRX)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=11000, 
           label= paste("UCI 14 días:", FortnightUCIGRX)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=9500,
           label= paste("Total casos:", TotCasesGRX)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=8500,
           label= paste("Total UCI:", TotUCIGRX)) +
  labs(x = "Fecha", y = "Confirmados 14 días")

# A 14 días UCI y fallecidos
dd%>%
  filter(Provincia == "Granada") %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = UCI_14d*(UCI_14d > 0)), colour = "darkgreen", size = 1) + 
  geom_line(aes(y = Fallecidos_14d*(UCI_14d > 0)), colour = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 1, name = "Fallecidos 14 días")) +
  theme( axis.line.y.right = element_line(color = "red"), 
         axis.ticks.y.right = element_line(color = "red"),
         axis.text.y.right = element_text(color = "red"),
         axis.title.y.right = element_text(color = "red")) +
  theme( axis.line.y.left = element_line(color = "darkgreen"), 
         axis.ticks.y.left = element_line(color = "darkgreen"),
         axis.text.y.left = element_text(color = "darkgreen"),
         axis.title.y.left = element_text(color = "darkgreen")) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=220,
           label = paste("Granada", LastDay)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=200, 
           label= paste("UCI 14 días:", FortnightUCIGRX)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=190, 
           label= paste("Fallecidos 14 días:", FortnightDeathsGRX)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=170,
           label= paste("Total UCI:", TotUCIGRX)) +
  annotate("text", x=as.POSIXct("2020-04-15"), y=160,
           label= paste("Total fallecidos:", TotDeathsGRX)) +
  labs(x = "Fecha", y = "UCI 14 días")