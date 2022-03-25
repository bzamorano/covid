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
hospital    <- data$Valor[data$Medida=="Hospitalizados"]
fallecidos  <- data$Valor[data$Medida=="Fallecidos"]

nrows <- length(fechas)

dd <- data[1:nrows,0]
dd['Fecha']       <- fechas
dd['Provincia']   <- provincia
dd['Confirmados'] <- confirmados
dd['UCI']         <- uci
dd['Fallecidos']  <- fallecidos
dd['Hospitalizados'] <- hospital 
rm(fechas, provincia, confirmados, uci, fallecidos, hospital)

provincias <- unique(dd$Provincia)

weeknew        <- integer(nrows)
fortnightnew   <- integer(nrows)
weekUCI        <- integer(nrows)
fortnightUCI   <- integer(nrows)
weekdeath      <- integer(nrows)
fortnightdeath <- integer(nrows)
weekhosp       <- integer(nrows)
fortnighthosp  <- integer(nrows)

for (prov in provincias) {
  irows <- (1:nrows)[dd$Provincia == prov]
  dt <- dd[irows,]
  
  n_rows <- nrow(dt)
  
  for (i in 1:n_rows ) {
    min_index <- min(i+7, n_rows)
    min_index2 <- min(i+14, n_rows)
    
    # correct for missing days
    diff_days <- as.integer(dt$Fecha[i] - dt$Fecha[min_index2])
    while (diff_days > 14) {
      min_index2 <- min_index2 -1
      diff_days <- as.integer(dt$Fecha[i] - dt$Fecha[min_index2])
    }
    diff_days <- as.integer(dt$Fecha[i] - dt$Fecha[min_index])
    while (diff_days > 7) {
      min_index <- min_index -1
      diff_days <- as.integer(dt$Fecha[i] - dt$Fecha[min_index])
    }
    
    weeknew[irows[i]] <- dt$Confirmados[i] - dt$Confirmados[min_index]
    fortnightnew[irows[i]] <- dt$Confirmados[i] - dt$Confirmados[min_index2]
    weekUCI[irows[i]] <- dt$UCI[i] - dt$UCI[min_index]
    fortnightUCI[irows[i]] <- dt$UCI[i] - dt$UCI[min_index2]
    weekdeath[irows[i]] <- dt$Fallecidos[i] - dt$Fallecidos[min_index]
    fortnightdeath[irows[i]] <- dt$Fallecidos[i] - dt$Fallecidos[min_index2]
    weekhosp[irows[i]] <- dt$Hospitalizados[i] - dt$Hospitalizados[min_index]
    fortnighthosp[irows[i]] <- dt$Hospitalizados[i] - dt$Hospitalizados[min_index2]
  }
}

rm(prov, irows, n_rows, i, min_index, min_index2, dt)

dd['Confirmados_7d']  <- weeknew
dd['Confirmados_14d'] <- fortnightnew
dd['UCI_7d']          <- weekUCI
dd['UCI_14d']         <- fortnightUCI
dd['Fallecidos_7d']   <- weekdeath
dd['Fallecidos_14d']  <- fortnightdeath
dd['Hospital_7d']     <- weekhosp
dd['Hospital_14d']    <- fortnighthosp

# Población (INE)
pop <- read.csv2("~/Work/covid/pop_andalucia.csv", sep=";", header = TRUE)
dd<-merge(x=dd, y=pop, by.x = "Provincia",  by.y = "Provincia", all.x=TRUE)

rm(weeknew, fortnightnew, weekUCI, fortnightUCI, 
   weekdeath, fortnightdeath, weekhosp, fortnighthosp)

TotCases <- max(data$Valor[data$Medida == "Total confirmados "], na.rm = TRUE)
TotCasesGRX <- max(data$Valor[data$Medida == "Total confirmados "
                              & data$Territorio == "Granada"], na.rm = TRUE)
TotUCI <- max(data$Valor[data$Medida == "Total UCI"], na.rm = TRUE)
TotUCIGRX <- max(data$Valor[data$Medida == "Total UCI"
                            & data$Territorio == "Granada"], na.rm = TRUE)
TotDeaths <- max(data$Valor[data$Medida == "Fallecidos"], na.rm = TRUE)
TotDeathsGRX <- max(data$Valor[data$Medida == "Fallecidos"
                               & data$Territorio == "Granada"], na.rm = TRUE)
TotHosp <- max(data$Valor[data$Medida == "Hospitalizados"], na.rm = TRUE)
TotHospGRX <- max(data$Valor[data$Medida == "Hospitalizados"
                               & data$Territorio == "Granada"], na.rm = TRUE)
LastDay <- max(data$Fecha, na.rm = TRUE)

WeekCasesGRX  <- first(dd$Confirmados_7d[dd$Provincia == "Granada" & dd$Fecha == LastDay])
WeekUCIGRX  <- first(dd$UCI_7d[dd$Provincia == "Granada" & dd$Fecha == LastDay])
WeekDeathsGRX <- first(dd$Fallecidos_7d[dd$Provincia == "Granada" & dd$Fecha == LastDay])
WeekHospGRX <- first(dd$Hospital_7d[dd$Provincia == "Granada" & dd$Fecha == LastDay])
FortnightCasesGRX  <- first(dd$Confirmados_14d[dd$Provincia == "Granada" & dd$Fecha == LastDay])
FortnightUCIGRX <- first(dd$UCI_14d[dd$Provincia == "Granada" & dd$Fecha == LastDay])
FortnightDeathsGRX <- first(dd$Fallecidos_14d[dd$Provincia == "Granada" & dd$Fecha == LastDay])
FortnightHospGRX <- first(dd$Hospital_14d[dd$Provincia == "Granada" & dd$Fecha == LastDay])
IncTot <- first(dd$Confirmados_14d[dd$Provincia == "Andalucía" & dd$Fecha == LastDay]) *1.e5 /
          pop$Poblacion[pop$Provincia == "Andalucía"]
IncGRX <- first(dd$Confirmados_14d[dd$Provincia == "Granada" & dd$Fecha == LastDay]) *1.e5 /
  pop$Poblacion[pop$Provincia == "Granada"]

# Casos totales
data%>%
  filter(Territorio != "Andalucía" & Medida == "Total confirmados " ) %>%
  ggplot(aes(x=Fecha, y=Valor, colour=Territorio)) +
  geom_line(size = 1.25) +
#  scale_y_log10(labels = comma_format(big.mark = " ")) +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(x = "Fecha", y = "Casos totales", col = "Provincia") +
  annotate("text", x=LastDay-86400*175, y=30000, label= LastDay) +
  annotate("text", x=LastDay-86400*175, y=19000, label= paste("Casos totales:", TotCases)) +
  annotate("text", x=LastDay-86400*175, y=7500, label= paste("Granada:", TotCasesGRX))

# Fallecidos totales
data%>%
  filter(Territorio != "Andalucía" & Medida == "Fallecidos" ) %>%
  ggplot(aes(x=Fecha, y=Valor, colour=Territorio)) +
  geom_line(size = 1.25) +
#  scale_y_log10(labels = comma_format(big.mark = " ")) +
  scale_y_continuous(labels = comma_format(big.mark = " ")) +
  labs(x = "Fecha", y = "Fallecidos totales", col = "Provincia") +
  annotate("text", x=LastDay-86400*175, y=300, label= LastDay) +
  annotate("text", x=LastDay-86400*175, y=180, label= paste("Fallecidos totales:", TotDeaths)) +
  annotate("text", x=LastDay-86400*175, y=50, label= paste("Granada:", TotDeathsGRX))

# Hospitalizados totales
data%>%
  filter(Territorio != "Andalucía" & Medida == "Hospitalizados" ) %>%
  ggplot(aes(x=Fecha, y=Valor, colour=Territorio)) +
  geom_line(size = 1.25) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Fecha", y = "Hospitalizados totales", col = "Provincia") +
  annotate("text", x=LastDay-86400*90, y=50, label= LastDay) +
  annotate("text", x=LastDay-86400*90, y=30, label= paste("Hospitalizados totales:", TotHosp)) +
  annotate("text", x=LastDay-86400*90, y=18, label= paste("Granada:", TotHospGRX))

# Confirmados 7 dias
dd%>%
  filter(Provincia != "Andalucía" & Fecha > "2020-10-01") %>%
  ggplot(aes(x=Fecha, y=Confirmados_7d, colour=Provincia)) +
  geom_line(size = 1.25) +
  labs(x = "Fecha", y = "Confirmados - 7 días")

# Confirmados 14 dias
dd%>%
  filter(Provincia != "Andalucía" & Fecha > "2020-10-01") %>%
  ggplot(aes(x=Fecha, y=Confirmados_14d, colour=Provincia)) +
  geom_line(size = 1.25) +
  labs(x = "Fecha", y = "Confirmados - 14 días")

# UCI 14 dias
dd%>%
  filter(Provincia != "Andalucía" & Fecha > "2020-10-01") %>%
  ggplot(aes(x=Fecha, y=UCI_14d, colour=Provincia)) +
  geom_line(size = 1.25) +
  labs(x = "Fecha", y = "UCI - 14 días")

# Fallecidos 14 dias
dd%>%
  filter(Provincia != "Andalucía" & Fecha > "2020-10-01") %>%
  ggplot(aes(x=Fecha, y=Fallecidos_14d, colour=Provincia)) +
  geom_line(size = 1.25) +
  labs(x = "Fecha", y = "Fallecidos - 14 días")

# Hospitalizados 14 dias
dd%>%
  filter(Provincia != "Andalucía" & Fecha > "2020-10-01") %>%
  ggplot(aes(x=Fecha, y=Hospital_14d, colour=Provincia)) +
  geom_line(size = 1.25) +
  labs(x = "Fecha", y = "Hospitalizados - 14 días")

# Incidencia acumulada
dd%>%
  filter(Provincia != "Andalucía" & Fecha > "2020-10-01") %>%
  ggplot(aes(x=Fecha, y=Confirmados_14d * 1e5/Poblacion, colour=Provincia)) +
  geom_line(size = 1.25) +
  labs(x = "Fecha", y = "Incidencia acumulada") +
  geom_vline(xintercept = as.POSIXct("2021-05-09"), col = "green", size = 1.25) +
  annotate("text", x=as.POSIXct("2021-01-15"), y=2000, label= LastDay) +
  annotate("text", x=as.POSIXct("2021-01-15"), y=1850, label= paste("Andalucía:", round(IncTot))) +
  annotate("text", x=as.POSIXct("2021-01-15"), y=1700, label= paste("Granada:", round(IncGRX)))

#Summary for GRX
dd%>%
  filter(Provincia == "Granada" & Fecha > "2020-10-01") %>%
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
  geom_vline(xintercept = as.POSIXct("2021-05-09"), col = "green", size = 1.25) +
  annotate("text", x=as.POSIXct("2021-01-25"), y=8500,
           label = paste("Granada", LastDay)) +
  annotate("text", x=as.POSIXct("2021-01-25"), y=8000, 
           label= paste("Casos 7 días:", WeekCasesGRX), color = "blue") +
  annotate("text", x=as.POSIXct("2021-01-25"), y=7500,
           label= paste("Total casos:", TotCasesGRX), color = "blue") +
  annotate("text", x=as.POSIXct("2021-01-25"), y=6800, 
           label= paste("Fallecidos 7 días:", WeekDeathsGRX), color = "red") +
  annotate("text", x=as.POSIXct("2021-01-25"), y=6300,
           label= paste("Total fallecidos:", TotDeathsGRX), color = "red") +
  labs(x = "Fecha", y = "Confirmados 7 días")

# Y a 14 días
dd%>%
  filter(Provincia == "Granada" & Fecha > "2020-10-01") %>%
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
  geom_vline(xintercept = as.POSIXct("2021-05-09"), col = "green", size = 1.25) +
  annotate("text", x=as.POSIXct("2021-01-25"), y=18500,
           label = paste("Granada", LastDay)) +
  annotate("text", x=as.POSIXct("2021-01-25"), y=17500, 
           label= paste("Casos 14 días:", FortnightCasesGRX), color = "blue") +
  annotate("text", x=as.POSIXct("2021-01-25"), y=16500,
           label= paste("Total casos:", TotCasesGRX), color = "blue") +
  annotate("text", x=as.POSIXct("2021-01-25"), y=15000, 
           label= paste("Fallecidos 14 días:", FortnightDeathsGRX), color = "red") +
  annotate("text", x=as.POSIXct("2021-01-25"), y=14000,
           label= paste("Total fallecidos:", TotDeathsGRX), color = "red") +
  labs(x = "Fecha", y = "Confirmados 14 días")

# Hospitalizados vs UCI
dd%>%
  filter(Provincia == "Granada") %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = Hospital_14d*(Hospital_14d > 0)), colour = "darkgreen", size = 1) + 
  geom_line(aes(y = 4*UCI_14d*(UCI_14d > 0)), colour = "purple", size = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~ . / 4, name = "UCI 14 días")) +
  theme( axis.line.y.right = element_line(color = "purple"), 
         axis.ticks.y.right = element_line(color = "purple"),
         axis.text.y.right = element_text(color = "purple"),
         axis.title.y.right = element_text(color = "purple")) +
  theme( axis.line.y.left = element_line(color = "darkgreen"), 
         axis.ticks.y.left = element_line(color = "darkgreen"),
         axis.text.y.left = element_text(color = "darkgreen"),
         axis.title.y.left = element_text(color = "darkgreen")) +
  geom_vline(xintercept = as.POSIXct("2021-05-09"), col = "green", size = 1.25) +
  annotate("text", x=as.POSIXct("2020-06-15"), y=850,
           label = paste("Granada", LastDay)) +
  annotate("text", x=as.POSIXct("2020-06-15"), y=800, 
           label= paste("Hospitalizados 14 días:", FortnightHospGRX), color = "darkgreen") +
  annotate("text", x=as.POSIXct("2020-06-15"), y=760,
           label= paste("Total hospital:", TotHospGRX), color = "darkgreen") +
  annotate("text", x=as.POSIXct("2020-06-01"), y=720, 
           label= paste("UCI 14 días:", FortnightUCIGRX), color = "purple") +
  annotate("text", x=as.POSIXct("2020-06-01"), y=680,
           label= paste("Total UCI:", TotUCIGRX), color = "purple") +
  labs(x = "Fecha", y = "Hospitalizados 14 días")

# A 14 días UCI y fallecidos
dd%>%
  filter(Provincia == "Granada") %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = UCI_14d*(UCI_14d > 0)), colour = "purple", size = 1) + 
  geom_line(aes(y = Fallecidos_14d*(UCI_14d > 0)), colour = "red", size = 1) +
  scale_y_continuous(sec.axis = sec_axis(~ . / 1, name = "Fallecidos 14 días")) +
  theme( axis.line.y.right = element_line(color = "red"), 
         axis.ticks.y.right = element_line(color = "red"),
         axis.text.y.right = element_text(color = "red"),
         axis.title.y.right = element_text(color = "red")) +
  theme( axis.line.y.left = element_line(color = "purple"), 
         axis.ticks.y.left = element_line(color = "purple"),
         axis.text.y.left = element_text(color = "purple"),
         axis.title.y.left = element_text(color = "purple")) +
  geom_vline(xintercept = as.POSIXct("2021-05-09"), col = "green", size = 1.25) +
  annotate("text", x=as.POSIXct("2020-06-15"), y=220,
           label = paste("Granada", LastDay)) +
  annotate("text", x=as.POSIXct("2020-06-01"), y=210, 
           label= paste("UCI 14 días:", FortnightUCIGRX), color = "purple") +
  annotate("text", x=as.POSIXct("2020-06-01"), y=200,
           label= paste("Total UCI:", TotUCIGRX), color = "purple") +
  annotate("text", x=as.POSIXct("2020-06-15"), y=190, 
           label= paste("Fallecidos 14 días:", FortnightDeathsGRX), color = "red") +
  annotate("text", x=as.POSIXct("2020-06-15"), y=180,
           label= paste("Total fallecidos:", TotDeathsGRX), color = "red") +
  labs(x = "Fecha", y = "UCI 14 días")

# Evolucion mortalidad
dd%>%
  filter(Provincia == "Granada" & Fecha > "2020-04-01") %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = (Fallecidos/Confirmados)), colour = "brown", size = 1) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = as.POSIXct("2021-05-09"), col = "green", size = 1.25) +
  geom_hline(yintercept = 0.012, col = "darkblue", size = 1.25) +
  labs(x = "Fecha", y = "Fallecidos / Confirmados")

# Evolucion mortalidad
dd%>%
  filter(Provincia == "Granada" & Fecha > "2020-04-01") %>%
  ggplot(aes(x = Fecha)) +
  geom_line(aes(y = (Fallecidos/Hospitalizados)), colour = "brown", size = 1) + 
  scale_y_continuous(labels = scales::percent) +
  geom_vline(xintercept = as.POSIXct("2021-05-09"), col = "green", size = 1.25) +
  labs(x = "Fecha", y = "Fallecidos / Hospitalizados")
