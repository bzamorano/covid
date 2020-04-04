library(utils)
library(httr)
library(ggplot2)
library(tidyr)
library(scales)

GET("https://rubenfcasal.github.io/COVID-19/acumulados.RData", 
    authenticate(":", ":", type="ntlm"),
    write_disk(tf <- "./acumulados.RData", overwrite = TRUE))

load("./acumulados.RData")

# sample Madrid
dt <- acumulados[acumulados$CCAA.ISO =="MD",c(3:8)]

nrows <- nrow(dt)

dCasos          <- integer(nrows)
wCasos          <- integer(nrows)
dHospitalizados <- integer(nrows)
wHospitalizados <- integer(nrows)
dUCI            <- integer(nrows)
wUCI            <- integer(nrows)
dFallecidos     <- integer(nrows)
wFallecidos     <- integer(nrows)
dRecuperados    <- integer(nrows)
wRecuperados    <- integer(nrows)

for (i in 2:nrows) {
  dCasos[i]          <- dt$Casos[i]-dt$Casos[i-1]
  dHospitalizados[i] <- dt$Hospitalizados[i]-dt$Hospitalizados[i-1]
  dUCI[i]            <- dt$UCI[i]-dt$UCI[i-1]
  dFallecidos[i]     <- dt$Fallecidos[i]-dt$Fallecidos[i-1]
  dRecuperados[i]    <- dt$Recuperados[i]-dt$Recuperados[i-1]
  
  max_index <- max(2, i-7)
  wCasos[i] <- sum(dCasos[max_index:i])
  wHospitalizados[i] <- sum(dHospitalizados[max_index:i])
  wUCI[i] <- sum(dUCI[max_index:i])
  wFallecidos[i] <- sum(dFallecidos[max_index:i])
  wRecuperados[i] <- sum(dRecuperados[max_index:i])
}

dt['dCasos'] <- dCasos
dt['wCasos'] <- wCasos
dt['dHospitalizados'] <- dHospitalizados
dt['wHospitalizados'] <- wHospitalizados
dt['dUCI'] <- dUCI
dt['wUCI'] <- wUCI
dt['dFallecidos'] <- dFallecidos
dt['wFallecidos'] <- wFallecidos
dt['dRecuperados'] <- dRecuperados
dt['wRecuperados'] <- wRecuperados

rm(dCasos, dHospitalizados, dUCI, dFallecidos, dRecuperados)
rm(wCasos, wHospitalizados, wUCI, wFallecidos, wRecuperados)

dt[dt$Hospitalizados > 0, ]%>%
  ggplot(aes(x=Fecha, y=Hospitalizados / Casos)) +
  geom_line(size=1, colour="red")  +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  labs(x = "Fecha", y = "Hospitalizados / Casos")

dt[(dt$Casos > 100), ]%>%
  ggplot(aes(x=Casos, y=wCasos)) +
  geom_line(size=1, colour="red") +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total cases", y = "Weekly new cases")

y <- glm(formula = dt$dFallecidos~dt$Casos+dt$Hospitalizados+dt$UCI)
