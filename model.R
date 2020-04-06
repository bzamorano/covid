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
hCasos <- matrix(data = 0, nrow = nrows, ncol = 7)
dHospitalizados <- integer(nrows)
wHospitalizados <- integer(nrows)
hHospitalizados <- matrix(data = 0, nrow = nrows, ncol = 7)
dUCI            <- integer(nrows)
wUCI            <- integer(nrows)
hUCI <- matrix(data = 0, nrow = nrows, ncol = 7)
dFallecidos     <- integer(nrows)
wFallecidos     <- integer(nrows)
hFallecidos <- matrix(data = 0, nrow = nrows, ncol = 7)
dRecuperados    <- integer(nrows)
wRecuperados    <- integer(nrows)
hRecuperados <- matrix(data = 0, nrow = nrows, ncol = 7)

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
  for (k in 1:(i-1)) {
    # print(paste("i", i, "k", k, "i-k:", i-k))
    if(i-k < 8){
      hCasos[i, i-k] <- dt$Casos[k]
      hHospitalizados[i, i-k] <- dt$Hospitalizados[k]
      hUCI[i, i-k] <- dt$UCI[k]
      hFallecidos[i, i-k] <- dt$Fallecidos[k]
      hRecuperados[i, i-k] <- dt$Recuperados[k]
    }
  }
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

# Make this smarter
dt['hCasos1'] <- hCasos[,1]
dt['hCasos2'] <- hCasos[,2]
dt['hCasos3'] <- hCasos[,3]
dt['hCasos4'] <- hCasos[,4]
dt['hCasos5'] <- hCasos[,5]
dt['hCasos6'] <- hCasos[,6]
dt['hCasos7'] <- hCasos[,7]

dt['hFallecidos1'] <- hFallecidos[,1]
dt['hFallecidos2'] <- hFallecidos[,2]
dt['hFallecidos3'] <- hFallecidos[,3]
dt['hFallecidos4'] <- hFallecidos[,4]
dt['hFallecidos5'] <- hFallecidos[,5]
dt['hFallecidos6'] <- hFallecidos[,6]
dt['hFallecidos7'] <- hFallecidos[,7]

dt['hHospitalizados1'] <- hHospitalizados[,1]
dt['hHospitalizados2'] <- hHospitalizados[,2]
dt['hHospitalizados3'] <- hHospitalizados[,3]
dt['hHospitalizados4'] <- hHospitalizados[,4]
dt['hHospitalizados5'] <- hHospitalizados[,5]
dt['hHospitalizados6'] <- hHospitalizados[,6]
dt['hHospitalizados7'] <- hHospitalizados[,7]

dt['hRecuperados1'] <- hRecuperados[,1]
dt['hRecuperados2'] <- hRecuperados[,2]
dt['hRecuperados3'] <- hRecuperados[,3]
dt['hRecuperados4'] <- hRecuperados[,4]
dt['hRecuperados5'] <- hRecuperados[,5]
dt['hRecuperados6'] <- hRecuperados[,6]
dt['hRecuperados7'] <- hRecuperados[,7]

dt['hUCI1'] <- hUCI[,1]
dt['hUCI2'] <- hUCI[,2]
dt['hUCI3'] <- hUCI[,3]
dt['hUCI4'] <- hUCI[,4]
dt['hUCI5'] <- hUCI[,5]
dt['hUCI6'] <- hUCI[,6]
dt['hUCI7'] <- hUCI[,7]


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

# Model 0:
mdl0 <- glm(formula = dt$Casos ~ dt$hCasos1+dt$hCasos2+dt$hCasos3+
              dt$hCasos4+dt$hCasos5+dt$hCasos6+dt$hCasos7)
# Model 1:
mdlCasos <- glm(formula = dt$Casos ~ dt$hCasos1+dt$hCasos2+dt$hCasos3+
              dt$hCasos4+dt$hCasos5+dt$hCasos6+dt$hCasos7+
              dt$hFallecidos1+dt$hFallecidos2+dt$hFallecidos3+
              dt$hFallecidos4+dt$hFallecidos5+dt$hFallecidos6+dt$hFallecidos7+
              dt$hHospitalizados1+dt$hHospitalizados2+dt$hHospitalizados3+
              dt$hHospitalizados4+dt$hHospitalizados5+dt$hHospitalizados6+dt$hHospitalizados7+
              dt$hRecuperados1+dt$hRecuperados2+dt$hRecuperados3+
              dt$hRecuperados4+dt$hRecuperados5+dt$hRecuperados6+dt$hRecuperados7+
              dt$hUCI1+dt$hUCI2+dt$hUCI3+
              dt$hUCI4+dt$hUCI5+dt$hUCI6+dt$hUCI7)

mdlFallecidos <- glm(formula = dt$Fallecidos ~ dt$hCasos1+dt$hCasos2+dt$hCasos3+
                  dt$hCasos4+dt$hCasos5+dt$hCasos6+dt$hCasos7+
                  dt$hFallecidos1+dt$hFallecidos2+dt$hFallecidos3+
                  dt$hFallecidos4+dt$hFallecidos5+dt$hFallecidos6+dt$hFallecidos7+
                  dt$hHospitalizados1+dt$hHospitalizados2+dt$hHospitalizados3+
                  dt$hHospitalizados4+dt$hHospitalizados5+dt$hHospitalizados6+dt$hHospitalizados7+
                  dt$hRecuperados1+dt$hRecuperados2+dt$hRecuperados3+
                  dt$hRecuperados4+dt$hRecuperados5+dt$hRecuperados6+dt$hRecuperados7+
                  dt$hUCI1+dt$hUCI2+dt$hUCI3+
                  dt$hUCI4+dt$hUCI5+dt$hUCI6+dt$hUCI7)

mdlHospitalizados <- glm(formula = dt$Hospitalizados ~ dt$hCasos1+dt$hCasos2+dt$hCasos3+
                       dt$hCasos4+dt$hCasos5+dt$hCasos6+dt$hCasos7+
                       dt$hFallecidos1+dt$hFallecidos2+dt$hFallecidos3+
                       dt$hFallecidos4+dt$hFallecidos5+dt$hFallecidos6+dt$hFallecidos7+
                       dt$hHospitalizados1+dt$hHospitalizados2+dt$hHospitalizados3+
                       dt$hHospitalizados4+dt$hHospitalizados5+dt$hHospitalizados6+dt$hHospitalizados7+
                       dt$hRecuperados1+dt$hRecuperados2+dt$hRecuperados3+
                       dt$hRecuperados4+dt$hRecuperados5+dt$hRecuperados6+dt$hRecuperados7+
                       dt$hUCI1+dt$hUCI2+dt$hUCI3+
                       dt$hUCI4+dt$hUCI5+dt$hUCI6+dt$hUCI7)

mdlRecuperados <- glm(formula = dt$Recuperados ~ dt$hCasos1+dt$hCasos2+dt$hCasos3+
                       dt$hCasos4+dt$hCasos5+dt$hCasos6+dt$hCasos7+
                       dt$hFallecidos1+dt$hFallecidos2+dt$hFallecidos3+
                       dt$hFallecidos4+dt$hFallecidos5+dt$hFallecidos6+dt$hFallecidos7+
                       dt$hHospitalizados1+dt$hHospitalizados2+dt$hHospitalizados3+
                       dt$hHospitalizados4+dt$hHospitalizados5+dt$hHospitalizados6+dt$hHospitalizados7+
                       dt$hRecuperados1+dt$hRecuperados2+dt$hRecuperados3+
                       dt$hRecuperados4+dt$hRecuperados5+dt$hRecuperados6+dt$hRecuperados7+
                       dt$hUCI1+dt$hUCI2+dt$hUCI3+
                       dt$hUCI4+dt$hUCI5+dt$hUCI6+dt$hUCI7)

mdlUCI <- glm(formula = dt$UCI ~ dt$hCasos1+dt$hCasos2+dt$hCasos3+
                       dt$hCasos4+dt$hCasos5+dt$hCasos6+dt$hCasos7+
                       dt$hFallecidos1+dt$hFallecidos2+dt$hFallecidos3+
                       dt$hFallecidos4+dt$hFallecidos5+dt$hFallecidos6+dt$hFallecidos7+
                       dt$hHospitalizados1+dt$hHospitalizados2+dt$hHospitalizados3+
                       dt$hHospitalizados4+dt$hHospitalizados5+dt$hHospitalizados6+dt$hHospitalizados7+
                       dt$hRecuperados1+dt$hRecuperados2+dt$hRecuperados3+
                       dt$hRecuperados4+dt$hRecuperados5+dt$hRecuperados6+dt$hRecuperados7+
                       dt$hUCI1+dt$hUCI2+dt$hUCI3+
                       dt$hUCI4+dt$hUCI5+dt$hUCI6+dt$hUCI7)

#Plot model for cases
ggplot(data=dt, aes(x=Fecha, y=Casos)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = mdlCasos$fitted.values, col = 'Modelo'), size=1, colour="red") +
  #scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Fecha", y = "Casos")

#Plot model for dead
ggplot(data=dt, aes(x=Fecha, y=Fallecidos)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = mdlFallecidos$fitted.values, col = 'Modelo'), size=1, colour="red") +
  #scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Fecha", y = "Fallecidos")

#Plot model for hospitalised
ggplot(data=dt, aes(x=Fecha, y=Hospitalizados)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = mdlHospitalizados$fitted.values, col = 'Modelo'), size=1, colour="red") +
  #scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Fecha", y = "Hospitalizados")

#Plot model for recovered
ggplot(data=dt, aes(x=Fecha, y=Recuperados)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = mdlRecuperados$fitted.values, col = 'Modelo'), size=1, colour="red") +
  #scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Fecha", y = "Recuperados")

#Plot model for intensive care
ggplot(data=dt, aes(x=Fecha, y=UCI)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = mdlUCI$fitted.values, col = 'Modelo'), size=1, colour="red") +
  #scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Fecha", y = "UCI")