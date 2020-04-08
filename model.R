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

for (i in 1:7) {
  dt[paste0('hCasos', i)] <- hCasos[,i]
  dt[paste0('hFallecidos', i)] <- hFallecidos[,i]
  dt[paste0('hHospitalizados', i)] <- hHospitalizados[,i]
  dt[paste0('hRecuperados', i)] <- hRecuperados[,i]
  dt[paste0('hUCI', i)] <- hUCI[,i]
}

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

var_shortlist <- c("hCasos1", "hCasos2", "hCasos3", "hCasos4",
                   "hCasos5", "hCasos6", "hCasos7",
                   "hFallecidos1", "hFallecidos2", "hFallecidos3", "hFallecidos4",
                   "hFallecidos5", "hFallecidos6", "hFallecidos7",
                   "hHospitalizados1", "hHospitalizados2", "hHospitalizados3", "hHospitalizados4",
                   "hHospitalizados5", "hHospitalizados6", "hHospitalizados7",
                   "hRecuperados1", "hRecuperados2", "hRecuperados3", "hRecuperados4",
                   "hRecuperados5", "hRecuperados6", "hRecuperados7",
                   "hUCI1", "hUCI2", "hUCI3", "hUCI4",
                   "hUCI5", "hUCI6", "hUCI7")

# Model 1:
modelMatrixFormula <- formula(dt[,c("Casos", var_shortlist)])
mdlCasos <- glm(data = dt, formula = modelMatrixFormula)

modelMatrixFormula <- formula(dt[,c("Fallecidos", var_shortlist)])
mdlFallecidos <- glm(data = dt, formula = modelMatrixFormula)

modelMatrixFormula <- formula(dt[,c("Hospitalizados", var_shortlist)])
mdlHospitalizados <- glm(data = dt, formula = modelMatrixFormula)

modelMatrixFormula <- formula(dt[,c("Recuperados", var_shortlist)])
mdlRecuperados <- glm(data = dt, formula = modelMatrixFormula)

modelMatrixFormula <- formula(dt[,c("UCI", var_shortlist)])
mdlUCI <- glm(data = dt, formula = modelMatrixFormula)

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


# There's clear overfitting, let's try elastic net
library(glmnet)
TuneEnet <- function(tAlpha){
  
  enTune <- cv.glmnet( x = mm, y = dt$Casos,
                       nfolds = 4,
                       alpha = tAlpha)
  
  return(enTune)
}

mm <- model.matrix(modelMatrixFormula, data=dt)
# Let's try some hiperparameter search and cross-validation all at once
#Ranges to search
rangeAlpha <- c(0,1)
bestScore <-  Inf

for (ii in 1:10){
  tAlpha <- runif(n=1, min=rangeAlpha[1], max=rangeAlpha[2])
  
  print(paste0("Testing Alpha: ",tAlpha))
  
  enet <- TuneEnet(tAlpha)
  currScore <- min(deviance(enet$glmnet.fit))
  
  if( currScore < bestScore){
    cat(paste0("Alpha updated, now: ", tAlpha, "\n\n"))
    bestScore <- currScore
    bestEnet <- enet
  }
  
  rm(enet, tAlpha, currScore)
  gc()
}

y <- predict(bestEnet, mm)
#Plot model for cases
ggplot(data=dt, aes(x=Fecha, y=Casos)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = y, col = 'Modelo'), size=1, colour="red") +
  #scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Fecha", y = "Casos")