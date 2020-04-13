library(utils)
library(tidyverse)
library(httr)
library(ggplot2)
library(tidyr)
library(scales)

source("./functions.R")

GET("https://rubenfcasal.github.io/COVID-19/acumulados.RData", 
    authenticate(":", ":", type="ntlm"),
    write_disk(tf <- "./acumulados.RData", overwrite = TRUE))
load(tf)

dt_tot <- acumulados  %>%
  group_by(Fecha) %>%
  summarise(Casos = sum(Casos),
            Fallecidos = sum(Fallecidos),
            Hospitalizados = sum(Hospitalizados),
            Recuperados = sum(Recuperados),
            UCI = sum(UCI)
  )


# Madrid: training sample. Catalonia: test sample
dt_tr <- acumulados[acumulados$CCAA.ISO =="MD",c(3:8)]
#dt_tr <- dt_tot
dt_te <- acumulados[acumulados$CCAA.ISO =="CT",c(3:8)]

dt_tr <- FixData(dt_tr)
dt_te <- FixData(dt_te)

dt_tr[(dt_tr$Casos > 100), ]%>%
  ggplot(aes(x=Casos, y=wCasos)) +
  geom_line(size=1, colour="red") +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total cases", y = "Weekly new cases")

# Model 0:
mdl0 <- glm(formula = dt_tr$Casos ~ dt_tr$hCasos1+dt_tr$hCasos2+dt_tr$hCasos3+
              dt_tr$hCasos4+dt_tr$hCasos5+dt_tr$hCasos6+dt_tr$hCasos7)

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
modelMatrixFormula <- formula(dt_tr[,c("Casos", var_shortlist)])
mdlCasos <- glm(data = dt_tr, formula = modelMatrixFormula)

modelMatrixFormula <- formula(dt_tr[,c("Fallecidos", var_shortlist)])
mdlFallecidos <- glm(data = dt_tr, formula = modelMatrixFormula)

modelMatrixFormula <- formula(dt_tr[,c("Hospitalizados", var_shortlist)])
mdlHospitalizados <- glm(data = dt_tr, formula = modelMatrixFormula)

modelMatrixFormula <- formula(dt_tr[,c("Recuperados", var_shortlist)])
mdlRecuperados <- glm(data = dt_tr, formula = modelMatrixFormula)

modelMatrixFormula <- formula(dt_tr[,c("UCI", var_shortlist)])
mdlUCI <- glm(data = dt_tr, formula = modelMatrixFormula)

#Plot model for cases (model 0)
ggplot(data=dt_te, aes(x=Fecha, y=Casos)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = predict(mdl0, dt_te), col = 'Modelo'), size=1, colour="red") +
  #scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Fecha", y = "Casos")

#Plot model for cases (model 1)
ggplot(data=dt_te, aes(x=Fecha, y=Casos)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = predict(mdlCasos, dt_te), col = 'Modelo'), size=1, colour="red") +
  labs(x = "Fecha", y = "Casos")

# There's clear overfitting, let's try elastic net
library(glmnet)

modelMatrixFormula <- formula(dt_tr[,c("Casos", var_shortlist)])

#modelMatrixFormula <- update(modelMatrixFormula, ~ . + hCasos1 * hFallecidos1 
#                             + hCasos1 * hUCI1
#                             + hCasos1 * hHospitalizados1
#                             + hCasos1 * hRecuperados1
#                             + hCasos1 * hCasos2
#                             + hUCI1 * hHospitalizados1)# add interactions


mm_tr <- model.matrix(modelMatrixFormula, data=dt_tr)
mm_te <- model.matrix(modelMatrixFormula, data=dt_te)

# Let's try some hiperparameter search and cross-validation all at once
#Ranges to search
rangeAlpha <- c(0,1)
bestScore <-  Inf

for (ii in 1:10){
  tAlpha <- runif(n=1, min=rangeAlpha[1], max=rangeAlpha[2])
  
  print(paste0("Testing Alpha: ",tAlpha))
  
  enet <- TuneEnet(tAlpha, dt_tr, mm_tr)
  currScore <- min(deviance(enet$glmnet.fit))
  
  if( currScore < bestScore){
    cat(paste0("Alpha updated, now: ", tAlpha, "\n\n"))
    bestScore <- currScore
    bestEnet <- enet
  }
  
  rm(enet, tAlpha, currScore)
  gc()
}

#Plot model for cases (Elastic Net)
ggplot(data=dt_te, aes(x=Fecha, y=Casos)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = predict(bestEnet, mm_te), col = 'Modelo'), size=1, colour="red") +
  labs(x = "Fecha", y = "Casos")

# Just as a test, let's see a random forest
library(randomForest)
modelMatrixFormula <- formula(dt_tr[,c("Casos", var_shortlist)])

rf <- randomForest(formula = modelMatrixFormula,
                   type = regression,
                   data = dt_tr,
                   importance = TRUE,
                   ntree = 300) # this is too small, but sufficient for demonstration purposes

#Plot model for cases (Random forest)
ggplot(data=dt_te, aes(x=Fecha, y=Casos)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = predict(rf, dt_te), col = 'Modelo'), size=1, colour="red") +
  labs(x = "Fecha", y = "Casos")
