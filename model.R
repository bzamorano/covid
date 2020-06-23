library(utils)
library(tidyverse)
library(httr)
library(ggplot2)
library(tidyr)
library(scales)

setwd("~/Work/covid")
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


# Madrid: training sample. Andalusia: test sample
dt_tr <- acumulados[acumulados$CCAA.ISO =="MD",c(3:8)]
#dt_tr <- dt_tot
dt_te <- acumulados[acumulados$CCAA.ISO =="AN",c(3:8)]

dt_tr <- FixData(dt_tr)
dt_te <- FixData(dt_te)
dt_tot <- FixData(dt_tot)

dt_tot[(dt_tot$Casos > 100), ]%>%
  ggplot(aes(x=Casos, y=wCasos)) +
  geom_line(size=1, colour="red") +
  scale_x_log10(labels = comma_format(big.mark = " ")) +
  scale_y_log10(labels = comma_format(big.mark = " ")) +
  labs(x = "Total cases", y = "Weekly new cases")

# Model 0:
mdl0 <- glm(formula = dt_tr$Casos ~ dt_tr$hCasos1+dt_tr$hCasos2+dt_tr$hCasos3+
              dt_tr$hCasos4+dt_tr$hCasos5+dt_tr$hCasos6+dt_tr$hCasos7)

var_shortlist <- c("weekDay", "hCasos1", "hCasos2", "hCasos3", "hCasos4", "hCasos5",
                   "hCasos6", "hCasos7", "hCasos8", "hCasos9", "hCasos10",
                   "hFallecidos1", "hFallecidos2", "hFallecidos3", 
                   "hFallecidos4", "hFallecidos5", "hFallecidos6",
                   "hFallecidos7", "hFallecidos8", "hFallecidos9", "hFallecidos10",
                   "hHospitalizados1", "hHospitalizados2", "hHospitalizados3",
                   "hHospitalizados4", "hHospitalizados5", "hHospitalizados6",
                   "hHospitalizados7", "hHospitalizados8", "hHospitalizados9",
                   "hHospitalizados10",
                   "hRecuperados1", "hRecuperados2", "hRecuperados3", "hRecuperados4",
                   "hRecuperados5", "hRecuperados6", "hRecuperados7", "hRecuperados8",
                   "hRecuperados9", "hRecuperados10",
                   "hUCI1", "hUCI2", "hUCI3", "hUCI4", "hUCI5", "hUCI6", "hUCI7",
                   "hUCI8", "hUCI9", "hUCI10")

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
  labs(x = "Fecha", y = "Casos", title = "Minimal GLM (Test sample)")

#Plot model for cases (model 1)
ggplot(data=dt_te, aes(x=Fecha, y=Casos)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = predict(mdlCasos, dt_te), col = 'Modelo'), size=1, colour="red") +
  labs(x = "Fecha", y = "Casos", title = "GLM (Test sample)")

# There's clear overfitting, let's try elastic net
library(glmnet)

modelMatrixFormula <- formula(dt_tr[,c("Casos", var_shortlist)])

mm_tr <- model.matrix(modelMatrixFormula, data=dt_tr)
mm_te <- model.matrix(modelMatrixFormula, data=dt_te)

# Let's try some hiperparameter search and cross-validation all at once
#Ranges to search
rangeAlpha <- c(0,1)
bestScore <-  Inf
eNetIterations <- 100

for (ii in 1:eNetIterations){
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
  labs(x = "Fecha", y = "Casos", title = "Elastic net (Test sample)")

# Just as a test, let's see a random forest
library(randomForest)
modelMatrixFormula <- formula(dt_tr[,c("Casos", var_shortlist)])

rf <- randomForest(formula = modelMatrixFormula,
                   type = regression,
                   data = dt_tr,
                   importance = TRUE,
                   ntree = 500) # A bit on the small side

#Plot model for cases (Random forest)
ggplot(data=dt_te, aes(x=Fecha, y=Casos)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = predict(rf, dt_te), col = 'Modelo'), size=1, colour="red") +
  labs(x = "Fecha", y = "Casos", title = "Random forest (Test sample)")

library(xgboost)

modelMatrixFormula <- formula(dt_tr[,c("Casos", var_shortlist)])
mm_tr <- model.matrix(modelMatrixFormula, data=dt_tr)
mm_te <- model.matrix(modelMatrixFormula, data=dt_te)

dmm_tr <- xgb.DMatrix(mm_tr, label = dt_tr$Casos)
dmm_te <- xgb.DMatrix(mm_te, label = dt_te$Casos)

# number of iterations to test during the optimisation
gbmIterations <- 100
gbmLearnRate <- 0.05

####Tune GBM Hyperparameters####
#Ranges to search
rangeDepth <- c(2,10)
rangeChild <- c(1,6)
rangeGamma <- c(0,0.5)
rangeSub <- c(0.6,1)
rangeCol <- c(0.6,1)
rangeAlpha <- c(-4,2)
rangeLambda <- c(0,0.1)

bestParams <- rep(0,7)
bestScore <-  Inf
optTrees <- 0

#Random search
for (ii in 1:gbmIterations){
  tDepth <- sample(rangeDepth[1]:rangeDepth[2],size=1)
  tChild <- sample(rangeChild[1]:rangeChild[2],size=1)
  tGamma <- runif(n=1,min=rangeGamma[1],max=rangeGamma[2])
  tSub <- runif(n=1,min=rangeSub[1],max=rangeSub[2])
  tCol <- runif(n=1,min=rangeCol[1],max=rangeCol[2])
  tAlpha <- 10^sample(rangeAlpha[1]:rangeAlpha[2],size=1)
  tLambda <- runif(n=1,min=rangeLambda[1],max=rangeLambda[2])
  currParams <- c(tDepth,tChild,tGamma,tSub,tCol,tAlpha,tLambda)
  print(paste0("Testing Parameters: ",paste0(round(currParams,2),collapse=",")))
  
  mdl <- TuneGBM(gbmLearnRate, 10000, FALSE, currParams) 
  gc()
  eval_log <- as.data.frame(mdl$evaluation_log)
  currScore <- min(eval_log[,"test_rmse_mean"])
  print(paste0("Score: ",currScore))
  
  if(currScore<bestScore){
    cat(paste0("Best parameters updated, now: ",paste0(round(currParams,2),collapse=","),"\n\n"))
    bestScore <- currScore
    bestParams <- currParams
    optTrees <- mdl$best_ntreelimit
  }
}

####Fit Final GBM####
mdl_GBM <- FitGBM(gbmLearnRate, optTrees, FALSE, bestParams)

#Plot model for cases (GBM)
ggplot(data=dt_te, aes(x=Fecha, y=Casos)) +
  geom_point(size=2, colour="blue") +
  geom_line(aes(y = predict(mdl_GBM, dmm_te), col = 'Modelo'), size=1, colour="red") +
  labs(x = "Fecha", y = "Casos", title = "GBM (Test sample)")
