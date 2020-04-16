FixData <- function(dt){
  nrows <- nrow(dt)
  
  dCasos          <- integer(nrows)
  wCasos          <- integer(nrows)
  hCasos <- matrix(data = 0, nrow = nrows, ncol = 10)
  dHospitalizados <- integer(nrows)
  wHospitalizados <- integer(nrows)
  hHospitalizados <- matrix(data = 0, nrow = nrows, ncol = 10)
  dUCI            <- integer(nrows)
  wUCI            <- integer(nrows)
  hUCI <- matrix(data = 0, nrow = nrows, ncol = 10)
  dFallecidos     <- integer(nrows)
  wFallecidos     <- integer(nrows)
  hFallecidos <- matrix(data = 0, nrow = nrows, ncol = 10)
  dRecuperados    <- integer(nrows)
  wRecuperados    <- integer(nrows)
  hRecuperados <- matrix(data = 0, nrow = nrows, ncol = 10)
  
  for (i in 2:nrows) {
    dCasos[i]          <- dt$Casos[i]-dt$Casos[i-1]
    dHospitalizados[i] <- dt$Hospitalizados[i]-dt$Hospitalizados[i-1]
    dUCI[i]            <- dt$UCI[i]-dt$UCI[i-1]
    dFallecidos[i]     <- dt$Fallecidos[i]-dt$Fallecidos[i-1]
    dRecuperados[i]    <- dt$Recuperados[i]-dt$Recuperados[i-1]
    
    max_index <- max(2, i-10)
    wCasos[i] <- sum(dCasos[max_index:i])
    wHospitalizados[i] <- sum(dHospitalizados[max_index:i])
    wUCI[i] <- sum(dUCI[max_index:i])
    wFallecidos[i] <- sum(dFallecidos[max_index:i])
    wRecuperados[i] <- sum(dRecuperados[max_index:i])
    for (k in 1:(i-1)) {
      # print(paste("i", i, "k", k, "i-k:", i-k))
      if(i-k < 11){
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
  
  for (i in 1:10) {
    dt[paste0('hCasos', i)] <- hCasos[,i]
    dt[paste0('hFallecidos', i)] <- hFallecidos[,i]
    dt[paste0('hHospitalizados', i)] <- hHospitalizados[,i]
    dt[paste0('hRecuperados', i)] <- hRecuperados[,i]
    dt[paste0('hUCI', i)] <- hUCI[,i]
  }
  
  rm(dCasos, dHospitalizados, dUCI, dFallecidos, dRecuperados)
  rm(wCasos, wHospitalizados, wUCI, wFallecidos, wRecuperados)
  rm(hCasos, hFallecidos, hHospitalizados, hRecuperados, hUCI)
  gc()
  return(dt)
}

TuneEnet <- function(tAlpha, dt, mm){
  
  enTune <- cv.glmnet( x = mm, y = dt$Casos,
                       nfolds = 3,
                       alpha = tAlpha)
  
  return(enTune)
}

TuneGBM <- function(LearnRate, NumTrees, Verbose, FittingParams){
  library(parallel)
  numCores <- detectCores()
  numCores <- ifelse(is.na(numCores),1,numCores-1) 
  
  tDepth <- FittingParams[1]
  tChild <- FittingParams[2]
  tGamma <- FittingParams[3]
  tSub <- FittingParams[4]
  tCol <- FittingParams[5]
  tAlpha <- FittingParams[6]
  tLambda <- FittingParams[7]
  
  gbPar <- list("objective" = "reg:squarederror",
                "booster" = "gbtree",
                "silent" = 0,
                "eta"=LearnRate,
                "max_depth" = tDepth,
                "min_child_weight" = tChild,
                "gamma" = tGamma,
                "subsample" = tSub,
                "colsample_bytree" = tCol,
                "alpha" = tAlpha,
                "lambda" = tLambda,
                "scale_pos_weight"=1,
                "nthread" = numCores
  )
  
  xgbTune <- xgb.cv(params =  gbPar,
                    data = dmm_tr,
                    nrounds = NumTrees,
                    prediction = F,
                    print_every_n = 5,
                    maximize = F,
                    verbose=Verbose,
                    nfold=3,
                    metrics="rmse",
                    stratified = FALSE,
                    early_stopping_rounds = 100)
  return(xgbTune)
}

FitGBM <- function(LearnRate, NumTrees, Verbose, FittingParams){
  library(parallel)
  numCores <- detectCores()
  numCores <- ifelse(is.na(numCores),1,numCores-1) 
  
  tDepth <- FittingParams[1]
  tChild <- FittingParams[2]
  tGamma <- FittingParams[3]
  tSub <- FittingParams[4]
  tCol <- FittingParams[5]
  tAlpha <- FittingParams[6]
  tLambda <- FittingParams[7]
  
  gbPar <- list("objective" = "reg:squarederror",
                "booster" = "gbtree",
                "silent" = 0,
                "eta"=LearnRate,
                "max_depth" = tDepth,
                "min_child_weight" = tChild,
                "gamma" = tGamma,
                "subsample" = tSub,
                "colsample_bytree" = tCol,
                "alpha" = tAlpha,
                "lambda" = tLambda,
                "scale_pos_weight"=1,
                "nthread" = numCores
  )
  
  xgbMDL <- xgb.train(params =  gbPar,
                      data = dmm_tr,
                      nrounds = NumTrees,
                      prediction = F,
                      print_every_n = 5,
                      maximize = F,
                      verbose=Verbose)
  return(xgbMDL)
}