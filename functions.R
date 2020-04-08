FixData <- function(dt){
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
  return(dt)
}



TuneEnet <- function(tAlpha, dt, mm){
  
  enTune <- cv.glmnet( x = mm, y = dt$Casos,
                       nfolds = 3,
                       alpha = tAlpha)
  
  return(enTune)
}