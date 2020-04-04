library(utils)
library(httr)
library(ggplot2)
library(tidyr)

GET("https://rubenfcasal.github.io/COVID-19/acumulados.RData", 
    authenticate(":", ":", type="ntlm"),
    write_disk(tf <- "./acumulados.RData", overwrite = TRUE))

load("./acumulados.RData")