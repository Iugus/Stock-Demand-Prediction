setwd("C:/Cursos/Razure-1/projeto2/Stock-Demand-Prediction")
library(tidyverse)
library(data.table)
df_comp <- read_csv("train.csv")

head(df_comp)

factors <- c("Semana", "Agencia_ID", "Canal_ID", "Ruta_SAK",
             "Cliente_ID", "Producto_ID" , "NombreProducto")


for(i in 1:length(factors)){
  df_comp[,factors[i]] <- lapply(df_comp[,factors[i]], as.factor)
}

str(df_comp)
