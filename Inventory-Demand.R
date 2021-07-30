setwd("~/Documents/Stock-Demand-Prediction")
library(tidyverse)
library(data.table)
df_comp <- fread("train.csv")
head(df_comp)

factors <- c("Agencia_ID", "Canal_ID", "Ruta_SAK",
             "Cliente_ID", "Producto_ID")



df_comp[,factors] <- df_comp[,lapply(.SD,as.factor),.SDcol = factors]

df_sample = sample_n(df_comp, nrow(df_comp)*0.05)

rm(df_comp)
  
str(df_sample)

df_sample%>% 
  group_by(Semana)%>%
  summarise(Demanda_Semanal = sum(Demanda_uni_equil))%>%
  ggplot(aes(x = Semana, y = Demanda_Semanal))+
  geom_line()+
  ggtitle("Mudanca de Necessidade de Estoque")
