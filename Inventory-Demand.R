setwd("~/Documents/Stock-Demand-Prediction")

library(tidyverse)
library(data.table)
library(lattice)
library(corrplot)

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


#histogramas para cada uma das variaveis inteiras

numeric <- c("Semana","Venta_uni_hoy","Venta_hoy",
                  "Dev_uni_proxima","Dev_proxima","Demanda_uni_equil")

factors <- c("Agencia_ID", "Canal_ID", "Ruta_SAK",
             "Producto_ID")

lapply(factors, function(x){
ggplot(df_sample, aes_string(y="Demanda_uni_equil", x = x))+
  geom_boxplot()+ ylim(c(0,1750))
})

corrplot(cor(df_sample[,..numeric]), method = "ellipse")



