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

#Demanda ajustada nas semanas
df_sample%>% 
  group_by(Semana)%>%
  summarise(Demanda_Semanal = sum(Demanda_uni_equil))%>%
  ggplot(aes(x = Semana, y = Demanda_Semanal))+
  geom_line()+
  ggtitle("Mudanca de Necessidade de Estoque")

numeric <- c("Semana","Venta_uni_hoy","Venta_hoy",
                  "Dev_uni_proxima","Dev_proxima","Demanda_uni_equil")

factors <- c("Agencia_ID", "Canal_ID", "Ruta_SAK",
             "Producto_ID")

#verificando diversas distribuicoes das variaveis categoricas
lapply(factors, function(x){
ggplot(df_sample, aes_string(y="Demanda_uni_equil", x = x))+
  geom_boxplot()+ ylim(c(0,1750))
})

#verificando correlacao entre variaveis numericas
corrplot(cor(df_sample[,..numeric]), method = "ellipse")

#potando graficos para verificacao de variaveis categoricas
lapply(factors, function(x){
  df_sample %>% 
    group_by(Semana, .data[[x]])%>%
    summarise(sum = sum(Demanda_uni_equil))%>%
    mutate(name = fct_reorder(.data[[x]],desc(sum)))%>%
    ggplot(aes_string(y="sum", x = "name"))+
    geom_bar(stat = "identity")+
    facet_wrap(. ~ Semana, dir = "v")+
    ggtitle(paste("Realacao de Demanda Ajustada por", x))+
    ylab("Demanda Ajustada")+
    xlab(paste(x))
})

#verificando se existem grandes mudancas de com relacao a vencimento de estoque e vendas na semana
df_sample%>% 
  group_by(Semana)%>%
  summarise(Vendas_Semana = sum(Venta_uni_hoy),
            Vencimento_prox_Semana = sum(Dev_uni_proxima))%>%
  ggplot(aes(x = Semana))+
  geom_line(aes(y=Vendas_Semana, color = "Vendas_Semana"))+
  geom_line(aes(y=Vencimento_prox_Semana, color = "Vencimento_prox_Semana"))+
  ggtitle("Mudancas de Estoque")

#verificando ranking dos produtos que mais chegam proximos ao vencimento percentuais de vencimento sao muito baixos para serem considerados

df_sample %>%
  group_by(Producto_ID)%>%
  summarise(Prox_venc = sum(Dev_uni_proxima))%>%
  arrange(desc(Prox_venc))





