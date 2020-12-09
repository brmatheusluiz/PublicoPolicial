#Limpar
rm(list=ls())

library("ggplot2")
library("dplyr")
library("readxl")
library("ggridges")
#Arquivo Conseguido pelo link:
#https://dados.gov.br/dataset/sistema-nacional-de-estatisticas-de-seguranca-publica/resource/feeae05e-faba-406c-8a4a-512aec91a9d1


dados <- read_excel("indicadoressegurancapublicaufjul20.xlsx")
dados

nomes<-c("UF","TipoCrime","Ano","Mes","Ocorrencia")
names(dados)<-nomes


#Filtrando dados de SP
dados.sp<-filter(dados,UF=="São Paulo")



#Análise exloratória serve para entendermos um pouco sobre os nossos dados. 
summary(dados.sp)


#Transformar em factor
dados.sp$TipoCrime<-as.factor(dados.sp$TipoCrime)
dados.sp$UF<-as.factor(dados.sp$UF)
dados.sp$Mes<-as.factor(dados.sp$Mes)
dados.sp$Ocorrencia<-as.numeric(dados.sp$Ocorrencia)

#Verificando
summary(dados.sp)

dados.sp %>%
   filter(TipoCrime=="Estupro" & Ano==2019) %>%
ggplot(aes(y = Mes, x = Ocorrencia)) +
  geom_density_ridges(na.rm = TRUE, show.legend = TRUE)






