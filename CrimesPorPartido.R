#Limpar
rm(list=ls())

library("ggplot2")
library("dplyr")
library("readxl")
library("ggridges")
#Arquivo Conseguido pelo link:
#https://dados.gov.br/dataset/sistema-nacional-de-estatisticas-de-seguranca-publica/resource/feeae05e-faba-406c-8a4a-512aec91a9d1


dados <- read_excel("indicadoressegurancapublicaufjul20.xlsx")
data.partidos<-read_excel("Excel//Pasta1.xlsx")


nomes<-c("UF","TipoCrime","Ano","Mes","Ocorrencia")
names(dados)<-nomes

dados.teste<-dados


#Transformando em factor
data.partidos$UF<-as.factor(data.partidos$UF)
data.partidos$PARTIDO<-as.factor(data.partidos$PARTIDO)

summary(data.partidos)





