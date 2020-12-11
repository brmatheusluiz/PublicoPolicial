#Limpar
rm(list=ls())

library("ggplot2")
library("dplyr")
library("readxl")
library("tidyr")
library("magrittr")
#Arquivo Conseguido pelo link:
#https://dados.gov.br/dataset/sistema-nacional-de-estatisticas-de-seguranca-publica/resource/feeae05e-faba-406c-8a4a-512aec91a9d1


dados <- read_excel("indicadoressegurancapublicaufjul20.xlsx")



nomes<-c("UF","TipoCrime","Ano","Mes","Ocorrencia","Partido")
names(dados)<-nomes

dados.teste<-dados

#Transformando em factor
summary(dados.teste)

#Transformar em factor
dados.teste$TipoCrime<-as.factor(dados.teste$TipoCrime)
dados.teste$UF<-as.factor(dados.teste$UF)
dados.teste$Mes<-as.factor(dados.teste$Mes)
dados.teste$Ocorrencia<-as.numeric(dados.teste$Ocorrencia)
dados.teste$Partido<-as.factor(dados.teste$Partido)

#Filtra para trabalharmos com o estado do Acre
dados.AC<-filter(dados.teste,UF=="Acre")

dados.AC %>%
  filter(Ano==2019) %>%
  group_by(TipoCrime) %>%
  count(TipoCrime,Quantidade=sum(Ocorrencia)) %>%
  ggplot()+
  geom_bar(aes(x=Quantidade,y=TipoCrime, fill = TipoCrime),stat='identity') +
  ggtitle("Acre")


#Coloca as ocorrencias por partido
dados.AC.Partido<-dados.AC %>%
  group_by(Ano,Partido) %>%
    summarise(Ocorrencias=sum(Ocorrencia))

#Quantidade de Ocorrencias por Ano
dados.AC.Partido %>%
  ggplot()+ geom_bar(aes(x=Ano,y=Ocorrencias, fill = Ocorrencias),stat='identity') +
  ggtitle("Quantidade de Ocorrencias por Ano")






