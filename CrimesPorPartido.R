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

#Filtra para trabalharmos com o estado do Alagoas
dados.AL<-filter(dados.teste,UF=="Alagoas")

#Crimes no ano  de 2019
dados.AL %>%
  filter(Ano==2019) %>%
  group_by(TipoCrime) %>%
  count(TipoCrime,Quantidade=sum(Ocorrencia)) %>%
  ggplot()+
  geom_bar(aes(x=Quantidade,y=TipoCrime, fill = TipoCrime),stat='identity') +
  ggtitle("Alagoas")

#Crimes no ano  de 2016
dados.AL %>%
  filter(Ano==2016) %>%
  group_by(TipoCrime) %>%
  count(TipoCrime,Quantidade=sum(Ocorrencia)) %>%
  ggplot()+
  geom_bar(aes(x=Quantidade,y=TipoCrime, fill = Quantidade),stat='identity') +
  ggtitle("Alagoas")


#Crimes no ano de 2018 por mês
dados.AL %>%
  filter(Ano==2018) %>%
  group_by(Mes) %>%
  summarise(Quantidade=sum(Ocorrencia)) %>%
  ggplot()+
  geom_bar(aes(x=Quantidade,y=Mes, fill = Quantidade),stat='identity') +
  ggtitle("Quantidade de Ocorrencias Por Mês")

#VERIFICAR QUAIS DIAS TEM O SAIDÃO DOS PRESOS
#VERIFICAR TAMBÉM NO ANO DE 2018 QUANTOS PRESOS FORAM SOLTOS




#Coloca as ocorrencias por partido
dados.AL.Partido<-dados.AL %>%
  group_by(Ano,Partido) %>%
    summarise(Ocorrencias=sum(Ocorrencia))

#Quantidade de Ocorrencias por Ano
dados.AL.Partido %>%
  ggplot()+ geom_bar(aes(x=Ano,y=Ocorrencias, fill = Ano),stat='identity') +
  ggtitle("Quantidade de Ocorrencias por Ano")






