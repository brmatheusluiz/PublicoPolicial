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


#Análise exloratória serve para entendermos um pouco sobre os nossos dados. 
summary(dados)

#Transformar em factor e o numero de ocorrencias
dados$TipoCrime<-as.factor(dados$TipoCrime)
dados$UF<-as.factor(dados$UF)
dados$Mes<-as.factor(dados$Mes)
dados$Ocorrencia<-as.numeric(dados$Ocorrencia)

#Filtrando dados pelo Estado
dados.AC<-filter(dados,UF=="Acre")
dados.AL<-filter(dados,UF=="Alagoas")
dados.AP<-filter(dados,UF=="Amapá")
dados.AM<-filter(dados,UF=="Amazonas")
dados.BA<-filter(dados,UF=="Bahia")
dados.CE<-filter(dados,UF=="Ceará")
dados.DF<-filter(dados,UF=="Distrito Federal")
dados.ES<-filter(dados,UF=="Espírito Santo")
dados.GO<-filter(dados,UF=="Goiás")
dados.MA<-filter(dados,UF=="Maranhão")
dados.MT<-filter(dados,UF=="Mato Grosso")
dados.MS<-filter(dados,UF=="Mato Grosso do Sul")
dados.MG<-filter(dados,UF=="Minas Gerais")
dados.PA<-filter(dados,UF=="Pará")
dados.PB<-filter(dados,UF=="Paraíba")
dados.PR<-filter(dados,UF=="Paraná")
dados.PE<-filter(dados,UF=="Pernambuco")
dados.PI<-filter(dados,UF=="Piauí")
dados.RJ<-filter(dados,UF=="Rio de Janeiro")
dados.RN<-filter(dados,UF=="Rio Grande do Norte")
dados.RS<-filter(dados,UF=="Rio Grande do Sul")
dados.RO<-filter(dados,UF=="Rondônia")
dados.RR<-filter(dados,UF=="Roraima")
dados.SC<-filter(dados,UF=="Santa Catarina")
dados.SP<-filter(dados,UF=="São Paulo")
dados.SE<-filter(dados,UF=="Sergipe")
dados.TO<-filter(dados,UF=="Tocantins")


#Quantidade de Ocorrencias registradas no ano de 2019
#ACRE
dados.AC %>%
  filter(Ano==2019) %>%
    group_by(TipoCrime) %>%
      count(TipoCrime,Quantidade=sum(Ocorrencia)) %>%
  ggplot()+
  geom_bar(aes(x=Quantidade,y=TipoCrime, fill = TipoCrime),stat='identity') +
  ggtitle("Acre")

#Alagoas
dados.AL %>%
  filter(Ano==2019) %>%
  group_by(TipoCrime) %>%
  count(TipoCrime,Quantidade=sum(Ocorrencia)) %>%
  ggplot()+
  geom_bar(aes(x=Quantidade,y=TipoCrime, fill = TipoCrime),stat='identity') +
  ggtitle("Alagoas")

#Alagoas
dados.AP %>%
  filter(Ano==2019) %>%
  group_by(TipoCrime) %>%
  count(TipoCrime,Quantidade=sum(Ocorrencia)) %>%
  ggplot()+
  geom_bar(aes(x=Quantidade,y=TipoCrime, fill = TipoCrime),stat='identity') +
  ggtitle("Amapa")






