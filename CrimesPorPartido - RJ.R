#Limpar
rm(list=ls())

#Operacao da forca nacional em 2017
#https://www.justica.gov.br/news/operacao-da-forca-nacional-no-rio-de-janeiro-e-estendida-por-30-dias
#GLO no rio de janeiro
#https://www.poder360.com.br/governo/rio-de-janeiro-e-estado-com-mais-glos-na-decada/


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

#Verifica a base
summary(dados.teste)

#Transformar em factor
dados.teste$TipoCrime<-as.factor(dados.teste$TipoCrime)
dados.teste$UF<-as.factor(dados.teste$UF)
dados.teste$Mes<-as.factor(dados.teste$Mes)
dados.teste$Ocorrencia<-as.numeric(dados.teste$Ocorrencia)
dados.teste$Partido<-as.factor(dados.teste$Partido)

#Verifica a base
summary(dados.teste)


#Filtra para trabalharmos com o estado do Rio
dados.AC<-filter(dados.teste,UF=="Rio de Janeiro")


##Crimes por ano
#Coloca as ocorrencias por Ano
dados.AC.Partido<-dados.AC %>%
  group_by(Ano,Partido) %>%
  summarise(Ocorrencias=sum(Ocorrencia))
#Quantidade de Ocorrencias por Ano
dados.AC.Partido %>%
  ggplot()+ geom_bar(aes(x=Ano,y=Ocorrencias, fill = Ano),stat='identity') +
  ggtitle("Quantidade de Ocorrencias por Ano")



#Crimes no ano  de 2019
dados.AC %>%
  filter(Ano==2019) %>%
  group_by(TipoCrime) %>%
  count(TipoCrime,Quantidade=sum(Ocorrencia)) %>%
  ggplot()+
  geom_bar(aes(x=Quantidade,y=TipoCrime, fill = TipoCrime),stat='identity') +
  ggtitle("Rio de Janeiro")

#Crimes no ano  de 2018
dados.AC %>%
  filter(Ano==2018) %>%
  group_by(TipoCrime) %>%
  count(TipoCrime,Quantidade=sum(Ocorrencia)) %>%
  ggplot()+
  geom_bar(aes(x=Quantidade,y=TipoCrime, fill = TipoCrime),stat='identity') +
  ggtitle("Rio de Janeiro")


#Filtra as ocorrencias por Roubo de veiculo e Mes no ano de Menor que 2020
dados.AC.Ano<-dados.AC %>%
  filter(Ano<2020) %>%
  group_by(Ano,Mes,TipoCrime) %>%
  summarise(Ocorrencias=sum(Ocorrencia))



#Juntar os dois
dados.AC.Ano<-dados.AC.Ano %>% filter(TipoCrime=="Roubo de veículo")
dados.AC.Ano$Ano<-as.factor(dados.AC.Ano$Ano)
dados.AC.Ano$Mes<-as.character(dados.AC.Ano$Mes)

#Organizar os meses pela ordem de janeiro a dezembro
#dados.AC.Ano <- mutate(dados.AC.Ano,Mes=ifelse(Mes=="Janeiro","1-Janeiro",Mes))

ggplot(dados.AC.Ano,aes(Mes,Ocorrencias,group=Ano)) + 
  geom_line(aes(colour=Ano)) +  
  ggtitle("Roubo de Veiculo")



