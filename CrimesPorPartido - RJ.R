#Limpar
rm(list=ls())

#Operacao da forca nacional em 2017
#https://www.justica.gov.br/news/operacao-da-forca-nacional-no-rio-de-janeiro-e-estendida-por-30-dias
#GLO no rio de janeiro
#https://www.poder360.com.br/governo/rio-de-janeiro-e-estado-com-mais-glos-na-decada/
#https://g1.globo.com/rio-de-janeiro/noticia/temer-assina-decreto-que-autoriza-forcas-armadas-a-atuarem-na-seguranca-publica-do-rio.ghtml
#Intervenção federal no rio de Janeiro:
#https://pt.wikipedia.org/wiki/Interven%C3%A7%C3%A3o_federal_no_Rio_de_Janeiro_em_2018

library("ggplot2")
library("dplyr")
library("readxl")
library("tidyr")
library("magrittr")
#Arquivo Conseguido pelo link:
#https://dados.gov.br/dataset/sistema-nacional-de-estatisticas-de-seguranca-publica/resource/feeae05e-faba-406c-8a4a-512aec91a9d1


dados <- read_excel("indicadoressegurancapublicaufjul20.xlsx")



nomes<-c("UF","TipoCrime","Ano","Mes","Ocorrencia","Partido","Populacao")
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
dados.teste$Populacao<-as.numeric(dados.teste$Populacao)

#Verifica a base
summary(dados.teste)

#Filtra por 2019
dados.teste.2019 <- dados.teste%>% filter(Ano==2019)
dados.teste.2019 <- 
  filter(dados.teste.2019, UF %in% c("São Paulo","Rio de Janeiro","Minas Gerais","Espírito Santo"))

#Cria a numeracao divido pelo numero de habitantes
dados.teste.2019 <- mutate(dados.teste.2019,OcorrenciaPHabitante=Ocorrencia/Populacao)

#Faz um grafico do total de ocorrencias no ano de 2019 por estado
dados.teste.2019 %>% 
  group_by(UF) %>%
  summarise(Ocorrencias=sum(OcorrenciaPHabitante)) %>%
  ggplot()+ geom_bar(aes(x=UF,y=Ocorrencias, fill = UF),stat='identity') +
  ggtitle("Índice de ocorrencias entre os estados do sudeste por 100 mil habitantes")


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


#Faz as alteracoes para rodar o grafico
dados.AC.Ano.furto<-dados.AC.Ano %>% filter(TipoCrime=="Furto de veículo")
dados.AC.Ano.furto$Ano<-as.factor(dados.AC.Ano.furto$Ano)
dados.AC.Ano.furto$Mes<-as.character(dados.AC.Ano.furto$Mes)

#Organizar os meses pela ordem de janeiro a dezembro
dados.AC.Ano.furto$Mes[dados.AC.Ano.furto$Mes=="janeiro"]<-"1 janeiro"
dados.AC.Ano.furto$Mes[dados.AC.Ano.furto$Mes=="fevereiro"]<-"2 fevereiro"
dados.AC.Ano.furto$Mes[dados.AC.Ano.furto$Mes=="março"]<-"3 março"
dados.AC.Ano.furto$Mes[dados.AC.Ano.furto$Mes=="abril"]<-"4 abril"
dados.AC.Ano.furto$Mes[dados.AC.Ano.furto$Mes=="maio"]<-"5 maio"
dados.AC.Ano.furto$Mes[dados.AC.Ano.furto$Mes=="junho"]<-"6 junho"
dados.AC.Ano.furto$Mes[dados.AC.Ano.furto$Mes=="julho"]<-"7 julho"
dados.AC.Ano.furto$Mes[dados.AC.Ano.furto$Mes=="agosto"]<-"8 agosto"
dados.AC.Ano.furto$Mes[dados.AC.Ano.furto$Mes=="setembro"]<-"9 setembro"
dados.AC.Ano.furto$Mes[dados.AC.Ano.furto$Mes=="outubro"]<-"91 outubro"
dados.AC.Ano.furto$Mes[dados.AC.Ano.furto$Mes=="novembro"]<-"92 novembro"
dados.AC.Ano.furto$Mes[dados.AC.Ano.furto$Mes=="dezembro"]<-"93 dezembro"



dados.AC.Ano.furto<-dados.AC.Ano.furto[order(dados.AC.Ano.furto$Ano,dados.AC.Ano.furto$Mes),]


ggplot(dados.AC.Ano.furto,aes(Mes,Ocorrencias,group=Ano)) + 
  geom_line(aes(colour=Ano)) +  
  ggtitle("Furto de Veiculo")


#***********Crimes Gerais***************
 

#Faz as alteracoes para rodar o grafico
dados.RJ.Geral<-dados.AC.Ano

#Gera um total de Ocorrencias Por Mes
dados.RJ.Geral<-dados.RJ.Geral %>%
  group_by(Ano,Mes) %>%
  summarise(Ocorrencias=sum(Ocorrencias))

dados.RJ.Geral$Ano<-as.factor(dados.RJ.Geral$Ano)
dados.RJ.Geral$Mes<-as.character(dados.RJ.Geral$Mes)


#Organizar os meses pela ordem de janeiro a dezembro
dados.RJ.Geral$Mes[dados.RJ.Geral$Mes=="janeiro"]<-"1 janeiro"
dados.RJ.Geral$Mes[dados.RJ.Geral$Mes=="fevereiro"]<-"2 fevereiro"
dados.RJ.Geral$Mes[dados.RJ.Geral$Mes=="março"]<-"3 março"
dados.RJ.Geral$Mes[dados.RJ.Geral$Mes=="abril"]<-"4 abril"
dados.RJ.Geral$Mes[dados.RJ.Geral$Mes=="maio"]<-"5 maio"
dados.RJ.Geral$Mes[dados.RJ.Geral$Mes=="junho"]<-"6 junho"
dados.RJ.Geral$Mes[dados.RJ.Geral$Mes=="julho"]<-"7 julho"
dados.RJ.Geral$Mes[dados.RJ.Geral$Mes=="agosto"]<-"8 agosto"
dados.RJ.Geral$Mes[dados.RJ.Geral$Mes=="setembro"]<-"9 setembro"
dados.RJ.Geral$Mes[dados.RJ.Geral$Mes=="outubro"]<-"91 outubro"
dados.RJ.Geral$Mes[dados.RJ.Geral$Mes=="novembro"]<-"92 novembro"
dados.RJ.Geral$Mes[dados.RJ.Geral$Mes=="dezembro"]<-"93 dezembro"



dados.RJ.Geral<-dados.RJ.Geral[order(dados.RJ.Geral$Ano,dados.RJ.Geral$Mes),]



ggplot(dados.RJ.Geral,aes(Mes,Ocorrencias,group=Ano)) + 
  geom_line(aes(colour=Ano)) +  
  ggtitle("Total de Ocorrencias por Mes") 




