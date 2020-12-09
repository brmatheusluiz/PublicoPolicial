library("ggplot2")

dados <-data.frame(tempo=c(96,92,106,100,98,104,110,101,116,106,109,100,112,105,118,108,113,112,127,117),idade=c(20,20,20,20,25,25,25,25,30,30,30,30,35,35,35,35,40,40,40,40)) 


ggplot(dados,aes(idade,tempo))+geom_point()
#É possível observar um crescimento nos valores da variável Tempo de acordo com o aumento dos valores da variável Idade.

modelo<-lm(dados,formula=tempo~idade)
#O Operador "~" representa "em função de" então o tempo em função da idade

modelo$coefficients
#Como esperavamos, o efeito da idade foi positivo.




#Precisamos então testar sua significância, ou seja, com que nivel de confiânça eu consigo afirmar que este efeito estimado é diferente de zero.
#O comando summary() poderá indicar se os seus parâmetros estimados são significativos ou não, ou seja, se é possível assumir que são diferentes de zero.

summary(modelo)

#Podemos observar neste output as estimativas dos parâmetros, o erro padrão associado a cada estimativa, uma estatística t e um p-valor associado, resultado do teste t utilizado para saber se as estimativas são realmente diferentes de zero.
#Quanto mais asteriscos presentes ao lado do efeito estimado, maior o nível de confiança com que podemos afirmar que o efeito não é nulo.
#Quanto ao R², ao utilizar apenas uma variável é normal que o valor não seja extremamente alto. De qualquer maneira, na prática, 0.56 é um valor bastante razoável.


ggplot(dados, aes(idade,tempo))+geom_point()+geom_smooth(method=lm,se=FALSE)

#A reta estimada claramente não coincidirá com todos os nossos dados. 
#As medidas de distância entre os dados observados e a reta estimada são chamadas resíduos. 
#Os resíduos são utilizados para avaliar o ajuste do modelo, e a qualidade das estimativas feitas a partir dele.


#Vamos utilizar a análise de variância (ANOVA) para visualizarmos o p value
anova(modelo)

#A linearidade indica o grau de associação entre as variáveis testadas e pode ser representada pelo coeficiente de determinação R², vamos utilizar o comando “cor” nativo do R.
cor(dados)
#Correlação de 0.76, logo, correlação positiva.
#Quanto maior for a correlação linear entre as variáveis, mais o gráfico de dispersão irá se parecer com uma reta
ggplot(dados, aes(idade,tempo))+geom_point()+geom_smooth(method=lm,se=FALSE)











