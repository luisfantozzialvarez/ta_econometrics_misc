#Remover objetos do ambiente
rm(list=ls())

#Instalando pacotes necess�rios

#Erros padr�o robustos
install.packages("sandwich")

#Testes de hip�teses
install.packages("lmtest")

#M�todos para painel
install.packages("plm")

#Carregar dados
data("EmplUK", package = "plm")

#N�mero de firmas
length(unique(EmplUK$firm))

#Anos
unique(EmplUK$year)

#Rodar modelo 1
modelo1 = lm(log(output)~log(capital)+log(emp), data = EmplUK)

#Erro padr�o homoced�stico
summary(modelo1)

#Carregando pacotes
library(sandwich)
library(lmtest)
#Erro padr�o heteroced�stico
coeftest(modelo1, vcov. = vcovHC)

#Rodar modelo 2
#Permitir tend�ncia comum na produtividade e componente 
#espec�fico ao setor
modelo2 = lm(log(output)~log(capital)+log(emp)+as.factor(sector)+as.factor(year), data = EmplUK)
coeftest(modelo2, vcov. = vcovHC)

#Rodar modelo 3
#Permitir tend�ncia espec�fica ao setor na produtividade 
#espec�fico ao setor
modelo3 = lm(log(output)~log(capital)+log(emp)+as.factor(sector)*as.factor(year), data = EmplUK)
summary(modelo3)


#Carregar pacote de painel
#Refer�ncia: https://cran.r-project.org/web/packages/plm/vignettes/plmPackage.html
library(plm)

#Criar painel no pacote
painel = pdata.frame(EmplUK,c("firm","year"))

modelo1_painel= plm(log(output)~log(capital)+log(emp), data = painel,
                  model = "pooling")
summary(modelo1_painel)
coeftest(modelo1_painel, vcov = vcovHC, method = "white1")

modelo1_painel_2= plm(log(output)~log(capital)+log(emp)+as.factor(sector), data = painel,
                    effect = "time",
                    model = "pooling")
summary(modelo1_painel_2)
coeftest(modelo1_painel_2, vcov = vcovHC, method = "white1")


modelo1_painel_3= plm(log(output)~log(capital)+log(emp)+as.factor(sector)*as.factor(year), data = painel,
                      model = "pooling")
summary(modelo1_painel_3)
coeftest(modelo1_painel_3, vcov = vcovHC, method = "white1")