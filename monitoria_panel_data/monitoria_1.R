#Remover objetos do ambiente
rm(list=ls())

#Instalando pacotes necessários

#Erros padrão robustos
install.packages("sandwich")

#Testes de hipóteses
install.packages("lmtest")

#Métodos para painel
install.packages("plm")

#Carregar dados
data("EmplUK", package = "plm")

#Número de firmas
length(unique(EmplUK$firm))

#Anos
unique(EmplUK$year)

#Rodar modelo 1
modelo1 = lm(log(output)~log(capital)+log(emp), data = EmplUK)

#Erro padrão homocedástico
summary(modelo1)

#Carregando pacotes
library(sandwich)
library(lmtest)
#Erro padrão heterocedástico
coeftest(modelo1, vcov. = vcovHC)

#Rodar modelo 2
#Permitir tendência comum na produtividade e componente 
#específico ao setor
modelo2 = lm(log(output)~log(capital)+log(emp)+as.factor(sector)+as.factor(year), data = EmplUK)
coeftest(modelo2, vcov. = vcovHC)

#Rodar modelo 3
#Permitir tendência específica ao setor na produtividade 
#específico ao setor
modelo3 = lm(log(output)~log(capital)+log(emp)+as.factor(sector)*as.factor(year), data = EmplUK)
summary(modelo3)


#Carregar pacote de painel
#Referência: https://cran.r-project.org/web/packages/plm/vignettes/plmPackage.html
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