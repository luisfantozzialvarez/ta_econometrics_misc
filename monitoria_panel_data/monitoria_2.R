#Instalar pacote
install.packages("plm")

#Carregar pacote
library("plm")
library("lmtest")

#Carregar base de dados
data("EmplUK", package = "plm")

painel_pacote = pdata.frame(EmplUK, index = c("firm", "year"))

modelofd = plm(log(output)~log(capital)+log(emp),
               effect = "individual", model = "fd",
               data = painel_pacote) 

#Erros padrão homocedásticos
summary(modelofd)
#Erro padrão robusto heterocedesticidade
coeftest(modelofd, vcov. = vcovHC, method = "white1")


modelowithin = plm(log(output)~log(capital)+log(emp),
               effect = "individual", model = "within",
               data = painel_pacote) 

#Erros padrão homocedásticos
summary(modelowithin)
#Erro padrão robusto heterocedesticidade
coeftest(modelowithin, vcov. = vcovHC, method = "white1")