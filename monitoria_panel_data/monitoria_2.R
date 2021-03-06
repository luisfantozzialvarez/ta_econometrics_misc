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

#Erros padr�o homoced�sticos
summary(modelofd)
#Erro padr�o robusto heterocedesticidade
coeftest(modelofd, vcov. = vcovHC, method = "white1")


modelowithin = plm(log(output)~log(capital)+log(emp),
               effect = "individual", model = "within",
               data = painel_pacote) 

#Erros padr�o homoced�sticos
summary(modelowithin)
#Erro padr�o robusto heterocedesticidade
coeftest(modelowithin, vcov. = vcovHC, method = "white1")