#Serie de hormonios
hormonio = lh

plot(lh)
acf(lh, lag.max = 5)
pacf(lh, lag.max = 5)

tabela = c()

for(p in 0:1)
  for(q in 0:1)
    if(p + q > 0)
    {
      modelo = arima(hormonio, c(p, 0, q) )  
      
      if(p > 0)
      {
          if(T %in% (abs(polyroot(c(1,-1*modelo$model$phi))) <= 1))
            estacionario = "N é estacionário" else
              estacionario = "É estacionario"
      } else estacionario = "N se aplica"
      
      if(q > 0)
      {
        if(T %in% (abs(polyroot(c(1,modelo$model$theta))) <= 1))
          invertivel = "N é  invertível" else
            invertivel = "É invertível"
      } else invertivel = "N se aplica"
      
      lb = Box.test(modelo$residuals, lag = 4, type = "Ljung-Box", fitdf = p + q)
      
      linha = cbind("p"=p, "q"=q, "sigma2" = modelo$sigma2, "N-signif" =
       paste(names(modelo$coef)[abs(modelo$coef/sqrt(diag(modelo$var.coef)))<=qnorm(0.95)],
             collapse = " ; "),"AIC" = AIC(modelo), "BIC" = BIC(modelo),
       "Estacionariedade" = estacionario, "Invertibilidade" = invertivel,
       "p-valor LB (lag = 4)" = lb$p.value)
      
      tabela = rbind(tabela, linha)
    } 

write.csv(tabela,"tabela.csv")
      