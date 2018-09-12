Mao_de_Obra <- function(tamanho, amostra){
  tam_m <- (tamanho*1000)/8 #quantidade de canos 
  
  #custo para cavar uma vala em horas
  CustoEscavar <- rtriangle(amostra,a=17,b=23, c=18.5) 
  
  #Tempo para escavar uma vala 
  tempE <- replicate(tam_m, sum(rtriangle(100, a=12, b=25, c=16)))
  temE_mean <- mean(tempE)
  temE_var <- var(tempE)
  #O tempo para escavar uma vala tem uma distribuição normal 
  TempoEscavacao <- rnorm(amostra, mean=temE_mean, sd=sqrt(temE_var))
  
  
  #Custo total para escavar
  CTescavacao <- tam_m * CustoEscavar * TempoEscavacao
  hist(TempoEscavacao*tam_m)
  
  #custo de soldagem dos tubos em horas/junção
  CustoSoldar <- rtriangle(amostra,a=17,b=23, c=18.5)
  
  #numero de soldagem por junção
  tempS <- replicate(tam_m+1, sum(rtriangle(100, a=4,b=5,c=4.5)))
  tempS_mean <- mean(tempS)
  tempS_var <- var(tempS)
  
  #tempo de soldagem tem uma distribuição normal
  TempoSoldagem <- rnorm(amostra, mean= tempS_mean, sd=sqrt(tempS_var))
  
  #Custo total para soldagem 
  CTsoldagem <- tam_m * TempoSoldagem *CustoSoldar
  
  CTmao_de_obra = CTsoldagem + CTescavacao
  
  CTmao_de_obra
  
  hist(CTmao_de_obra)
  
}

#O valor tem que ser aproximadamente isso 

(25*t*23) + (5*t*23)
