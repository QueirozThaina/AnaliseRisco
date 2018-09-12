library(triangle)

Material <- function(tamanho, amostra){
  
  tam_m <- (tamanho*1000)/8 #quantidade de tubos 
  
  #preco da tubulacao
  precoTub <- rtriangle(amostra, a=725, b=790, c =740)
  
  #Custo da tubulação
  CustoMaterial <- tam_m*precoTub 
  
  CustoMaterial
}

Mao_de_Obra <- function(tamanho, amostra){
  tam_m <- (tamanho*1000)/8 #quantidade de canos 
  
  #custo para cavar uma vala em horas
  CustoEscavar <- rtriangle(amostra,a=17,b=23, c=18.5) 
  
  #Tempo para escavar uma vala 
  tempE <- replicate(100, sum(rtriangle(tam_m, a=12, b=25, c=16)))
  temE_mean <- mean(tempE)
  temE_var <- var(tempE)
  
  #O tempo para escavar uma vala tem uma distribuição normal 
  TempoEscavacao <- rnorm(amostra, mean=temE_mean, sd=sqrt(temE_var))
  
  #Custo total para escavar
  
  CTescavacao <- CustoEscavar * TempoEscavacao
  
  #custo de soldagem dos tubos em horas/junção
  CustoSoldar <- rtriangle(amostra,a=17,b=23, c=18.5)
  
  #tempo de soldagem por junção
  tempS <- replicate(100, sum(rtriangle(tam_m+1, a=4,b=5,c=4.5)))
  tempS_mean <- mean(tempS)
  tempS_var <- var(tempS)
  
  #tempo de soldagem tem uma distribuição normal
  TempoSoldagem <- rnorm(amostra, mean= tempS_mean, sd=sqrt(tempS_var))
  
  #Custo total para soldagem 
  
  CTsoldagem <- TempoSoldagem *CustoSoldar
  
  Cenario <- cbind(CTescavacao, CTsoldagem)
  
  
  #Custo total da mao de obra
  CTmao_de_obra <-apply(Cenario, M=1, sum)
  
  CTmao_de_obra
  
}

Servico <- function(tamanho, amostra){
  
  tam_m <- (tamanho*1000)/8
  
  #Transporte de tubulação 
  CustoTransporte <- rtriangle(amostra,a=6.1, b=7.4, c=6.6)
  CTtransporte <- tam_m*CustoTransporte
  
  
  #Sistema de Filtragem 
  CTfiltragem <- rtriangle(amostra, a=165000, b= 188000, c= 173000)
  
  
  #Custo acabamento 
  CustoAcabamento <- rtriangle(amostra, a=14000,b=17000,c=15000)
  CTAcabamento <- tamanho * CustoAcabamento
  
  Cenario2 <- cbind(CTtransporte, CTfiltragem, CTAcabamento)
  
  
  #Calculo total dos servicos
  CTservico <- apply(Cenario2, M=1, sum)
  
  CTservico
  
}

CustoTotal <- function(tamanho, amostra){
  
  #Usando as funções anteriores para calular o valor de cada área do projeto
  #Custo certo do Projeto (260 km)
  Mat <- Material(260, amostra)
  MO <- Mao_de_Obra(260, amostra) 
  Serv <- Servico(260, amostra)
  
  CustoCerto <- Mat + MO + Serv
  
  CustoCont <- 0 
  
  if( tamanho > 260){
    Cont <- tamanho - 260
    
    prob <- sample(35:40, 1 )
    Mat_2 <- Material(Cont, amostra) * rbinom(amostra,1,(prob/100))
    MO_2 <- Mao_de_Obra(Cont, amostra) * rbinom(amostra,1,(prob/100))
    Serv_2 <- Servico(Cont, amostra) * rbinom(amostra,1,(prob/100))
    
    CustoCont <- Mat_2 + MO_2 + Serv_2
  }
  
  #Custo total
  CT <- CustoCerto + CustoCont
  
  hist(CT, main="Custo total da construção do Gasoduto", col="sienna",
       xlab = "Custo (USD)", ylab="Frequência")
  
  x<- ecdf(CT)
  plot(x, main = "Função Cumulativa do Custo do Gasoduto",
       xlab = "Custo (USD)", ylab = "Pr (CT <= x)")
  
  #Qual a probabilidade CT<=45000000
  print("A probablididade do custo total ser maior que $45M é:")
  x(45000000)
}


