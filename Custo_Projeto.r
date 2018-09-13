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
  tempE <- replicate(50, sum(rtriangle(tam_m, a=12, b=25, c=16)))
  temE_mean <- mean(tempE)
  temE_var <- var(tempE)
  
  #O tempo para escavar uma vala tem uma distribuição normal 
  TempoEscavacao <- rnorm(amostra, mean=temE_mean, sd=sqrt(temE_var))
  
  #Custo total para escavar
  
  CTescavacao <- CustoEscavar * TempoEscavacao
  
  #custo de soldagem dos tubos em horas/junção
  CustoSoldar <- rtriangle(amostra,a=17,b=23, c=18.5)
  
  #tempo de soldagem por junção
  tempS <- replicate(50, sum(rtriangle(tam_m+1, a=4,b=5,c=4.5)))
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


CustoTotal <- function(amostra){
  
  #probabilidade do gasoduto ser maior que 260KM
  prob1 <- sample(35:40, 1 )
  
  #se 0 -> o gasoduto, neste cen�rio, ter� 260KM de extens�o 
  #se 1 -> o gasoduto, neste cen�rio, ter� 290KM de extens�o 
  extensao = replicate(1000, rbinom(1, 1, prob1/100))
  
  Mat <- Material(260, amostra) + extensao * Material(30, amostra)
  MO <- Mao_de_Obra(260, amostra) + extensao *Mao_de_Obra(30, amostra)
  Serv <- Servico(260, amostra) + extensao * Servico(30, amostra)
  
  CT <- Mat + MO + Serv
  
  CT
}

################################QUEST�O 1################################
#Histograma e Fun��o cumulativa do custo total do gasoduto em fun��o
#da percep��o de incerteza da tota alternativa
q1 <- function(amostra){
  CT <- CustoTotal(amostra)
  
  hist(CT, main="Custo total da construção do Gasoduto", col="sienna",
       xlab = "Custo (USD)", ylab="Frequ�ncia")
  
  x<- ecdf(CT)
  
  plot(x, main = "Fun��o Cumulativa do Custo do Gasoduto",
       xlab = "Custo (USD)", ylab = "Pr (CT <= x)")
  
}

################################QUEST�O 2################################
#Retorna o pre�o do proposto -> De acordo com a distribui��o acumulada que obtemos, procuramos o quantil associado a procentagem de 85%
#o alvo de custo -> De acordo com a distribui��o acumulada que obtemos, procuramos o quantil associado a procentagem de 50%
#� o quanto pretendemos gastar durante a obra
#Valor contingenciado -> � o valor protosto menos o alvo de custo, � o quanto salvamos para emprevistos durante a obra
q2 <- function(amostra){
  CT <- CustoTotal(amostra)
  x <- ecdf(CT)
  
  alvo <- quantile(x, 0.5, names = FALSE)
  print(paste("O alvo de custo �: $", alvo))
  
  proposto <- quantile(x, 0.85, names = FALSE)
  print(paste("O preço proposto �: $", proposto))
  
  contingente <- proposto - alvo
  print(paste("O valor contingenciado da obra �: $", contingente))
  
}

################################QUEST�O 3################################
#Aceitamos ou rejeitamos a proposta da costru��o do gasoduto 
q3 <- function(amostra){
  CT <- CustoTotal(amostra)
  x <- ecdf(CT)
  
  alvo <- 0.5
  quantilProposta <- x(45000000)
  
  if( (quantilProposta - alvo) >= 0.15){
    #Qual a probabilidade CT<=45000000
    print( paste("A probablididade do custo total ser maior que $45M �:", quantilProposta))
    
    print("Aceito a proposta!")
  }else{
    print( paste("A probablididade do custo total ser maior que $45M �:", quantilProposta))
    print("Rejeito a proposta.")
  }
  
}