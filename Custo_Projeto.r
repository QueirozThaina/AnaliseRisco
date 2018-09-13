library(triangle)

Material <- function(tamanho, amostra){
  
  tam_m <- (tamanho*1000)/8 #quantidade de tubos 
  
  #preco da tubulacao
  precoTub <- rtriangle(amostra, a=725, b=790, c =740)
  
  #Custo da tubulaÃ§Ã£o
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
  
  #O tempo para escavar uma vala tem uma distribuiÃ§Ã£o normal 
  TempoEscavacao <- rnorm(amostra, mean=temE_mean, sd=sqrt(temE_var))
  
  #Custo total para escavar
  
  CTescavacao <- CustoEscavar * TempoEscavacao
  
  #custo de soldagem dos tubos em horas/junÃ§Ã£o
  CustoSoldar <- rtriangle(amostra,a=17,b=23, c=18.5)
  
  #tempo de soldagem por junÃ§Ã£o
  tempS <- replicate(50, sum(rtriangle(tam_m+1, a=4,b=5,c=4.5)))
  tempS_mean <- mean(tempS)
  tempS_var <- var(tempS)
  
  #tempo de soldagem tem uma distribuiÃ§Ã£o normal
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
  
  #Transporte de tubulaÃ§Ã£o 
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
  
  #se 0 -> o gasoduto, neste cenário, terá 260KM de extensão 
  #se 1 -> o gasoduto, neste cenário, terá 290KM de extensão 
  extensao = replicate(1000, rbinom(1, 1, prob1/100))
  
  Mat <- Material(260, amostra) + extensao * Material(30, amostra)
  MO <- Mao_de_Obra(260, amostra) + extensao *Mao_de_Obra(30, amostra)
  Serv <- Servico(260, amostra) + extensao * Servico(30, amostra)
  
  CT <- Mat + MO + Serv
  
  CT
}

################################QUESTÃO 1################################
#Histograma e Função cumulativa do custo total do gasoduto em função
#da percepção de incerteza da tota alternativa
q1 <- function(amostra){
  CT <- CustoTotal(amostra)
  
  hist(CT, main="Custo total da construÃ§Ã£o do Gasoduto", col="sienna",
       xlab = "Custo (USD)", ylab="Frequência")
  
  x<- ecdf(CT)
  
  plot(x, main = "Função Cumulativa do Custo do Gasoduto",
       xlab = "Custo (USD)", ylab = "Pr (CT <= x)")
  
}

################################QUESTÃO 2################################
#Retorna o preço do proposto -> De acordo com a distribuição acumulada que obtemos, procuramos o quantil associado a procentagem de 85%
#o alvo de custo -> De acordo com a distribuição acumulada que obtemos, procuramos o quantil associado a procentagem de 50%
#É o quanto pretendemos gastar durante a obra
#Valor contingenciado -> É o valor protosto menos o alvo de custo, é o quanto salvamos para emprevistos durante a obra
q2 <- function(amostra){
  CT <- CustoTotal(amostra)
  x <- ecdf(CT)
  
  alvo <- quantile(x, 0.5, names = FALSE)
  print(paste("O alvo de custo é: $", alvo))
  
  proposto <- quantile(x, 0.85, names = FALSE)
  print(paste("O preÃ§o proposto é: $", proposto))
  
  contingente <- proposto - alvo
  print(paste("O valor contingenciado da obra é: $", contingente))
  
}

################################QUESTÃO 3################################
#Aceitamos ou rejeitamos a proposta da costrução do gasoduto 
q3 <- function(amostra){
  CT <- CustoTotal(amostra)
  x <- ecdf(CT)
  
  alvo <- 0.5
  quantilProposta <- x(45000000)
  
  if( (quantilProposta - alvo) >= 0.15){
    #Qual a probabilidade CT<=45000000
    print( paste("A probablididade do custo total ser maior que $45M é:", quantilProposta))
    
    print("Aceito a proposta!")
  }else{
    print( paste("A probablididade do custo total ser maior que $45M é:", quantilProposta))
    print("Rejeito a proposta.")
  }
  
}