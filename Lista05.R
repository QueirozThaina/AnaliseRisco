#Lista 5

succ <- list(c(2,3,4),c(5,7),8, 6, 8, 8,9, 9,NULL)
pred <- list(NULL,1,1,1,c(1,2),c(1,4),c(1,2), c(1,2,3,5),c(1,2,3,5,7,8))
#pred <- list(NULL,1,1,1,2,4,2,c(3,5),c(7,8))
est <- numeric(9)
eft <- numeric(9)


#Aproximação empirica
#vamos supor que a duração de cada atividade sera dada pela média das triangulares
#Aproximação Empirica
d <- c(0,2,6,4,3,5,4,2,0)

cmpf <- function(s,est,eft){
  
  eft[s] <- est[s] + d[s]
  
  for(i in succ[[s]]){
    
    if(est[i] < eft[s]){
      est[i] <- eft[s]
    }
    est <- cmpf(i, est,eft)
  }
  est
}

#Passo de volta
#lft = est no passo de volta do ultimo nó 
lft <- cmpf(1,est,eft)
lst <- numeric(9) #vetor que vai ser preenchido

cpmb <- function(s = 9,lft,lst){
  
  print("Iniciando chamada ...")
  
  lst[s] <- lft[s] - d[s]
  print(lst)
  
  for(i in pred[[s]]){
    
    
    print(pred[[s]])
    print(paste("S:",s))
    print(i)
    
    if(lft[i] > lst[s]){
      lft[i] <- lst[s]
    }
    lft <- cpmb(i, lft, lst)
  }
  
  lft
}

########################################################################################
q1 <- function(){
  #Aproximação empirica:
  #Vamos considerar que a duração de cada atividade é a média da triangular
  #relacionada aquela atividade
  d <- c(0,2,6,4,3,5,4,2,0)
  
  
  tInicio <- cmpf(1,est,eft)
  tFinal <- cpmb(9,lft,lst)
  #tFinal <- c(0,4,3,0,6,4,7,9,11)
  
  folga <- tFinal - tInicio
  print(folga)
  
  #Menor tempo de duração do projeto é dado pelo tempo de duração do caminho critico
  tempoTotal <- sum(d[folga==0])
  
  tempoTotal

}

########################################################################################
library(triangle)
#Retorna um vetor aleatório de duração das atividades
duracao <- function(){
  
  A <- rtriangle(1,1,4,2)
  B <- rtriangle(1,5,7,6)
  C <- rtriangle(1,2,5,4)
  D <- rtriangle(1,1,4,3)
  E <- rtriangle(1,4,7,5)
  f <- rtriangle(1,3,5,4)
  G <- rtriangle(1,1,3,2)
  
  d <- c(A,B,C,D,E,f,G)
  
  d
}

#Calcula o tempo de duração de um projeto
#A partir de um vetor de duração de atividades gerado aleatóriamente
tempoAtividade <- function(){
  
  d <- duracao
  
  tInicio <- cmpf(1,est,eft)
  tFinal <- cpmb(9,lft,lst)
  #tFinal <- c(0,4,3,0,6,4,7,9,11)
  
  folga <- tFinal - tInicio
  print(folga)
  
  #Menor tempo de duração do projeto é dado pelo tempo de duração do caminho critico
  tempoTotalMC <- sum(d[folga==0])
  
  tempoTotalMC

}

#Faz uma estimativa do tempo de duração do projeto utilizando Monte Carlo
q2 <- function(){
  tempoMC <- replicate(3000, tempoAtividade())
  
  hist(tempoMC)
}

########################################################################################
PERT <- function(){
  #Aproximação empirica:
  #Vamos considerar que a duração de cada atividade é a média da triangular
  #relacionada aquela atividade
  d <- c(0,2,6,4,3,5,4,2,0)
  
  var <- #terminar 
    
    tInicio <- cmpf(1,est,eft)
  tFinal <- cpmb(9,lft,lst)
  #tFinal <- c(0,4,3,0,6,4,7,9,11)
  
  folga <- tFinal - tInicio
  print(folga)
  
  #Menor tempo de duração do projeto é dado pelo tempo de duração do caminho critico
  tempoTotal <- sum(d[folga==0])
  
  tempoTotal
  
  medTemp <- sum(d[folga==0])
  varTemp <- sum(var[folga==0])
  tempoTotal <- rnorm(3000, medTemp, varTemp)
  hist(tempoTotal)
}


