#Lista 5

#Retorna a Folga dado o tempo de duração de cada projeto

CaminhoCritico <- function(d){
  succ <- list(c(2,3,4),c(5,7),8, 6, 8, 8,9, 9,0)
  pred <- list(0,1,1,1,2,4,2,c(3,5,6),c(7,8))
  est <- numeric(9) #tempo mais cedo de inicio
  eft <- numeric(9) #tempo mais tarde de inicio 
  
  cpmf <- function(s,est,eft){
    eft[s] <- est[s] + d[s]
    
    if(succ[[s]][1]!=0){
      for(i in succ[[s]]){
        
        if(est[i] < eft[s]){
          est[i] <- eft[s]
        }
        est <- cpmf(i, est,eft)
      }
    }
    est
  }
  
  
  #Tempo mais cedo de inicio 
  EST <- cpmf(1,est,eft)
  #Tempo mais tarde de inicio 
  EFT <- EST + d
  
  lft <- rep(EFT[9],9)
  lst <- numeric(9)
  
  cpmb <- function(s = 9,lft,lst){
    
    lst[s] <- lft[s] - d[s]
    
    if(pred[[s]][1] != 0){
      for(i in pred[[s]]){
        
        if(lft[i] > lst[s]){
          lft[i] <- lst[s]
        }
        lft <- cpmb(i, lft, lst)
      }
    }
    
    lft
  }
  
  #Tempo mais tarde de tarde de inicio
  LFT <- cpmb(9, lft, lst)
  
  folga <- LFT - EFT
  
  #d[folga==0]
  
  folga
  
}


###################################### QUESTÃO 1 #######################################
#Calcula a média de uma triangular
mediad <- function(a,b,c){
  media <- (a+b+c)/3
  media
}

#Calcula a variância de uma triangular
varianciad <- function(a,b,c){
  variancia <- (a^2 + b^2 + c^2 - a*b - a*c  - b*c)/18
  variancia
}

#Retorna um vetor com a média e a variância de cada uma das atividades
medVar <- function(){
  A <- c(1,4,2)
  B <- c(5,7,6)
  C <- c(2,5,4)
  D <- c(1,4,3)
  E <- c(4,7,5)
  f <- c(3,5,4)
  G <- c(1,3,2)
  
  tri <- cbind(0,A,B,C,D,E,f,G,0)
  vecMedia <- numeric(9)
  vecVar <- numeric(9)
  for (i in 1:9){
    vecMedia[i] <- round(mediad(tri[1,i],tri[2,i],tri[3,i]))
    vecVar[i] <- varianciad(tri[1,i],tri[2,i],tri[3,i])
  }
  x <- cbind(vecMedia, vecVar)
  
  x
}


########################################################################################
#Questao 1 - A
#Obter uma aproximação empirica da duração do projeto

library(triangle)

duracao <- function(){
  
  A <- rtriangle(1,1,4,2)
  B <- rtriangle(1,5,7,6)
  C <- rtriangle(1,2,5,4)
  D <- rtriangle(1,1,4,3)
  E <- rtriangle(1,4,7,5)
  f <- rtriangle(1,3,5,4)
  G <- rtriangle(1,1,3,2)
  
  d <- c(0,A,B,C,D,E,f,G,0)
  
  round(d)
}

#Tempo de duração do Projeto
tempoD <- function(){
  d<- duracao()
  folga <- CaminhoCritico(duracao())
  sum(d[folga==0])
}

#Usando Monte Carlo para obter uma proximação da duração do projeto
q1A <- function(){
  tempo <- replicate(3000,tempoD())
  
  hist(tempo)
  
  tempo
}

########################################################################################
#Questao 1 - B
#Obter uma estimativa das probabilidades das atividades que pertecem ao caminho critico


########################################################################################
#Questao 1 - C
#Comparar os resultados obtidos por Monte Carlo com os resultados obtido por PERK

Perk <- function(){
  
  vec <- medVar()
  d <- vec[,1]
  varD <- vec[,2]
  
  folga <- CaminhoCritico(d)
  
  #Menor tempo de duração do projeto é dado pelo tempo de duração do caminho critico
  media <- sum(d[folga==0])
  variancia <- sum(varD[folga==0])
  
  tcl <- rnorm(3000, media, variancia)
  
  hist(tcl)
  
  tcl
  
}

q1C <- function(){
  perk <- Perk()
  MC <- q1A()
  
  qqnorm(perk, col="blue")
  lines(qqmorm(MC), col="red")
  
}

########################################################################################
#Questao 1 - D

q1D <- function(){
  
  matAtividades <- matrix(0,nrow=1000,ncol=7)
  mat <- medVar()[2:8,]
  for (i in 1:7){
    matAtividades[,i] <- rnorm(1000, mat[i,1], mat[i,2])
  }
  #histograma Atividade A
  hist(matAtividades[,1], main = "Atividade A", xlab = "Duração", ylab = "Frequência",
       col="lightblue")
  #histograma Atividade B
  hist(matAtividades[,2], main = "Atividade B", xlab = "Duração", ylab = "Frequência",
       col="lightpink")
  #histograma Atividade C
  hist(matAtividades[,3], main = "Atividade C", xlab = "Duração", ylab = "Frequência",
       col="orange")
  #histograma Atividade D
  hist(matAtividades[,4], main = "Atividade D", xlab = "Duração", ylab = "Frequência",
       col="lightgreen")
  #histograma Atividade E
  hist(matAtividades[,5], main = "Atividade E", xlab = "Duração", ylab = "Frequência",
       col = "turquoise")
  #histograma Atividade F
  hist(matAtividades[,6], main = "Atividade F", xlab = "Duração", ylab = "Frequência",
       col="yellow1")
  #histograma Atividade G
  hist(matAtividades[,7], main = "Atividade G", xlab = "Duração", ylab = "Frequência",
       col="grey")
}
