#Lista 5

succ <- list(c(2,3,4),c(5,7),8, 6, 8, 8,9, 9,NULL)
pred <- list(NULL,1,1,1,c(1,2),c(1,4),c(1,2), c(1,2,3,5),c(1,2,3,5,7,8))
#pred <- list(NULL,1,1,1,2,4,2,c(3,5),c(7,8))
est <- numeric(9)
eft <- numeric(9)

#Tempo mais cedo de incio
cmpf <- function(s,est,eft,d){
  
  eft[s] <- est[s] + d[s]
  
  for(i in succ[[s]]){
    
    if(est[i] < eft[s]){
      est[i] <- eft[s]
    }
    est <- cmpf(i, est,eft,d)
  }
  est
}

#Passo de volta
#lft = est no passo de volta do ultimo nó 
lft <- cmpf(1,est,eft,d)
lst <- numeric(9) #vetor que vai ser preenchido

cpmb <- function(s = 9,lft,lst,d){
  
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

###################################### QUESTÃO 1 #######################################
mediad <- function(a,b,c){
  media <- (a+b+c)/3
  media
}

varianciad <- function(a,b,c){
  variancia <- (a^2 + b^2 + c^2 - a*b - a*c  - b*c)/18
  variancia
}

########################################################################################
#Questao 1 - A
#Obter uma aproximação empirica da duração do projeto 

#Vamos considerar que a duração de cada atividade é a média da triangular
#relacionada aquela atividade
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


q1A <- function(){

  vec <- medVar()
  d <- vec[,1]
  varD <- vec[,2]
  print(d)
  
  tInicio <- cmpf(1,est,eft,d)
  #tFinal <- cpmb(9,lft,lst)
  tFinal <- c(0,4,3,0,6,4,7,9,11)
  
  folga <- tFinal - tInicio
  print(folga)
  
  #Menor tempo de duração do projeto é dado pelo tempo de duração do caminho critico
  media <- sum(d[folga==0])
  variancia <- sum(varD[folga==0])
  
  tcl <- rnorm(3000, media, variancia)
  
  hist(tcl)
  
  tcl
  
}


########################################################################################
#Questao 1 - B
#Obter uma estimativa das probabilidades das atividades que pertecem ao caminho critico


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
  
  d <- c(0,A,B,C,D,E,f,G,0)
  
  round(d)
}

#Calcula o tempo de duração de um projeto
#A partir de um vetor de duração de atividades gerado aleatóriamente
tempoAtividade <- function(){
  
  d <- duracao()
  
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
q1B <- function(){
  tempoMC <- replicate(3000, tempoAtividade())
  
  hist(tempoMC)
  
  tempoMC
}

########################################################################################
#Questao 1 - C
#Comparar os resultados obtidos por Monte Carlo com os resultados obtido por PERK
q1C <- function(){
  perk <- q1A()
  MC <- q1B()
  
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

###################################### QUESTÃO 2 #######################################

########################################################################################
#Questao 2 - A 
succ <- list(c(2,3),4,6,5,6,c(7,8),c(9,10),9,11,11,NULL)
pred <- #fazer
  est2 <- numeric(11)
eft2 <- numeric(11)

medVar2 <- function(){
  A <- c(2,18,4)
  B <- c(5,19,9)
  C <- c(4,28,10)
  D <- c(8,36,13)
  E <- c(44,100,60)
  f <- c(30,74,40)
  G <- c(9,43,10)
  H <- c(24,48,30)
  I <- c(28,96,29)
  J <- c(10,12,10)
  
  tri <- cbind(0,A,B,C,D,E,f,G,H,I,J,0)
  
  vecMedia <- numeric(12)
  vecVar <- numeric(12)
  
  for (i in 1:12){
    vecMedia[i] <- round(mediad(tri[1,i],tri[2,i],tri[3,i]))
    vecVar[i] <- varianciad(tri[1,i],tri[2,i],tri[3,i])
  }
  
  cbind(vecMedia, vecVar)
}


RiscoPrazo <- function(){
  d <- medVar2()[,1]
  varD <- medVar2()[,2]
  tInicio <- cmpf(1,est2,eft2,d)
  #tFinal <- cpmb(11,lft2,lst2)
  tFinal <- c(0,0,30,8,22,41,109,65,174,157,208)
  
  folga <- tFinal - tInicio
  print(folga)
  
  #Menor tempo de duração do projeto é dado pelo tempo de duração do caminho critico
  media <- sum(d[folga==0])
  variancia <- sum(varD[folga==0])
  print(media)
  print(variancia)
  
  tcl <- rnorm(3000, media, variancia)
  
  hist(tcl)
  
  tcl
}


  
