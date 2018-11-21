{library(triangle)

golsFeitos <- read.csv("C:/Users/Thainá/Documents/Faculdade/2018.2/Análise de Risco/golsFeitos.csv",
               sep="\t", header = TRUE)
golsSofridos <- read.csv("C:/Users/Thainá/Documents/Faculdade/2018.2/Análise de Risco/golsSofridos.csv",
                       sep=";", header = TRUE)
}

#Calula a forca de um time no ataque, utilizando os dados dos ultimos 5 campeonatos
#Esta taxa será dada por uma triangular. 
forcaAtaque <- function(amostra){
  forcas <- matrix(0, ncol=20, nrow=amostra)
  colnames(forcas) <- c("PAL","INT","FLA","GRE","SPO","CAM","CRU","CAP","SAN","BAH","BOT",
                        "FLU","COR","VAS","SPORT","CEA","CHA","VIT","AME","PAR")
  for (i in 1:20){
    a<- min(golsFeitos[,i])
    b<- max(golsFeitos[,i])
    c <- golsFeitos[5,i]
    #c <- mean(golsFeitos[,i])
    forcas[,i] <- rtriangle(amostra,a,b,c)
  }
  forcas/38
}

#Simula quantos gols um time sofreu, utilizando como base os dados dos ultimos 5 campeonatos
#Esta taxa será dada por uma triangular. 
Defesa <- function(amostra){
  forcas <- matrix(0, ncol=20, nrow=amostra)
  colnames(forcas) <- c("PAL","INT","FLA","GRE","SPO","CAM","CRU","CAP","SAN","BAH","BOT",
                        "FLU","COR","VAS","SPORT","CEA","CHA","VIT","AME","PAR")
  for (i in 1:20){
    a<- min(golsSofridos[,i])
    b<- max(golsSofridos[,i])
    c <- golsSofridos[5,i]
    #c <- mean(golsSofridos[,i])
    forcas[,i] <- rtriangle(amostra,a,b,c)
  }
  forcas
}

#Funcao auxiliar para calcular de a taxa de defesa de um time é positiva ou negativa
#A taxa sera positiva se a quantidade de gols sofridos for menor do que a quantidade de rodadas
#E a taxa será negativa se a quantidade de gols sofridos for maior do que a quantidade de rodadas
#fazer apply com 2, para pegar cada coluna 
#ex: apply(Defesa(10),2,taxaDefesa)
forcaDefesa <- function(forca){
  n<- length(forca)
  forca2 <- numeric(n)
  for (i in 1:n){
    if (forca[i]>38){
      forca2[i] <- (-forca[i]/38)
    }
    else{
      forca2[i] <- (forca[i]/38)
    }
  }
  forca2
}

#Dado dois times e suas respectivas taxas de ataque e defesa,
#a taxa de gols de cada um dos times é calculada e então é inferido o placar do jogo
#a taxa de gols que cada time faz é dada pro uma poisson 
jogo <- function(timeA, timeB, Ata, Def){
  
  #A<- Ata[1,]
  
  lambdaA <- Ata[timeA] - Def[timeB] + 1
  lambdaB <- Ata[timeB] - Def[timeA]
  
  if(lambdaA < 0){
    lambdaA <- 0
  }
  if(lambdaB < 0){
    lambdaB <- 0
  }
  
  golsA <- rpois(1,lambdaA)
  golsB <- rpois(1,lambdaB)
  
  cbind(golsA, golsB)
}

#Dado o placar de um jogo, altera no vetor passado "mat", nos times "i" e "j"
#a pontuação associada a aquele jogo.
Pontuacao <- function(placar,mat,i,j){
      
  if(placar[1] > placar[2]){
    mat[i] <- mat[i] + 3
    mat[j] <- mat[j] + 0 
  }
  if(placar[1] < placar[2]){
    mat[i] <- mat[i] + 0
    mat[j] <- mat[j] + 3 
  }
  if(placar[1]==placar[2]){
    mat[i] <- mat[i] + 1
    mat[j] <- mat[j] + 1 
  }
  mat
}

#Simula a pontuação de N(amostra) campeonatos
campeonato <- function(amostra){
  times <- c("PAL","INT","FLA","GRE","SPO","CAM","CRU","CAP","SAN","BAH","BOT",
             "FLU","COR","VAS","SPORT","CEA","CHA","VIT","AME","PAR")
  
  camp <- matrix(0,ncol=20,nrow = amostra)
  
  A<- forcaAtaque(amostra)
  C<- apply(Defesa(amostra),2,forcaDefesa)
  
  for(rodada in 1:amostra){
    matriz <- numeric(20)
    for(i in 1:20){
      for(j in (i+1):20){
        if(j>20){
          break()
        }
        placar1 <- jogo(times[i],times[j],A[rodada,],C[rodada,])
        matriz <- Pontuacao(placar1,matriz,i,j)
        placar2 <- jogo(times[j],times[i],A[rodada,],C[rodada,])
        matriz <- Pontuacao(placar2,matriz,j,i)
      }
    }
    camp[rodada,]<-matriz
  }
  colnames(camp) <- times
  
  camp
}

#Retorna a probabilidade de um time ser campeão, dada uma matriz
probsCampeao <- function(matriz){
  d <- dim(matriz)
  matTF <- matrix(0, nrow = d[1],ncol=d[2])
  colnames(matTF) <- c("PAL","INT","FLA","GRE","SPO","CAM","CRU","CAP","SAN","BAH","BOT",
                                "FLU","COR","VAS","SPORT","CEA","CHA","VIT","AME","PAR")
  for(i in 1:d[1]){
    maximo <- max(matriz[i,])
    matTF[i,] <- matriz[i,] >= maximo
  }
  apply(matTF,2,sum)/d[1]
}

#Retorna a probabilidade de rebaixamento de um time, dada uma matriz
probsRebaixamento <- function(matriz){
  d <- dim(matriz)
  matTF <- matrix(0, nrow = d[1],ncol=d[2])
  colnames(matTF) <- c("PAL","INT","FLA","GRE","SPO","CAM","CRU","CAP","SAN","BAH","BOT",
                       "FLU","COR","VAS","SPORT","CEA","CHA","VIT","AME","PAR")
  for(i in 1:d[1]){
    minimo <- min(matriz[i,])
    matTF[i,] <- matriz[i,] <= minimo
  }
  apply(matTF,2,sum)/d[1]
}

main <-  function(amostra=3000){
  
  Camps <- campeonato(amostra)

  campeao <- sort(probsCampeao(Camps), decreasing = TRUE)
  print("As probabilidades de cada um dos times ser CAMPEÃO, no proximo ano são: ")
  print(round(campeao,3))
  
  print("#############################################################################")
  
  rebaixado <- sort(probsRebaixamento(Camps), decreasing = TRUE)
  print("As probabilidades de cada um dos times ser REBAIXADO, no proximo ano são: ")
  print(round(rebaixado,3))
  
}

