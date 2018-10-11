#Lista 06

CaminhoCritico <- function(d){
  succ <- list(c(2,3),4,6,5,6,c(7,8),c(9,10),9,11,11,12,0)
  pred <- list(0,1,1,2,4,c(3,5),6,6,c(7,8),7,c(9,10),11)
  est <- numeric(12) #tempo mais cedo de inicio
  eft <- numeric(12) #tempo mais tarde de inicio 
  
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
  
  #print(EST)
  #Tempo mais tarde de inicio 
  EFT <- EST + d
  
  lft <- rep(EFT[12],12)
  lst <- numeric(12)
  
  cpmb <- function(s = 12,lft,lst){
    
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
  LFT <- cpmb(12, lft, lst)
  
  folga <- LFT - EFT
  
  #d[folga==0]
  
  folga
  
}

tempoD <- function(d){
  #d<- duracao()
  folga <- CaminhoCritico(d)
  sum(d[folga==0])
}

################################################################################

Duracao <- function(amostra=3000){
  aDuracao <- rtriangle(amostra, 2,18,4)
  bDuracao <- rtriangle(amostra,5,19,9)
  corAB <- matrix(c(1,0.9,0.9,1),ncol=2)
  medA <- mean(aDuracao)
  medB <- mean(bDuracao)
  AB <- mvrnorm(amostra, mu=c(medA, medB), Sigma = corAB)
  
  c<- rtriangle(amostra, 4,28,10)
  
  dDuracao <- rtriangle(amostra,8,36,13)
  eDuracao <- rtriangle(amostra,44,100,60)
  corDE <- matrix(c(1,0.85,0.85,1),ncol=2)
  medD <- mean(dDuracao)
  medE <- mean(eDuracao)
  DE <- mvrnorm(amostra, mu=c(medD,medE), Sigma= corDE)
  
  fduracao <- rtriangle(amostra,30,74,40)
  gDuracao <- rtriangle(amostra,9,43,20)
  corFG <- matrix(c(1,0.9,0.9,1),ncol=2)
  medF <- mean(fduracao)
  medG <- mean(gDuracao)
  FG <- mvrnorm(amostra, mu=c(medF,medG), Sigma = corFG)
  
  hDuracao <- rtriangle(amostra, 24,48,30)
  iDuracao <- rtriangle(amostra,28,96,29)
  jDuracao <- rtriangle(amostra,10,12,10)
  corHIJ <- matrix(c(1,0.85,0.85,0.85,1,0.85,0.85,0.85,1),ncol=3)
  meanH <- mean(hDuracao)
  meanI <- mean(iDuracao)
  meanJ <- mean(jDuracao)
  HIJ <- mvrnorm(amostra, mu= c(meanH,meanI,meanJ), Sigma = corHIJ)
  
  Dur <- data.frame(0,AB,c,DE,FG,HIJ,0)
  names(Dur) <- c('start','A','B','C','D','E','F','G','H','I','J','end')
  
  Dur
}

q1 <- function(amostra=3000){
  dProjeto <- round(Duracao(amostra))
  tcenario <- apply(dProjeto,1,tempoD)
  hist(tcenario, main="Risco de Prazo")
}
