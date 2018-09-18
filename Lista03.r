library(triangle)
##############################QUEST�O 1##############################

q1A <- function(amostra=3000){
  frota = 20
  
  #Calcula quantos litros de gasolina � gasto por veiculo em cada cen�rio
  qLitros <- replicate(amostra, sum(rtriangle(frota, a=40, b=60, c=58)))
  #Custo da gasolina por dia 
  #assumindo que todos abastecem pelo mesmo pre�o a cada dia
  custoL <- rtriangle(amostra, a=3.1, b=4, c=3.8)
  
  custoTotal <- qLitros * custoL
  
  hist(custoTotal)
  
  custoTotal
}


q1B <- function(amostra=3000){
  
  #Simulando o gasto di�rio de um taxi
  qLitrosT <- replicate(amostra, rtriangle(1, a=40, b=60, c=58))
  custoLitrosT <- replicate(amostra, rtriangle(1, a=3.1, b=4, c=3.8))
  
  ctTaxi <- qLitrosT * custoLitrosT
  
  meanFrota <- 20 * mean(ctTaxi)
  varFrota <- 20 * var(ctTaxi)
  
  #Usando o TCL
  ctTaxiNormal <- rnorm(amostra, meanFrota, sqrt(varFrota))
  
  hist(ctTaxiNormal)
  
  ctTaxiNormal
  
}


q1C <- function(amostra=3000){
  
  #Simulando o gasto di�rio de um taxi
  qLitrosT <- replicate(amostra, rtriangle(1, a=40, b=60, c=58))
  custoLitrosT <- replicate(amostra, rtriangle(1, a=3.1, b=4, c=3.8))
  
  meanqLitrosT <- mean(qLitrosT)
  varqLitrosT <-  var(qLitrosT)
  meanCustoLitrosT <- mean(custoLitrosT)
  varcustoLitrosT <- var(custoLitrosT)
  
  #Teorema do produto de duas vari�veis
  meanCusto <- meanqLitrosT * meanCustoLitrosT
  varCusto <- (meanCustoLitrosT^2) * varqLitrosT + (meanqLitrosT^2) * varcustoLitrosT
  
  #Usando o TCL
  custoNorm <- rnorm(amostra, 20*meanCusto, sqrt(20*varCusto))
  
  hist(custoNorm)
  
  custoNorm
  
}

q1D <- function(amostra = 3000){
  
  custoA <- q1A(amostra)
  custoB <- q1B(amostra)
  custoC <- q1C(amostra)
  
  qqnorm(custoA, main = "Modelo de Risco de Custo A")
  qqnorm(custoB, main = "Modelo de Risco de Custo B")
  qqnorm(custoC, main = "Modelo de Risco de Custo C")
}

##############################QUEST�O 2##############################

custo_MC <- function(amostra=3000){
  
  #calcula o soma to tempo gasto pelo rebitador em todos os cen�rios
  tempoT <- replicate(amostra, sum(rtriangle(561, a=3.75,b=5.5,c=4.25)))
  
  custoMC <-7.50 *tempoT
  
  hist(custoMC, main = "Custo do Rebitador - Via: Monte Carlo")
  
  custoMC
  
}

custo_TCL <- function(amostra=3000){
  
  #gerando o tempo para rebitar apenas uma placa
  tempoT <- replicate(amostra, rtriangle(1, a=3.75,b=5.5,c=4.25))
  
  custo <- 7.50 * tempoT
  
  meanTempoT <- 561*mean(custo)
  varTempoT <- 561*var(custo)
  
  #Aplicando o TCL
  custoTCL <- rnorm(amostra, meanTempoT, sqrt(varTempoT))
  
  hist(custoTCL, main = "Custo do Rebitador - Via: TCL")
  
  custoTCL
  
}

q2A <- function(){
  custo <- custo_MC()
  
  x <- ecdf(custo)
  #Assumindo um Risco de Custo de 85%, teremos que o pre�o maximo sera de:
  quantil <- quantile(x, 0.85)
  
  plot(x, main = "Fun��o Acumulada do Custo de M�o-de-Obra")
  abline(v=quantil, col="red")
  
  quantil
}

q2B <- function(){
  
  MonteCarlo <- custo_MC()
  TCL <- custo_TCL()
  
  #qqnorm da distribui��o obitida por Monte Carlo
  qqnorm(MonteCarlo, main="Distribui��o de custo - Via: Monte Carlo")
  
  #qqnorm da distribui��o obitida pelo TCL
  qqnorm(TCL, main="Distribui��o de custo - Via: TCL")
}

