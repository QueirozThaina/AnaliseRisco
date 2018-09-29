library(triangle)
##############################QUESTÃO 1##############################

q1A <- function(amostra=3000){
  frota = 20
  
  #Calcula quantos litros de gasolina é gasto por veiculo em cada cenário
  qLitros <- replicate(amostra, sum(rtriangle(frota, a=40, b=60, c=58)))
  
  custoL <- rtriangle(amostra, a=3.1, b=4, c=3.8)
  
  custoTotal <- qLitros * custoL
  
  hist(custoTotal)
  
  custoTotal
}


q1B <- function(amostra=3000){
  
  #Simulando o gasto diário de um taxi
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
  
  #Simulando o gasto diário de um taxi
  qLitrosT <- replicate(amostra, rtriangle(1, a=40, b=60, c=58))
  custoLitrosT <- replicate(amostra, rtriangle(1, a=3.1, b=4, c=3.8))
  
  meanqLitrosT <- mean(qLitrosT)
  varqLitrosT <-  var(qLitrosT)
  meanCustoLitrosT <- mean(custoLitrosT)
  varcustoLitrosT <- var(custoLitrosT)
  
  #Teorema do produto de duas variáveis
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

##############################QUESTÃO 2##############################

custo_MC <- function(amostra=3000){
  
  #calcula o soma to tempo gasto pelo rebitador em todos os cenários
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
  #Assumindo um Risco de Custo de 85%, teremos que o preço maximo sera de:
  quantil <- quantile(x, 0.85)
  
  plot(x, main = "Função Acumulada do Custo de Mão-de-Obra")
  abline(v=quantil, col="red")
  
  round(quantil,2)
}

q2B <- function(){
  
  MonteCarlo <- custo_MC()
  TCL <- custo_TCL()
  
  #qqnorm da distribuição obitida por Monte Carlo
  qqnorm(MonteCarlo, main="Distribuição de custo - Via: Monte Carlo")
  
  #qqnorm da distribuição obitida pelo TCL
  qqnorm(TCL, main="Distribuição de custo - Via: TCL")
}

##############################QUESTÃO 3##############################

#Função que simula o gasto de um ano
gastoSexta <- function(){
  #sorteia quantas sextas-feiras o ano terá
  nSextas <- sample(40:42,1)
  
  #Numero de executivos que vão almoçar em cada sexta-feira
  qPessoas <- round(rtriangle(nSextas, 16,22,18))
  
  totalAnual <- numeric(nSextas)
  #calcula qual o valor da conta de cada um dos executivos no almoço
  for (i in 1:nSextas){
    total1 <- sum(rtriangle(qPessoas[i], 25,36,28))
    totalAnual[i] <- total1
    
  }
  
  sum(totalAnual)
  
}

q3<- function(N=3000){
  
  #Simulando o gasto anual da empresa N vezes
  totalAnos <- replicate(N, gastoSexta())
  
  hist(totalAnos, main = "Gasto Anual da empresa com os almoços")
  
  x <- ecdf(totalAnos)
  
  #Assumindo um Risco de Custo de 85%:
  
  quantil <- quantile(x, 0.85)
  
  
  
  plot(x, main = "Função Acumulada do Custo anual com os Almoços")
  
  abline(v=quantil, col="red")
  
  
  print(paste("Alvo de Custo", round(quantil,digits=2)))
  #quantil
  
}

##############################QUESTÃO 4##############################
#Calcula o lucro de um mês do restaurante
lucroMensal <- function(){
  #numero de grupos por dia
  nGrupos <- round(rtriangle(30, 40,120,60))
  total <- numeric(30)
  #gasto cada grupo
  for(i in 1:30){
    gastoGrupoDiario <- rtriangle(nGrupos[i], 90,250,130)
    lucroDiario <- rtriangle(nGrupos[i], 0.15,0.3,0.22)
    totalDia <- sum(gastoGrupoDiario * lucroDiario)
    total[i] <- totalDia
  }
  totalMensal <- sum(total)
  
  totalMensal
}


#Calculo do "valor presente do lucro total" (juros compostos)
vLucroTotal <- function(){
  
  Vm <- lucroMensal()
  
  taxa <- 0
  for(i in 1:12){
    t <- (1/(1+0.065))**i
    taxa <- taxa + t
  }
  
  lucroAnual <- Vm*taxa
  
  lucroAnual
}


q4 <- function(amostra=3000){
  #Simulação do lucro em um ano
  lucroAnual <- replicate(amostra,vLucroTotal())
  
  hist(lucroAnual, main = "Valor Presente do Lucro total", xlab = "Lucro primeiro ano",
       ylab = "Frequencia")
}

