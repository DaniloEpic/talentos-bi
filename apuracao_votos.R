require(dplyr)
require(readxl)

paineis <- read_xlsx("Paineis_Finalistas.xlsx")

z <- data.frame('Painel'=paineis$Painel,'votos_recebidos'=0,check.names=FALSE)

criterios <- c("Relevância","Inovação","Usabilidade","Design","Assertividade")

k <- matrix(ncol=length(criterios),nrow=nrow(paineis))
colnames(k) <- criterios

View(paineis)
View(z)
View(k)

# LENDO OS VOTOS DA COMISSÃO JULGADORA
notas <- read.table("Votos-Comissao-Julgadora.csv",sep=",",header=TRUE,check.names=FALSE)
dim(notas)
Notas <- notas[,3:52]
n <- sub('Notas para o painel ','',names(Notas))
colnames(Notas) <- n
#View(Notas)

# Calculando a nota média de cada critério de todos os paineis
for (i in 1:nrow(k)) {
 for (j in 1:ncol(k)) {
 y <- paste(paineis$Painel[i],". ","[",criterios[j],"]",sep="")
 notam <- colMeans(Notas[y])
 k[i,j] <- notam
 }
}
#View(k)

nota_comissao <- round(rowSums(k),2)
tabela_b <- data.frame(Painel=paineis$Painel,k,nota_comissao)
#View(tabela_b)



# LENDO OS VOTOS DA PLATEIA
a <- read_xlsx("Prêmio Talentos BI - Votação Popular.xlsx")
#View(a)

# Total de Votos
t <- length(a$ID); t
b <- a[,6]
colnames(b) <- c('votados')

W <- data.frame()
for (i in 1:t) {
  s <- b$votados[i]
  y <- strsplit(b$votados[i],split=';')
  w <- data.frame(table(y))
  W <- rbind(W,w)
}
colnames(W) <- c('Painel','votos_recebidos')
#View(W)

Wd <- rbind(z,W)
#View(Wd)

res <- Wd |> group_by(Painel) |> summarize(Votos = sum(votos_recebidos))
#View(res)

# juntando os votos da comissão julgadora como o voto popular
com <- left_join(tabela_b,res,by=join_by(Painel))
#View(com)

miM <- min(tabela_b$nota_comissao)
maM <- max(tabela_b$nota_comissao)
deltaM <- (maM - miM)
#print(paste("Mínimo: ",miM," Máximo: ",maM," Amplitude: ",deltaM))

Res <- res[res$Votos != 0,]

miP <- min(Res$Votos)
maP <- max(Res$Votos)
deltaP <- (maP - miP)
#print(paste("Mínimo: ",miP," Máximo: ",maP," Amplitude: ",deltaP))


# Método 1
#u <- res$Votos * (0.75 * maM) / maP
#res$Votos <- round(u,2)

res$Votos <- miM*0.9 + (res$Votos - miP) * (deltaM*0.9/deltaP)
colnames(res) <- c('Painel','nota_plateia') 
#View(res)


Resultado_final <- left_join(com,res,by=join_by(Painel))
#View(Resultado_final)

Rf <- Resultado_final |> mutate(Score=nota_comissao+nota_plateia)
s <- max(Rf$Score)
resfin <- Rf |> mutate(Pontuacao_Final=round((Score*100/s),2))
#View(resfin)

resfin['Rank CJ'] <- dense_rank(desc(resfin$nota_comissao))
resfin['Rank VP'] <- dense_rank(desc(resfin$nota_plateia))
resfin['Colocação'] <- dense_rank(desc(resfin$Pontuacao_Final))
View(resfin)

write.table(resfin,'resultado-talentos-bi.csv',sep=';',row.names=FALSE)
