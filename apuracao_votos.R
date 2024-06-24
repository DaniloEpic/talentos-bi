require(dplyr)
require(readxl)



#paineis <- read.table("paineis.txt")
#pre <- data.frame(Painel=paineis[,1],Total_Votos=0)


paineis <- read_xlsx("Paineis_Finalistas.xlsx")
z <- data.frame('Painel'=paineis$Painel,'Votos recebidos'=0,check.names=FALSE)
criterios <- c("Relevância","Inovação","Usabilidade","Design","Assertividade")
k <- matrix(ncol=length(criterios),nrow=nrow(paineis))
colnames(k) <- criterios

# LENDO OS VOTOS DA COMISSÃO JULGADORA
notas <- read.table("Votos-Comissao-Julgadora.csv",sep=",",header=TRUE,check.names=FALSE)
Notas <- notas[,3:52]
n <- sub('Notas para o painel ','',names(Notas))
colnames(Notas) <- n
View(Notas)

# Calculando a nota média de cada critério de todos os paineis
for (i in 1:nrow(k)) {
 for (j in 1:ncol(k)) {
 y <- paste(paineis$Painel[i],". ","[",criterios[j],"]",sep="")
 #print(y)
 notam <- colMeans(Notas[y])
 k[i,j] <- notam
 }
}

nota_comissao <- round(rowSums(k),2)
#View(nota_comissao)

tabela_b <- data.frame(Painel=paineis$Painel,k,nota_comissao)
View(tabela_b)

# Lendo os votos da platéia
a <- read_xlsx("Prêmio Talentos BI - Votação Popular.xlsx")

# Total de Votos
t <- length(a$ID)
b <- a[,6]
colnames(b) <- c('votados')

W <- data.frame()
for (i in 1:t) {
  s <- b$votados[i]
  y <- strsplit(b$votados[i],split=';')
  w <- data.frame(table(y))
  W <- rbind(W,w)
}
colnames(W) <- c('Painel','Total_Votos')
Wd <- rbind(pre,W)
res <- Wd |> group_by(Painel) |> summarize(Votos = sum(Total_Votos))
Res <- res |> arrange(desc(Votos))
#View(Res)

# juntando os votos da comissão julgadora como o voto popular
com <- left_join(tabela_b,res,by=join_by(Painel))
#View(com)


miM <- min(tabela_b$nota_comissao)
maM <- max(tabela_b$nota_comissao)
deltaM <- max(tabela_b$nota_comissao) - miM
miP <- min(Res$Votos)
maP <- max(Res$Votos)
deltaP <- max(Res$Votos) - miP

Res$Votos <- Res$Votos * (0.75 * maM) / maP
#Res$Votos <- miM*0.9 + (Res$Votos - miP) * (deltaM*0.9/deltaP)
#View(Res)
colnames(Res) <- c('Painel','nota_plateia')

#m <- bTab[,c(1,7)]
#colnames(m) <- c('Painel','Nota')
#colnames(Res) <- c('Painel','Nota')
#xr <- rbind(m,Res)
#rf <- xr |> group_by(Painel) |> summarize(Nota=sum(Nota))

#resultado_final <- rf |> arrange(desc(Nota))

Resultado_final <- left_join(com,Res,by=join_by(Painel))
#View(Resultado_final)

Rf <- Resultado_final |> mutate(Score=nota_comissao+nota_plateia)
s <- max(Rf$Score)
resfin <- Rf |> mutate(Pontuacao_Final=(Score*100/s))

View(resfin)
write.table(resfin,'resultado-talentos-bi.csv',sep=';',row.names=FALSE)
























