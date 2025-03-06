library(lpSolve)
library(readxl)

dados <- read_excel("Bairros.xlsx")

if(!all(c("LAT", "LONG", "ID")%in% colnames(dados))) {
  stop("Deve conter os dados LAT, LONG e ID.")
}

coords <- as.matrix(dados[, c("LAT", "LONG")])
dist_matrix <- as.matrix(dist(coords))

p <- 3 #numero de postos
n <- nrow(dados) #numero de bairros

obj <- c(as.vector(dist_matrix), rep(0,n))

# Restrições de atendimento: Cada bairro deve ser atendido por pelo menos um posto
A1<- matrix(0, n, n * n + n)
row_idx <- seq_len(n)
for (i in row_idx) {
  A1[i, ((i - 1) * n + 1):(i * n)] <- 1
}
b1 <- rep(1,n)

# Restrições de cobertura: Um bairro só pode ser atendido por outro se este for um posto
A2 <- matrix(0, n * n, n * n + n)
for(i in row_idx){
  for(j in row_idx){
    idx <- (i - 1) * n + j
    A2[idx, idx] <- 1
    A2[idx, n*n + j] <- -1
  }
}
b2 <- rep(0, n*n)

# Restrição: Exatamente p postos devem ser instalados
A3 <- matrix(0, 1, n * n + n)
A3[1, (n * n + 1):(n*n+n)] <- 1
b3 <- p

#Combina as restrições
A <-rbind(A1, A2, A3)
dir <- c(rep("=", n), rep("<=", n * n), "=")
b <- c(b1, b2, b3)

resultado <- lp("min", obj, A, dir, b, all.bin = TRUE)

if (resultado$status == 0) {
  cat("Solução encontrada\n")
  #Extrair resultados
  solucao <- resultado$solution
  y <- solucao[(n * n + 1):(n*n+n)]
  postos_selecionados <- which(y == 1)
  bairros_selecionados <- dados$ID[postos_selecionados]
  cat("Os postos devem ser instalados nos seguintes bairros")
  print(bairros_selecionados)
}else{
  cat("Não foi possível encontrar uma solução possível\n")
}