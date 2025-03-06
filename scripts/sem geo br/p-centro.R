#Minimizar a distancia MÁXIMA

library(lpSolve)
library(readxl)

dados <- read_excel("Bairros.xlsx")

if (!all(c("LAT", "LONG", "ID") %in% colnames(dados))) {
  stop("O arquivo deve conter as colunas LAT, LONG e ID.")
}

coords <- as.matrix(dados[, c("LAT", "LONG")])  # Matriz de coordenadas
dist_matrix <- as.matrix(dist(coords))  # Calcula a matriz de distância

# Parâmetros do problema
p <- 3  # Número de postos
n <- nrow(dados)  # Número de bairros

# Introduzir variável para a distância máxima (C)
# C será a distância máxima de qualquer bairro até o posto mais próximo
# Variável C é a que queremos minimizar

obj <- c(rep(0, n * n), rep(1, n))

#0 para as variáveis Xij(bairro é atendido?) porque não influenciam diretamente na distancia max
#1 para a variável C

# Restrições de atendimento: 
#Cada bairro deve ser atendido por pelo menos um posto
A1 <- matrix(0, n, n * n + n)
for (i in 1:n) {
  A1[i, ((i - 1) * n + 1):(i * n)] <- 1
}
b1 <- rep(1, n)

# Restrições de cobertura: Um bairro só pode atender outro se for um posto
A2 <- matrix(0, n * n, n * n + n)
for (i in 1:n) {
  for (j in 1:n) {
    idx <- (i - 1) * n + j
    A2[idx, idx] <- 1
    A2[idx, n * n + j] <- -1
  }
}
b2 <- rep(0, n * n)

# Restrições para limitar a distância máxima (C)
A3 <- matrix(0, n * n, n * n + n)
for (i in 1:n) {
  for (j in 1:n) {
    idx <- (i - 1) * n + j
    A3[idx, idx] <- 1
    A3[idx, n * n + j] <- -1
  }
}
b3 <- rep(0, n * n)

# Restrição: Exatamente p postos devem ser instalados
A4 <- matrix(0, 1, n * n + n)
A4[1, (n * n + 1):(n * n + n)] <- 1
b4 <- p

# Combina todas as restrições
A <- rbind(A1, A2, A3, A4)
dir <- c(rep("=", n), rep("<=", n * n), rep("<=", n * n), "=")
b <- c(b1, b2, b3, b4)

# Resolver o problema
resultado <- lp("min", obj, A, dir, b, all.bin = TRUE)

# Verificar o status da solução
if (resultado$status == 0) {
  cat("Solução encontrada!\n")
  # Extrair resultados
  solucao <- resultado$solution
  y <- solucao[(n * n + 1):(n * n + n)]  # Variáveis y_j
  postos_selecionados <- which(y == 1)
  bairros_selecionados <- dados$ID[postos_selecionados]
  cat("Os postos devem ser instalados nos bairros com os seguintes IDs:\n")
  print(bairros_selecionados)
} else {
  cat("Não foi possível encontrar uma solução.\n")
}
