library(lpSolve)
library(geobr)
library(sf)
library(ggplot2)

bairros <- read_neighborhood(year = 2022)
bairros_itacoatiara <- bairros[bairros$code_muni == 1301902, ] 

if (nrow(bairros_itacoatiara) == 0) {
  stop("Não foram encontrados bairros para Itacoatiara-AM")
}

coords <- st_centroid(bairros_itacoatiara$geom)
coords_matrix <- st_coordinates(coords) # Converter para matriz numérica

dist_matrix <- as.matrix(dist(coords_matrix))

# Parâmetros do problema
p <- 5  # Número de postos
n <- nrow(bairros_itacoatiara)  # Número de bairros

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
  bairros_selecionados <- bairros_itacoatiara$name_neighborhood[postos_selecionados]
  cat("Os postos devem ser instalados nos bairros com os seguintes IDs:\n")
  print(bairros_selecionados)
  
  # Adicionar uma coluna para indicar se o bairro foi selecionado
  bairros_itacoatiara$selected <- ifelse(1:nrow(bairros_itacoatiara) %in% postos_selecionados, "Selected", "Not Selected")
  
  # Plotar o resultado
  ggplot() +
    geom_sf(data = bairros_itacoatiara, aes(fill = selected), color = "black") +
    scale_fill_manual(values = c("Selected" = "blue", "Not Selected" = "lightgrey")) +
    labs(title = "Localização dos Postos de Saúde (p-Centro)",
         fill = "Status") +
    theme_minimal()
  
} else {
  cat("Não foi possível encontrar uma solução.\n")
}