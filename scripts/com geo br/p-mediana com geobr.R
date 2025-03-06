library(lpSolve)
library(geobr)
library(sf)

# Carregar os dados dos bairros de Itacoatiara, Amazonas
bairros <- read_neighborhood(year = 2022) # Carrega todos os bairros do Brasil
bairros_itacoatiara <- bairros[bairros$code_muni == 1301902, ] # Filtro para Itacoatiara-AM

# Verificar se os dados foram carregados corretamente
if (nrow(bairros_itacoatiara) == 0) {
  stop("Não foram encontrados bairros para Itacoatiara-AM no geobr.")
}

# Obter os centróides dos bairros
coords <- st_centroid(bairros_itacoatiara$geom)
coords_matrix <- st_coordinates(coords) # Converter para matriz numérica

# Criar matriz de distâncias euclidianas
dist_matrix <- as.matrix(dist(coords_matrix))

p <- 3  # Número de postos a instalar
n <- nrow(bairros_itacoatiara)  # Número de bairros

obj <- c(as.vector(dist_matrix), rep(0, n))

# Restrições de atendimento
A1 <- matrix(0, n, n * n + n)
for (i in 1:n) {
  A1[i, ((i - 1) * n + 1):(i * n)] <- 1
}
b1 <- rep(1, n)

# Restrições de cobertura
A2 <- matrix(0, n * n, n * n + n)
for (i in 1:n) {
  for (j in 1:n) {
    idx <- (i - 1) * n + j
    A2[idx, idx] <- 1
    A2[idx, n * n + j] <- -1
  }
}
b2 <- rep(0, n * n)

# Restrição: Exatamente p postos devem ser instalados
A3 <- matrix(0, 1, n * n + n)
A3[1, (n * n + 1):(n * n + n)] <- 1
b3 <- p

# Combina as restrições
A <- rbind(A1, A2, A3)
dir <- c(rep("=", n), rep("<=", n * n), "=")
b <- c(b1, b2, b3)

# Resolver o problema
resultado <- lp("min", obj, A, dir, b, all.bin = TRUE)

if (resultado$status == 0) {
  cat("Solução encontrada\n")
  solucao <- resultado$solution
  y <- solucao[(n * n + 1):(n * n + n)]
  postos_selecionados <- which(y == 1)
  bairros_selecionados <- bairros_itacoatiara$name_neighborhood[postos_selecionados]
  cat("Os postos devem ser instalados nos seguintes bairros:\n")
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
  cat("Não foi possível encontrar uma solução possível\n")
}
