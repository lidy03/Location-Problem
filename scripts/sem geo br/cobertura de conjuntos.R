library(lpSolve)
library(readxl)

resolver_matrix_cobertura_conjuntos <- function(arquivo, R_max) {
  # Carregar os dados
  dados <- read_excel(arquivo)
  
  if (!all(c("LAT", "LONG", "ID") %in% colnames(dados))) {
    stop("O arquivo deve conter as colunas LAT, LONG e ID.")
  }
  
  # Criar matriz de distâncias
  coords <- as.matrix(dados[, c("LAT", "LONG")])
  dist_matrix <- as.matrix(dist(coords))
  
  # Criar matriz de cobertura (1 se está dentro do raio, 0 caso contrário)
  matrix_cobertura <- ifelse(dist_matrix <= R_max, 1, 0)
  
  
  n <- nrow(dados)
  var_decisao <- n
  
  # Função objetivo: minimizar o número de postos
  obj <- rep(1, var_decisao)
  
  # Restrição: cada bairro deve ser coberto por pelo menos um posto
  A <- matrix_cobertura
  b <- rep(1, n)  # Cada bairro deve ter pelo menos um posto
  dir <- rep(">=", n)  # Restrição da cobertura
  
  # Resolver o problema
  resultado <- lp("min", obj, A, dir, b, all.bin = TRUE)
  
  if (resultado$status == 0) {
    cat("Solução encontrada com sucesso!\n")
    solucao <- resultado$solution
    postos_selecionados <- which(solucao == 1)
    bairros_selecionados <- dados$ID[postos_selecionados]
    cat("Os postos devem ser instalados nos bairros com os seguintes ID's:\n")
    print(bairros_selecionados)
    
  } else {
    cat("Não foi possível encontrar uma solução.\n")
  }
}

resolver_matrix_cobertura_conjuntos("Bairros.xlsx", 5)
