library(lpSolve)
library(geobr)
library(sf)
library(units)
library(ggplot2)

resolver_matrix_cobertura_conjuntos <- function(R_max_km) {
  bairros <- read_neighborhood(year = 2022)
  bairros_itacoatiara <- bairros[bairros$code_muni == 1301902, ] 
  
  centroids <- st_centroid(bairros_itacoatiara)
  
  # Criar matriz de distâncias geodésicas (em km)
  dist_matrix <- st_distance(centroids, centroids) / 1000
  dist_matrix <- drop_units(dist_matrix)
  
  # Criar matriz de cobertura (1 se está dentro do raio, 0 caso contrário)
  matrix_cobertura <- ifelse(dist_matrix <= R_max_km, 1, 0)
  
  n <- nrow(bairros_itacoatiara)
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
    bairros_selecionados <- bairros_itacoatiara$name_neighborhood[postos_selecionados]
    cat("Os postos devem ser instalados nos bairros:\n")
    print(bairros_selecionados)
    
    bairros_itacoatiara$selected <- ifelse(1:nrow(bairros_itacoatiara) %in% postos_selecionados, "Selected", "Not Selected")
    
    ggplot() +
      geom_sf(data = bairros_itacoatiara, aes(fill = selected), color = "black") +
      scale_fill_manual(values = c("Selected" = "yellow", "Not Selected" = "lightgrey")) +
      labs(title = "Localização dos Postos de Saúde",
           fill = "Status") +
      theme_minimal()
  } else {
    cat("Não foi possível encontrar uma solução.\n")
  }
}

resolver_matrix_cobertura_conjuntos(5)
