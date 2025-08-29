library(dplyr) 

#Whipple Geral
soma_digitos_0_e_5 <- x |>
  filter(idade >= 23 & idade <= 62 & idade %% 5 == 0) |>
  nrow()

total_populacao_teste <- x |>
  filter(idade >= 23 & idade <= 62) |>
  nrow()

indice_de_whipple <- (soma_digitos_0_e_5 / (total_populacao_teste / 5)) * 100

print(paste("O Índice de Whipple para a variável 'idade' é:", round(indice_de_whipple, 2)))


#