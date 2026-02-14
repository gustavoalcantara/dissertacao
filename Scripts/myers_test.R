#Indíce de Myers
library(dplyr)

myers <- x|>
  dplyr::filter(idade>=10 & idade<=99)

frequencia_1 <- myers|>
  dplyr::group_by(digito = idade %% 10)|>
  dplyr::summarise(contagem_1 = dplyr::n())
frequencia_1

frequencia_2 <- myers|>
  dplyr::group_by(digito = idade %% 20)|>
  dplyr::summarise(contagem_2 = dplyr::n())
frequencia_2

#Atribuição dos Pesos
tabela_myers <- frequencia_1|>
  dplyr::left_join(frequencia_2, by = 'digito')|>
  dplyr::mutate(peso_1 = digito+1,
                peso_2 = 10 - (digito+1),
                pop_misturada = (peso_1*contagem_1) +
                  (peso_2 * contagem_2))

total_misturado <- sum(tabela_myers$pop_misturada)

resultado_myers <- tabela_myers |>
  dplyr::mutate(percentual = (pop_misturada/total_misturado)*100,
                desvio = abs(percentual-10))|>
  dplyr::summarise(indice_myers = sum(desvio)/2)

print(paste('O Índice de Myers (10-99 anos) é:',
            round(resultado_myers$indice_myers)))
  

