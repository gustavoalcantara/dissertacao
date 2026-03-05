library(ggplot2)
x <- data.table::fread('srag.csv')
#Análise dos Dados no texto do Capítulo de Resultados
#3.1 e 3.2

#Capítulo 3.1
#####Análise dos Missings
x|>
  dplyr::count('Nome da Variável')

#Gera subconjunto de Dados sem os N.As das DCNT
dados_limpo <- x |>
  filter(
    !is.na(possui_cardiopatia) &
      !is.na(possui_asma) &
      !is.na(possui_doenca_neurologica) &
      !is.na(possui_doenca_renal) &
      !is.na(possui_obesidade) &
      !is.na(possui_doenca_renal)
  )

limpo <- data.table::fread('dados_limpo.csv')

write.csv(dados_limpo, "dados_limpo.csv", row.names = FALSE)

#Gera subconjunto de Dados somente dos dados de DCNT
dados_missing <- x |>
  dplyr::filter(
    is.na(possui_cardiopatia) &
      is.na(possui_asma) &
      is.na(possui_pneumopatia) &
      is.na(possui_doenca_renal) &
      is.na(possui_obesidade) &
      is.na(possui_doenca_neurologica)
  )

write.csv(dados_missing, "dados_missing.csv", row.names = FALSE)


####Capítulo 3.2 
#Contagem comum referente aos anos. 
x|>
  dplyr::group_by(status, sexo)|>
  dplyr::summarise(conta = dplyr::n())|>
  print(n=1000)

windows()
#Gráfico da dinamica da alta/óbito durante o tempo
x |>
  dplyr::filter(evolucao_caso %in% c('Óbito', 'Cura')) |>
  dplyr::group_by(data_alta_obito, evolucao_caso) |>
  dplyr::summarise(conta = dplyr::n(), .groups = 'drop') |>
  dplyr::arrange(data_alta_obito) |>
  ggplot(aes(x = data_alta_obito, y = conta, color = evolucao_caso)) +
  geom_line(size = 1) + 
  geom_vline(xintercept = as.numeric(as.Date("2021-01-17")),
             color = "black", linetype = "dashed", size = 1) +
  annotate('text', x = as.Date("2021-01-17"), y = 2500,
           label = 'Início da Vacinação',
           vjust = 38, color = 'black', fontface = 'bold', size = 5) +
  scale_color_manual(name = "Evolução do Caso", values = c("Cura" = "blue", "Óbito" = "red")) +
  labs(x = "Data", y = "Quantidade") +
  ylim(0, 3000) +
  theme_minimal() +
  theme(
    text = element_text(face = "bold", color = "black"),
    axis.text = element_text(face = "bold", color = "black"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12, face = "bold")
  )


#Gráfico Faixa Etária Sexo e Evolução do Caso
windows()
library(tidyverse)
x_prop <- x |>
  dplyr::group_by(faixa_etaria_dez, sexo, evolucao_caso, regiao)|>
  dplyr::summarise(conta = dplyr::n())|>
  dplyr::filter(!is.na(evolucao_caso) & !is.na(faixa_etaria_dez) & !is.na(regiao))|>
  dplyr::filter(!evolucao_caso == 'Óbito por outras causas')|>
  group_by(sexo, regiao) |>
  mutate(proporcao = conta / sum(conta)) |>
  dplyr::ungroup()

windows()
ggplot(x_prop, aes(x = faixa_etaria_dez, y = proporcao, color = evolucao_caso, group = evolucao_caso)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_grid(regiao ~ sexo) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    x = "Faixa Etária",
    y = "Proporção dentro do Grupo",
    color = "Evolução do Caso"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

x_prop

####Gráfico do tempo de cura e óbito por faixa etária e sexo 
#para pessoas com dcnt


#Histograma do tempo até o óbito
x |>
  dplyr::filter(evolucao_caso == 'Óbito') |>
  dplyr::mutate(
    possui_doenca_cronica = dplyr::case_when(
      possui_cardiopatia == "Sim" |
        possui_pneumopatia == "Sim" |
        possui_doenca_neurologica == "Sim" |
        possui_doenca_renal == "Sim" |
        possui_obesidade == "Sim" |
        possui_asma == "Sim" ~ "Com DCNT",
      possui_cardiopatia == "Não" &
        possui_pneumopatia == "Não" &
        possui_doenca_neurologica == "Não" &
        possui_doenca_renal == "Não" &
        possui_obesidade == "Não" &
        possui_asma == "Não" ~ "Sem DCNT",
      TRUE ~ "Ignorado"
    )
  ) -> tempo
  dplyr::filter(
    possui_doenca_cronica != 'Ignorado',
    !is.na(tempo_ate_obito),
    !is.na(regiao),
    !is.na(faixa_etaria_dez),
    tempo_ate_obito >= 0 & tempo_ate_obito <= 60
  ) -> tempo
  ggplot2::ggplot(
    ggplot2::aes(x = faixa_etaria_dez, y = tempo_ate_obito, fill = possui_doenca_cronica)
  ) +
  ggplot2::geom_boxplot(outlier.shape = NA) +
  ggplot2::facet_grid(regiao ~ sexo) +
  ggplot2::scale_fill_manual(values = c("Com DCNT" = "#d95f02", "Sem DCNT" = "#7570b3")) +
  ggplot2::labs(
    x = "Faixa Etária",
    y = "Tempo dos Sintomas até o Óbito (dias)",
    fill = "Presença de DCNT"
  ) +
  ggplot2::theme_light() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )



windows()
library(tidyverse)


##Análise do tempo
summary(tempo$tempo_ate_obito)

tempo|>
  dplyr::mutate(faixa_etaria_ibge =
                 dplyr::case_when(
                   idade <= 14 ~ 'Infância',
                   idade <= 29 ~ 'Jovem',
                   idade <= 59 ~ 'Adulto',
                   idade >= 60 ~ 'Idoso',
                   TRUE ~ 'Sem informação'))|>
  dplyr::filter(faixa_etaria_ibge != 'Sem informação',  
                  !is.na(regiao) & 
                  !is.na(possui_doenca_cronica))|>
  dplyr::group_by(regiao, faixa_etaria_ibge, possui_doenca_cronica) |>
  dplyr::summarise(
    contagem = dplyr::n(),
    media_dias = mean(tempo_ate_obito, na.rm = TRUE),
    mediana_dias = median(tempo_ate_obito, na.rm = TRUE),
    desvio_padrao = sd(tempo_ate_obito, na.rm = TRUE),
    .groups = 'drop'
  ) |>
  dplyr::arrange(regiao, faixa_etaria_ibge, possui_doenca_cronica)|>
  print(n=1000)
