#Lê toda a base
x <- data.table::fread('srag_covid.csv', sep = ',', header = T)

x
#Esse é o filtro da base geral com mais de 3M. de observação
#Exclui gestantes e puerperias
#delimita quem de fato testou positivo pro PCR
#Exclui sexo Indefinido
#Seleciona somente os anos de 2020 a 2022

x|>
  dplyr::filter(classificacao_final == 5 & 
                  pcr_sarscov2 == 1 &
                  puerpera  %in% c(2, 9, NA),
                gestante %in% c(5, 6, 9, NA),
                !sexo == 'I', 
                ano %in% c(2020, 2021, 2022)) -> x

#ETL que muda/cria/renomeia e condiciona variávies
x|>
  dplyr:: mutate(
    idade = as.numeric(difftime(Sys.Date(), data_nascimento, units = "days")) %/% 365,
    faixa_etaria = dplyr::case_when(
      idade >= 0 & idade <= 4 ~ '0-4',
      idade >= 5 & idade <= 9 ~ '5-9',
      idade >= 10 & idade <= 14 ~ '10-14',
      idade >= 15 & idade <= 19 ~ '15-19',
      idade >= 20 & idade <= 24 ~ '20-24',
      idade >= 25 & idade <= 29 ~ '25-29',
      idade >= 30 & idade <= 34 ~ '30-34',
      idade >= 35 & idade <= 39 ~ '35-39',
      idade >= 40 & idade <= 44 ~ '40-44',
      idade >= 45 & idade <= 49 ~ '45-49',
      idade >= 50 & idade <= 54 ~ '50-54',
      idade >= 55 & idade <= 59 ~ '55-59',
      idade >= 60 & idade <= 64 ~ '60-64',
      idade >= 65 & idade <= 69 ~ '65-69',
      idade >= 70 & idade <= 74 ~ '70-74',
      idade >= 75 & idade <= 79 ~ '75-79',
      idade >= 80 ~ '80+'
    ),
    faixa_etaria_dez = dplyr::case_when(
        idade >= 0   & idade <= 9   ~ '0-9',
        idade >= 10  & idade <= 19  ~ '10-19',
        idade >= 20  & idade <= 29  ~ '20-29',
        idade >= 30  & idade <= 39  ~ '30-39',
        idade >= 40  & idade <= 49  ~ '40-49',
        idade >= 50  & idade <= 59  ~ '50-59',
        idade >= 60  & idade <= 69  ~ '60-69',
        idade >= 70  & idade <= 79  ~ '70-79',
        idade >= 80               ~ '80+',
        TRUE                      ~ NA_character_
    ),
    raca_cor = dplyr::case_when(
      raca_cor == 1 ~ 'Branca',
      raca_cor == 2 ~ 'Preta',
      raca_cor == 3 ~ 'Amarela',
      raca_cor == 4 ~ 'Parda',
      raca_cor == 5 ~ 'Indígena',
      TRUE ~ NA_character_  
    ),
    escolaridade = dplyr::case_when(
      escolaridade == 0 ~ 'Sem Escolaridade',
      escolaridade == 1 ~ 'Fundamental 1º Ciclo',
      escolaridade == 2 ~ 'Fundamental 2º Ciclo',
      escolaridade == 3 ~ 'Médio',
      escolaridade == 4 ~ 'Superior',
      TRUE ~ NA_character_  
      ),
    possui_cardiopatia = dplyr::case_when(
      possui_cardiopatia == 1 ~ 'Sim',
      possui_cardiopatia == 2 ~ 'Não',
      TRUE ~ NA_character_  
    ),
    possui_pneumopatia = dplyr::case_when(
      possui_pneumopatia == 1 ~ 'Sim',
      possui_pneumopatia == 2 ~ 'Não',
      TRUE ~ NA_character_  
    ),
    possui_doenca_renal = dplyr::case_when(
      possui_doenca_renal == 1 ~ 'Sim',
      possui_doenca_renal == 2 ~ 'Não',
      TRUE ~ NA_character_
    ),
    possui_doenca_neurologica = dplyr::case_when(
      possui_doenca_neurologica == 1 ~ 'Sim',
      possui_doenca_neurologica == 2 ~ 'Não',
      TRUE ~ NA_character_
    ), 
    possui_obesidade = dplyr::case_when(
      possui_obesidade == 1 ~ 'Sim',
      possui_obesidade == 2 ~ 'Não',
      TRUE ~ NA_character_
    ), 
    possui_asma = dplyr::case_when(
      possui_asma == 1 ~ 'Sim',
      possui_asma == 2 ~ 'Não',
      TRUE ~ NA_character_
    ),
    tempo_ate_internacao = as.numeric(difftime(data_internacao, data_primeiros_sintomas, units = "days")),
    regiao =  dplyr::case_when(
      sigla_uf_internacao %in% c("AC", "AP", "AM", "PA", "RO", "RR", "TO") ~ "Norte",
      sigla_uf_internacao %in% c("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE") ~ "Nordeste",
      sigla_uf_internacao %in% c("DF", "GO", "MT", "MS") ~ "Centro-Oeste",
      sigla_uf_internacao %in% c("ES", "MG", "RJ", "SP") ~ "Sudeste",
      sigla_uf_internacao %in% c("PR", "RS", "SC") ~ "Sul",
      TRUE ~ NA_character_  
    ),
    evolucao_caso = dplyr::case_when(
      evolucao_caso == 1 ~ 'Cura',
      evolucao_caso == 2 ~ 'Óbito',
      evolucao_caso == 3 ~ 'Óbito por outras causas',
      TRUE ~ NA_character_  
    ),
    faixa_etaria_dez = factor(
      faixa_etaria_dez,
      levels = c("0-9", "10-19", "20-29", "30-39", "40-49", 
                 "50-59", "60-69", "70-79", "80+")
    ),
    faixa_etaria = factor(
      faixa_etaria, 
      levels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", 
                 "30-34", "35-39", "40-44", "45-49", "50-54", 
                 "55-59", "60-64", "65-69", "70-74", "75-79", "80+")
    )) -> x

#Criação de Variáveis de tempo
x |>
  dplyr::mutate(tempo_ate_obito = 
                  as.numeric(difftime(data_alta_obito, data_primeiros_sintomas,
                                      units = 'days')),
                tempo_ate_obito_internacao = 
                  as.numeric(difftime(data_alta_obito, data_internacao,
                                      units = 'days')),
                sexo = dplyr::recode(sexo, "F" = "Feminino", "M" = "Masculino")) -> x

#Cria Variável de Status para usar no Kaplan-Meier
x|>
  dplyr::mutate(status = 
                  dplyr::case_when(evolucao_caso == 2 ~ 1,
                                   evolucao_caso == 1 ~ 0,
                                   TRUE ~ NA_real_)) -> x

#Fim da ETL

