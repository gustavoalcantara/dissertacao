#KM Geral - Fazer os gráficos
#Chame as bibliotecas
library(tidyverse)
library(survminer)
library(survival)
x|>
  dplyr::filter(tempo_ate_obito>=0 &
                  tempo_ate_obito <= 60) ->x

Surv(x$tempo_ate_obito,x$status) 
table(x$tempo_ate_obito, x$status)

#Variável dependente até o tempo do evento. 
windows()
y<-Surv(x$tempo_ate_obito, x$status) 
y
windows()
plot(y, conf.int=F)

KM<-survfit(y~sexo, data=x)
KM
summary(KM)


#KM SEXO
km_sexo <- surv_fit(y~sexo, data=x)
km_sexo <- survfit(Surv(tempo_ate_obito, status) ~ sexo, data = x)
survdiff(y~sexo, data = x)
# Gráfico  por sexo
plot_sexo <- ggsurvplot(
  km_sexo,
  data = x,
  pval = TRUE,
  conf.int = TRUE,
  #risk.table = TRUE
  xlab = "Tempo (em dias)",
  ylab = "Probabilidade de Sobrevida",
  legend.title = "Sexo",
  legend.labs = c("Feminino", "Masculino")
)
plot_sexo <- plot_sexo$plot

summary(km_sexo)

###KM Faixa Etária
km_faixa_etaria<-survfit(y~faixa_etaria, data=x)
survdiff(y~faixa_etaria_dez, data = x)
plot_faixa_etaria <- ggsurvplot(
  km_faixa_etaria,
  data = x,
  pval = TRUE,
  conf.int = TRUE,
  #risk.table = TRUE
  xlab = "Tempo (em dias)",
  ylab = "Probabilidade de Sobrevida",
  legend.title = "Faixa Etária")
plot_faixa_etaria <- plot_faixa_etaria$plot

###Região
km_regiao <- survfit(y~regiao, data = x)
survdiff(y~regiao, data = x)
plot_regiao <- ggsurvplot(
  km_regiao,
  data = x,
  pval = TRUE,
  conf.int = TRUE,
  #risk.table = TRUE
  xlab = "Tempo (em dias)",
  ylab = "Probabilidade de Sobrevida",
  legend.title = "Região")
plot_regiao <- plot_regiao$plot

#Raça/Cor
km_raca_cor <- survfit(y~raca_cor, data = x)
survdiff(y~raca_cor, data = x)
plot_raca_cor <- ggsurvplot(
  km_raca_cor,
  data = x,
  pval = TRUE,
  conf.int = TRUE,
  #risk.table = TRUE
  xlab = "Tempo (em dias)",
  ylab = "Probabilidade de Sobrevida",
  legend.title = "Raça/cor")
plot_raca_cor <- plot_raca_cor$plot

#Escolaridade
km_escolaridade <- survfit(y~escolaridade, data = x)
survdiff(y~escolaridade, data = x)
plot_escolaridade <- ggsurvplot(
  km_escolaridade,
  data = x,
  pval = TRUE,
  conf.int = TRUE,
  #risk.table = TRUE
  xlab = "Tempo (em dias)",
  ylab = "Probabilidade de Sobrevida",
  legend.title = "Escolaridade")
plot_escolaridade <- plot_escolaridade$plot

#DCNTS
#Cardiopatia
km_cardiopatia <- survfit(y~possui_cardiopatia, data = x)
survdiff(y~possui_cardiopatia, data = x)
plot_cardiopatia <- ggsurvplot(
  km_cardiopatia,
  data = x,
  pval = TRUE,
  conf.int = TRUE,
  #risk.table = TRUE
  xlab = "Tempo (em dias)",
  ylab = "Probabilidade de Sobrevida",
  legend.title = "Cardiopatia")
plot_cardiopatia <- plot_cardiopatia$plot

#Pneumopatia
km_pneumopatia <- survfit(y~possui_pneumopatia, data = x)
survdiff(y~possui_pneumopatia, data = x)
plot_pneumopatia <- ggsurvplot(
  km_pneumopatia,
  data = x,
  pval = TRUE,
  conf.int = TRUE,
  #risk.table = TRUE
  xlab = "Tempo (em dias)",
  ylab = "Probabilidade de Sobrevida",
  legend.title = "Pneumopatia")
plot_pneumopatia <- plot_pneumopatia$plot

#Doença Renal
km_doenca_renal <- survfit(y~possui_doenca_renal, data = x)
survdiff(y~possui_doenca_renal, data = x)
plot_renal <- ggsurvplot(
  km_doenca_renal,
  data = x,
  pval = TRUE,
  conf.int = TRUE,
  #risk.table = TRUE
  xlab = "Tempo (em dias)",
  ylab = "Probabilidade de Sobrevida",
  legend.title = "Doença Renal")
plot_renal <- plot_renal$plot

#Doença Neurologica
km_doenca_neurologica <- survfit(y~possui_doenca_neurologica, data = x)
survdiff(y~possui_doenca_neurologica, data = x)
plot_neurologica <- ggsurvplot(
  km_doenca_neurologica,
  data = x,
  pval = TRUE,
  conf.int = TRUE,
  #risk.table = TRUE
  xlab = "Tempo (em dias)",
  ylab = "Probabilidade de Sobrevida",
  legend.title = "Doença Neurológica")

#Obesidade
km_obesidade <- survfit(y~possui_obesidade, data = x)
KM
survdiff(y~possui_obesidade, data = x)
ggsurvplot(
  km_obesidade,
  data = x,
  pval = TRUE,
  conf.int = TRUE,
  #risk.table = TRUE
  xlab = "Tempo (em dias)",
  ylab = "Probabilidade de Sobrevida",
  legend.title = "Obesidade")

#Asma
km_asma <- survfit(y~possui_asma, data = x)
km_asma
survdiff(y~possui_asma, data = x)
ggsurvplot(
  km_asma,
  data = x,
  pval = TRUE,
  conf.int = TRUE,
  #risk.table = TRUE
  xlab = "Tempo (em dias)",
  ylab = "Probabilidade de Sobrevida",
  legend.title = "Asma")

#Juntar todos os gráficos




