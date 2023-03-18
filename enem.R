# Ambiente ----
# setwd("~/Pessoal/Aprendendo/R/enem")
setwd("~/Tutoriais - R")
rm(list = ls())

# Bibliotecas ----
# library(readxl)
library(tidyverse)

# Dados ----
# base <- readxl::read_xlsx("enem.xlsx") 
base <- readxl::read_xlsx("dados/enem/enem.xlsx") 

# Funções ----
## Função 1: checar se há os NAs são sistemáticos em relação a outras variaveis do banco
missing_check <- function(filtro , grupo, df = base) {
  tabela <- df %>% filter(is.na({{filtro}})) %>% 
    group_by({{grupo}}) %>% 
    count()
  return(tabela)
}

## Função 2: checar se pessoas que foram nas provas possuem as notas
presenca_check <- function(filtro, na, df = base) {
  check <- df %>% filter ({{filtro}} == 1, 
                          is.na({{na}})) 
  n_linhas <- nrow(check)
  return(n_linhas)
}

## Função 3: Tratar os missings das outras bases adicionando 0 para quem foi eliminado ou faltou na prova

trat_missing <- function(nota_original, pres_original, pres1 = 0, pres2 = 2) {
  miss_tratado <- ifelse({{pres_original}} %in% c(pres1, pres2), 0, {{nota_original}}) 
  return(miss_tratado)
}

## Função 4 : Checar se eu fiz certo o anterior
missing_check_trat <- function(col, grupo_pres, df = base_trat) {
  tabela <- df %>% 
    group_by({{grupo_pres}}) %>% 
    summarise(media = mean({{col}}, na.rm = T))
  return(tabela)
}

# Analisando dados ----
summary(base) #tem NAs iguais em grupos de variáveis

base1 <- base %>% 
  mutate(across(c(SG_UF_RESIDENCIA, starts_with('TP'), starts_with("Q")), as.factor))

missing_check(TP_ENSINO, TP_ESCOLA) # a maioria não respondeu a questao anterior
missing_check(NU_NOTA_CN, TP_PRESENCA_CN) # não estavam presentes ou foram eliminados
missing_check(NU_NOTA_LC, TP_PRESENCA_LC) # não estavam presentes ou foram eliminados
missing_check(NU_NOTA_MT, TP_PRESENCA_LC) # algumas pessoas não foram pq não estavam ou forma eliminadas, mas 3377 deeriam ter a nota de matemática

presenca_check(TP_PRESENCA_CN, NU_NOTA_CN) # sim
presenca_check(TP_PRESENCA_CH, NU_NOTA_CH) # sim
presenca_check(TP_PRESENCA_LC, NU_NOTA_LC) # sim
presenca_check(TP_PRESENCA_LC, NU_NOTA_MT) # 3377 deveriam ter a nota de matematica


# Tratamento de missings ----
base_trat <- base1 %>% 
  mutate(NOTA_CN_TRAT = trat_missing(NU_NOTA_CN, TP_PRESENCA_CN),
         NOTA_CH_TRAT = trat_missing(NU_NOTA_CH, TP_PRESENCA_CH),
         NOTA_LC_TRAT = trat_missing(NU_NOTA_LC, TP_PRESENCA_LC),
         NOTA_REDACAO_TRAT= trat_missing(NU_NOTA_REDACAO, TP_PRESENCA_LC),
         NOTA_MT_TRAT = trat_missing(NU_NOTA_MT, TP_PRESENCA_LC),
         TP_ESCOLA = ifelse(TP_ESCOLA == 4, 1 , TP_ESCOLA),
         across(c(TP_ENSINO, TP_DEPENDENCIA_ADM_ESC), ~ ifelse(is.na(.x), 0, .x)),
         across(c(TP_ENSINO, TP_DEPENDENCIA_ADM_ESC, TP_ESCOLA), factor)) %>% 
  select(-NU_INSCRICAO, -starts_with("NU_NOTA"), -starts_with("Q"), -TP_STATUS_REDACAO)

missing_check_trat(NOTA_CN_TRAT, TP_PRESENCA_CN)
missing_check_trat(NOTA_CH_TRAT, TP_PRESENCA_CH)
missing_check_trat(NOTA_LC_TRAT, TP_PRESENCA_LC)
missing_check_trat(NOTA_REDACAO_TRAT, TP_PRESENCA_LC)
missing_check_trat(NOTA_MT_TRAT, TP_PRESENCA_LC)


## Criar o modelo de predição de nota

full_model <- lm(NOTA_MT_TRAT ~ ., data = base_trat %>% select(starts_with("NOTA"), 
                                                               NU_IDADE, 
                                                               TP_SEXO,
                                                               TP_COR_RACA,
                                                               TP_ST_CONCLUSAO,
                                                               TP_ESCOLA,
                                                               TP_ENSINO,
                                                               TP_DEPENDENCIA_ADM_ESC))

full_model <- lm(NOTA_MT_TRAT ~ ., data = base_trat)

lin_model <- lm(NOTA_MT_TRAT ~ NOTA_CN_TRAT + NOTA_CH_TRAT + NOTA_LC_TRAT + NOTA_REDACAO_TRAT, 
                data = base_trat) # duvida >> aqui não dá certo colocar TP_ST_CONCLUSAO, é pq tem NA?

summary(lin_model) # tem 91% de variância explicada
summary(full_model)

pred <- predict(lin_model, base_trat)

##Base final
base_final <- base_trat %>% 
  mutate(nota_mt_pred = pred, 
         nota_final_mt = ifelse(is.na(NOTA_MT_TRAT), nota_mt_pred, NOTA_MT_TRAT))

##duvida >> porque deu errado eu colocar c(O,2)?
base_trat_errada <- base %>% 
  mutate(NOTA_CN_TRAT = ifelse(is.na(NU_NOTA_CN)&TP_PRESENCA_CN == c(0,2), "0", NU_NOTA_CN))

summary(base_trat)
base_trat %>% group_by(TP_STATUS_REDACAO) %>% count()

