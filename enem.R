# Ambiente ----
# setwd("~/Pessoal/Aprendendo/R/enem")
setwd("~/Tutoriais - R")
rm(list = ls())

# Bibliotecas ----
# library(readxl)
library(tidyverse)
library(caret)
library(rpart)
library(randomForest)

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


# Criar o modelo de predição de nota ----

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

zero_model <- lm(NOTA_MT_TRAT ~ 1, data = base_trat)

back_model <- step(full_model, scope = formula(full_model), direction = 'backward')

summary(lin_model) # tem 91% de variância explicada
summary(full_model)
summary(back_model)

base_validacao <- base_trat %>% 
  select(TP_PRESENCA_LC, NOTA_MT_TRAT) %>% 
  mutate(pred_lin = ifelse(TP_PRESENCA_LC %in% c(0, 2), 0, predict(lin_model, base_trat)),
         pred_full = ifelse(TP_PRESENCA_LC %in% c(0, 2), 0, predict(full_model, base_trat)),
         pred_back = ifelse(TP_PRESENCA_LC %in% c(0, 2), 0, predict(back_model, base_trat))
         )


ggplot(base_validacao) +
  aes(x = NOTA_MT_TRAT) +
  # geom_point(aes(y = pred_lin)) +
  # geom_point(aes(y = pred_full)) +
  geom_point(aes(y = pred_back), alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1, color = 'red') +
  scale_x_continuous(limits = c(0, 1000)) +
  scale_y_continuous(limits = c(0, 1000)) +
  theme(panel.background = element_rect(fill = 'white', color = 'grey'),
        panel.grid = element_blank())

base_validacao %>% 
  filter(TP_PRESENCA_LC == 1,
         NOTA_MT_TRAT == 0)


RMSE(base_validacao$NOTA_MT_TRAT, base_validacao$pred_full, na.rm = T)
RMSE(base_validacao$NOTA_MT_TRAT, base_validacao$pred_back, na.rm = T)
RMSE(base_validacao$NOTA_MT_TRAT, base_validacao$pred_lin, na.rm = T)


# Bases de treino e teste ----
base_completa <- base_trat %>% 
  filter(!is.na(NOTA_MT_TRAT))

base_na <- base_trat %>% 
  filter(is.na(NOTA_MT_TRAT))

set.seed(1)
index <- createDataPartition(base_completa$NOTA_MT_TRAT, p = 0.7, list = F)

treino <- base_completa[index, ]
teste <- base_completa[-index, ]

# Outros modelos ----
full_model <- lm(NOTA_MT_TRAT ~ ., data = treino)
lin_model <- lm(NOTA_MT_TRAT ~ NOTA_CN_TRAT + NOTA_CH_TRAT + NOTA_LC_TRAT + NOTA_REDACAO_TRAT, 
                data = treino) 

tree_model <- rpart(NOTA_MT_TRAT ~ ., data = treino)
rf_model <- randomForest(NOTA_MT_TRAT ~ ., data = treino)



validacao <- teste %>% 
  select(TP_PRESENCA_LC, NOTA_MT_TRAT) %>% 
  mutate(pred_lin = ifelse(TP_PRESENCA_LC %in% c(0, 2), 0, predict(lin_model, teste)),
         pred_full = ifelse(TP_PRESENCA_LC %in% c(0, 2), 0, predict(full_model, teste)),
         pred_tree = ifelse(TP_PRESENCA_LC %in% c(0, 2), 0, predict(tree_model, teste)),
         pred_rf = ifelse(TP_PRESENCA_LC %in% c(0, 2), 0, predict(rf_model, teste))
         )

ggplot(validacao) +
  aes(x = NOTA_MT_TRAT) +
  # geom_point(aes(y = pred_lin), alpha = 0.2) +
  # geom_point(aes(y = pred_full), alpha = 0.2) +
  # geom_point(aes(y = pred_tree), alpha = 0.2) +
  geom_point(aes(y = pred_rf), alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, color = 'red') +
  scale_x_continuous(limits = c(0, 1000)) +
  scale_y_continuous(limits = c(0, 1000)) +
  labs(x = 'Nota real',
       y = 'Nota predita') +
  theme(panel.background = element_rect(fill = 'white', color = 'grey'),
        panel.grid = element_blank())


RMSE(validacao$NOTA_MT_TRAT, validacao$pred_full, na.rm = T)
RMSE(validacao$NOTA_MT_TRAT, validacao$pred_lin, na.rm = T)
RMSE(validacao$NOTA_MT_TRAT, validacao$pred_tree, na.rm = T)
RMSE(validacao$NOTA_MT_TRAT, validacao$pred_rf, na.rm = T)


##Base final
base_final <- base_na %>% 
  mutate(nota_final_mt = ifelse(TP_PRESENCA_LC %in% c(0, 2), 0, predict(rf_model, base_na)))

















