# setwd("~/Pessoal/Aprendendo/R/enem")
setwd("C:/Users/Naruhiko/Documents/Tutoriais - R")

library(readxl)
library(tidyverse)
library(randomForest)
library(rpart)
library(glmnet)
library(MLmetrics)
library(xg)

## Preparando o ambiente

rm(list = ls())

base <- read_xlsx("dados/enem/enem.xlsx") #

summary(base) #tem NAs iguais em grupos de vari�veis

##Fun��o 1: checar se h� os NAs s�o sistem�ticos em rela��o a outras variaveis do banco
missing_check <- function(filtro , grupo, df = base) {
  tabela <- df %>% filter(is.na({{filtro}})) %>% 
    group_by({{grupo}}) %>% 
    count()
  return(tabela)
}

missing_check(TP_ENSINO,TP_ESCOLA) #a maioria n�o respondeu a questao anterior
missing_check(NU_NOTA_CN,TP_PRESENCA_CN) #n�o estavam presentes ou foram eliminados
missing_check(NU_NOTA_LC,TP_PRESENCA_LC) #n�o estavam presentes ou foram eliminados
missing_check(NU_NOTA_MT,TP_PRESENCA_LC) #algumas pessoas n�o foram pq n�o estavam ou forma eliminadas, mas 3377 deeriam ter a nota de matem�tica

##Fun��o 2: checar se pessoas que foram nas provas possuem as notas
presenca_check <- function(filtro, na, df = base) {
  check <- df %>% filter ({{filtro}} == 1, 
                            is.na({{na}})) 
  n_linhas <- nrow(check)
  return(n_linhas)
}

presenca_check(TP_PRESENCA_CN,NU_NOTA_CN) #sim
presenca_check(TP_PRESENCA_CH,NU_NOTA_CH) #sim
presenca_check(TP_PRESENCA_LC,NU_NOTA_LC) #sim
presenca_check(TP_PRESENCA_LC,NU_NOTA_MT) #3377 deveriam ter a nota de matematica

##Fun��o 3: Tratar os missings das outras bases adicionando 0 para quem foi eliminado ou faltou na prova

trat_missing <- function(nota_original, pres_original, pres1 = 0, pres2 = 2) {
  miss_tratado <- as.numeric(ifelse(is.na({{nota_original}})& {{pres_original}} == pres1 | {{pres_original}} == pres2 , "0", {{nota_original}})) 
return(miss_tratado)}


base_trat <- base %>% 
  mutate(NOTA_CN_TRAT = trat_missing(NU_NOTA_CN, TP_PRESENCA_CN),
         NOTA_CH_TRAT = trat_missing(NU_NOTA_CH, TP_PRESENCA_CH),
         NOTA_LC_TRAT = trat_missing(NU_NOTA_LC, TP_PRESENCA_LC),
         NOTA_REDACAO_TRAT= trat_missing(NU_NOTA_REDACAO, TP_PRESENCA_LC),
         NOTA_MT_TRAT = trat_missing(NU_NOTA_MT, TP_PRESENCA_LC))


## Fun��o 4 : Checar se eu fiz certo o anterior
missing_check_trat <- function(col, grupo_pres, df = base_trat) {
  tabela <- df %>% 
    group_by({{grupo_pres}}) %>% 
    summarise(media = mean({{col}}, na.rm = T))
  return(tabela)
}

missing_check_trat(NOTA_CN_TRAT, TP_PRESENCA_CN)
missing_check_trat(NOTA_CH_TRAT, TP_PRESENCA_CH)
missing_check_trat(NOTA_LC_TRAT, TP_PRESENCA_LC)
missing_check_trat(NOTA_REDACAO_TRAT, TP_PRESENCA_LC)
missing_check_trat(NOTA_MT_TRAT, TP_PRESENCA_LC)


## Criar o modelo de predi��o de nota

lin_model <- lm(NOTA_MT_TRAT ~ NOTA_CN_TRAT + NOTA_CH_TRAT + NOTA_LC_TRAT + NOTA_REDACAO_TRAT, data = base_trat) # duvida >> aqui n�o d� certo colocar TP_ST_CONCLUSAO, � pq tem NA?
lin_model_1 <- lm(NOTA_MT_TRAT ~ . , data = base_trat %>% 
                    select(-NU_INSCRICAO), -starts_with())
lin_model_0 <- lm(NOTA_MT_TRAT ~ NOTA_CN_TRAT + NOTA_CH_TRAT + NOTA_LC_TRAT + NOTA_REDACAO_TRAT + 0, data = base_trat)
tree_model <- rpart(NOTA_MT_TRAT ~ ., data = base_trat %>% filter(!is.na(NOTA_MT_TRAT)) %>% select(-NU_INSCRICAO), method = 'anova')
forest_model <- randomForest(NOTA_MT_TRAT ~ NOTA_CN_TRAT + NOTA_CH_TRAT + NOTA_LC_TRAT + NOTA_REDACAO_TRAT, data = base_trat %>% filter(!is.na(NOTA_MT_TRAT)))




summary(lin_model) #tem 91% de acur�cia
summary(lin_model_1) #tem 91% de acur�cia
summary(lin_model_0) #tem 91% de acur�cia


pred <- predict(lin_model, base_trat)
pred1 <- predict(lin_model_1, base_trat)
pred0 <- predict(lin_model_0, base_trat)
pred_tree <- predict(tree_model, base_trat)
pred_forest <- predict(forest_model, base_trat)

##Base final
base_final <- base_trat %>% 
  mutate(nota_mt_pred = pred, 
         nota_mt_pred0 = pred0, 
         nota_mt_pred1 = pred1, 
         nota_mt_tree = pred_tree,
         nota_mt_forest = pred_forest,
         # across(starts_with(nota_mt_, ignore.case = F), ~ ifelse(is.na(NOTA_MT_TRAT)))
         nota_final_mt = ifelse(is.na(NOTA_MT_TRAT), nota_mt_pred, NOTA_MT_TRAT))


base_f_test <- base_final %>% 
  filter(!is.na(NU_NOTA_MT) & NU_NOTA_MT > 0) %>% 
  sample_frac(0.5)

MSE(base_f_test$NU_NOTA_MT, base_f_test$nota_mt_pred)
MSE(base_f_test$NU_NOTA_MT, base_f_test$nota_mt_pred0)
MSE(base_f_test$NU_NOTA_MT, base_f_test$nota_mt_tree)
MSE(base_f_test$NU_NOTA_MT, base_f_test$nota_mt_forest)


RMSE(base_f_test$NU_NOTA_MT, base_f_test$nota_mt_pred)
RMSE(base_f_test$NU_NOTA_MT, base_f_test$nota_mt_pred0)
RMSE(base_f_test$NU_NOTA_MT, base_f_test$nota_mt_pred1)
RMSE(base_f_test$NU_NOTA_MT, base_f_test$nota_mt_tree)
RMSE(base_f_test$NU_NOTA_MT, base_f_test$nota_mt_forest)

##duvida >> porque deu errado eu colocar c(O,2)?
base_trat_errada <- base %>% 
  mutate(NOTA_CN_TRAT = ifelse(is.na(NU_NOTA_CN) & TP_PRESENCA_CN == c(0,2), "0", NU_NOTA_CN))





ggplot(base_final %>% filter(NOTA_MT_TRAT > 0)) +
  aes(x = NOTA_MT_TRAT, y = nota_mt_pred) +
  geom_point() +
  scale_x_continuous(limits = c(0, 1000)) +
  scale_y_continuous(limits = c(0, 1000))


ggplot(base_final %>% filter(NOTA_MT_TRAT > 0)) +
  aes(x = NOTA_MT_TRAT, y = nota_mt_tree) +
  geom_point() +
  scale_x_continuous(limits = c(0, 1000)) +
  scale_y_continuous(limits = c(0, 1000)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = 'Nota matem�tica',
       y = 'Predi��o') +
  theme(panel.background = element_rect(fill = 'white', color = 'gray'),
        panel.grid = element_blank())

ggplot(base_final %>% filter(NOTA_MT_TRAT > 0)) +
  aes(x = NOTA_MT_TRAT) +
  geom_point(aes(y = nota_mt_forest), color = "#25d366", alpha = 0.1) +
  geom_point(aes(y = nota_mt_pred), color = "#5288db", alpha = 0.1) +
  scale_x_continuous(limits = c(0, 1000)) +
  scale_y_continuous(limits = c(0, 1000)) +
  geom_abline(intercept = 0, slope = 1) +
  labs(x = 'Nota matem�tica',
       y = 'Predi��o') +
  theme(panel.background = element_rect(fill = 'white', color = 'gray'),
        panel.grid = element_blank())





