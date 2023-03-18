# Ambiente ----
setwd("~/Tutoriais - R")
rm(list = ls())

# Biblioteca ----
library(tidyverse)
library(caret)
library(rpart)
library(randomForest)

# Funcoes ----
na_check <- function(col1, col2, df = enem) {
  tabela <- df %>% 
    group_by({{col1}}, {{col2}}) %>% 
    count()
  return(tabela)
}

missing_check <- function(filtro , grupo, df = base) {
  tabela <- df %>% 
    filter(is.na({{filtro}})) %>% 
    group_by({{grupo}}) %>% 
    count()
  return(tabela)
}

substitui_na <- function(nota, presenca) {
  nova_nota <- ifelse(presenca %in% c(0, 2), 0, nota)
  return(nova_nota)
}


# Dados ----
enem <- openxlsx::read.xlsx('dados/enem/enem.xlsx')

# Tratamento ----
summary(enem)

na_check(TP_ESCOLA, TP_ENSINO)
na_check(TP_PRESENCA_CN, NU_NOTA_CH)


enem1 <- enem %>% 
  mutate(across(c(TP_ENSINO, TP_DEPENDENCIA_ADM_ESC), ~ ifelse(is.na(.x), 0, .x)),
         across(c(SG_UF_RESIDENCIA, starts_with('TP'), starts_with('Q')), factor),
         # across(c(NU_NOTA_LC, NU_NOTA_MT, NU_NOTA_REDACAO), ~substitui_na(.x, TP_PRESENCA_LC))
         ) %>% 
  mutate(NU_NOTA_CH = substitui_na(NU_NOTA_CH, TP_PRESENCA_CH),
         NU_NOTA_CN = substitui_na(NU_NOTA_CN, TP_PRESENCA_CN),
         NU_NOTA_LC = substitui_na(NU_NOTA_LC, TP_PRESENCA_LC),
         NU_NOTA_MT = substitui_na(NU_NOTA_MT, TP_PRESENCA_LC),
         NU_NOTA_REDACAO = substitui_na(NU_NOTA_REDACAO, TP_PRESENCA_LC)  # transformar 3 ult linhas em across
         ) %>% 
  select(-starts_with('Q'), -TP_STATUS_REDACAO, -TP_NACIONALIDADE)


summary(enem1)

cor_raca <- c("Branca",
              "Nao declarado",
              "Preta",
              "Parda",
              "Amarela",
              "Indigena",
              "Nao dispoe info")

enem_trat <- enem1 %>% 
  mutate(COR_RACA = case_when(
                                TP_COR_RACA == 0 ~ "Nao declarado",
                                TP_COR_RACA == 1 ~ "0_Branca",
                                TP_COR_RACA == 2 ~ "Preta",
                                TP_COR_RACA == 3 ~ "Parda",
                                TP_COR_RACA == 4 ~ "Amarela",
                                TP_COR_RACA == 5 ~ "Indigena",
                                T ~ "Nao dispoe info",
                              )) %>% 
  select(-TP_COR_RACA)
  
# Treino e teste ----
## NA e nao-NA
enem_completo <- enem_trat %>% 
  filter(!is.na(NU_NOTA_MT)) %>% 
  select(-NU_INSCRICAO)

set.seed(101)
index <- createDataPartition(enem_completo$NU_NOTA_MT, p = 0.8, list = F)

treino <- enem_completo[index, ]
teste <- enem_completo[-index, ]

enem_na <- enem_trat %>% 
  filter(is.na(NU_NOTA_MT))

# Modelos ----
lin_full_model <- lm(NU_NOTA_MT ~ ., data = treino)
lin_model_notas <- lm(NU_NOTA_MT ~ NU_NOTA_CH + NU_NOTA_CN + NU_NOTA_LC + NU_NOTA_REDACAO, data = treino)
back_model <- step(lin_full_model, direction = 'backward', scope = formula(lin_full_model))
tree_model <- rpart(formula = NU_NOTA_MT ~ ., data = treino, method = 'anova')
rf_model <- randomForest(formula = NU_NOTA_MT ~ ., data = treino)

# Avaliacao de modelos ----
summary(back_model)

lin_full_model_pred <- predict(lin_full_model, teste)
lin_model_notas_pred <- predict(lin_model_notas, teste)
back_model_notas_pred <- predict(back_model, teste)
tree_model_pred <- predict(tree_model, teste)
rf_model_pred <- predict(rf_model, teste)


ggplot(teste) +
  aes(x = NU_NOTA_MT) +
  geom_point(aes(y = tree_model_pred)) +
  geom_abline(intercept = 0, slope = 1, color = 'red') +
  scale_x_continuous(limits = c(0, 1000)) +
  scale_y_continuous(limits = c(0, 1000)) +
  labs(x = 'Nota real',
       y = 'Nota predita') +
  theme(panel.background = element_rect(fill = 'white', color = 'grey'),
        panel.grid = element_blank())


RMSE(lin_model_pred, teste$NU_NOTA_MT)
RMSE(lin_model_notas_pred, teste$NU_NOTA_MT)
RMSE(back_model_notas_pred, teste$NU_NOTA_MT)
RMSE(tree_model_pred, teste$NU_NOTA_MT)
RMSE(rf_model_pred, teste$NU_NOTA_MT)


## O mesmo processo com funcao
create_pred <- function(.data, nome_nova_col, predicao, col_presenca) {
  .data <- .data %>% 
    mutate("{nome_nova_col}" := ifelse({{col_presenca}} %in% c(0, 2), 0, predicao))
}

teste1 <- teste %>% 
  create_pred('full_model', lin_full_model_pred, TP_PRESENCA_LC) %>% 
  create_pred('lin_model', lin_model_notas_pred, TP_PRESENCA_LC) %>% 
  create_pred('back_model', back_model_notas_pred, TP_PRESENCA_LC) %>% 
  create_pred('tree_model', tree_model_pred, TP_PRESENCA_LC) %>% 
  create_pred('rf_model', rf_model_pred, TP_PRESENCA_LC) 


RMSE(teste1$full_model, teste1$NU_NOTA_MT)
RMSE(teste1$lin_model, teste1$NU_NOTA_MT)
RMSE(teste1$back_model, teste1$NU_NOTA_MT)
RMSE(teste1$tree_model, teste1$NU_NOTA_MT)
RMSE(teste1$rf_model, teste1$NU_NOTA_MT)

# Predicao
pred <- predict(rf_model, enem_na)

enem_predito <- enem_na %>% 
  create_pred('NU_NOTA_MT', pred, TP_PRESENCA_LC)







