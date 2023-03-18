# Ambiente
setwd("~/Tutoriais - R")
rm(list = ls())

# Biblioteca
library(tidyverse)

# Dados
df <- openxlsx::read.xlsx("dados/base.xlsx")
df2 <- openxlsx::read.xlsx("dados/base_numera.xlsx")

# Tratamento
df1 <- df %>% 
  mutate(Data.de.admissao_padrao = as.Date(Data.de.admissao, origin = '1899-12-30'),
         Data.de.admissao_openxlsx = openxlsx::convertToDate(Data.de.admissao),
         Lider = ifelse(is.na(Lider), "NivelMax", Lider),
         Tipo.de.desligamento = ifelse(is.na(Tipo.de.desligamento), "nao_saiu", Tipo.de.desligamento))


df2 <- df1 %>% 
  group_by(Lider) %>% 
  summarise(qtde = n()) %>% 
  ungroup() %>% 
  mutate(perc = qtde / sum(qtde))
            

df_qtde_wide <- df2 %>% 
  select(Lider, qtde) %>% 
  pivot_wider(names_from = Lider, values_from = qtde)

df_perc_wide <- df2 %>% 
  select(Lider, perc) %>% 
  pivot_wider(names_from = Lider, values_from = perc)

df_wide <- bind_rows(df_qtde_wide, df_perc_wide) # Esse daqui não aceita nomes de colunas diferentes
# bind_rows > Aceita nomes diferentes, mas vai criar novas colunas

df_wide_evasao <- df1 %>% 
  mutate(valor = 1) %>% 
  pivot_wider(names_from = Tipo.de.desligamento, values_from = valor) %>% 
  mutate(nao_saiu  = ifelse(is.na(nao_saiu), 0, nao_saiu),
         vol  = ifelse(is.na(vol), 0, vol),
         invol  = ifelse(is.na(invol), 0, invol),
         )


df_long_evasao <- df_wide_evasao %>% 
  pivot_longer(cols = c(nao_saiu, vol, invol), names_to = 'Tipo.de.desligamento', values_to = 'valor') %>% 
  filter(valor == 1) 

# Analise de missings
df_na <- df2 %>% 
  filter(is.na(av_des_2022))

set.seed(101)
df_na %>% 
  filter(Matricula %in% sample(df_na$Matricula, 10))

df_tratado <- df2 %>% 
  mutate(Tipo.de.desligamento = ifelse(is.na(Tipo.de.desligamento), 'nao saiu', Tipo.de.desligamento),
         media_teste = (av_des_2021 + av_des_2022) / 2) %>% 
  rowwise() %>% 
  mutate(media_notas = mean(c(av_des_2021, av_des_2022), na.rm = TRUE)) %>% 
  ungroup()


df_media <- df_tratado %>% 
  group_by(Tipo.de.desligamento) %>% 
  summarise(media = mean(av_des_2021))

media21 <- mean(df_tratado$av_des_2021)
media22 <- mean(df_tratado$av_des_2022, na.rm = TRUE)

ggplot(df_media) +
  aes(x = Tipo.de.desligamento, y = media) + # aes aplicado globalmente
  geom_col(fill = "#5288db") +
  # geom_col(aes(fill = Tipo.de.desligamento)) +
  # geom_hline(aes(yintercept = media21), linetype = "dashed") +    # vline para linhas verticais
  geom_segment(aes(x = 0.5, y = media21, xend = 3.5, yend = media21), linetype = "dashed") + # controle de onde começa e termina a linha
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  theme(panel.background = element_rect(fill = 'white', color = 'gray60'),
        panel.grid = element_blank(),
        axis.ticks.x = element_blank())

# Teste previsao de missing
lin_model <- lm(av_des_2022 ~ av_des_2021 + Data.de.admissao + Area, data = df2)

summary(lin_model)

df_pred_na <- df_na %>% 
  mutate(nota_pred = predict(lin_model, av_des_2021))

pred <- predict(lin_model, df_na)


df_na$nota_pred <- pred



