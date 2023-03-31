# Ambiente ----
setwd("~/Tutoriais - R")
rm(list = ls())

# Bibliotecas ----
library(tidyverse)

# Dados ----
base <- readxl::read_xlsx("dados/enem/enem.xlsx")

enem <- base %>% 
  filter(NU_NOTA_MT > 0)

# Loops ----
nomes <- c('Naruhiko', 'Paula', 'Camila', 'Letícia', 'Renata', 'Vinicius') 

for (i in 1:length(nomes)) {
  print(paste0("O nome na posição ", i, " é ", nomes[i]))
}

for (i in nomes) {
  print(i)
}

tp_st_conclusao_enem <- unique(enem$TP_ST_CONCLUSAO)
estado_enem <- unique(enem$SG_UF_RESIDENCIA)


for (i in estado_enem) {
  df_temp <- enem %>% 
  filter(SG_UF_RESIDENCIA == i) %>% 
  group_by(TP_ST_CONCLUSAO) %>% 
  summarise(media = mean(NU_NOTA_MT))
  
  p <- ggplot(df_temp) +
    aes(x = TP_ST_CONCLUSAO, y = media) +
    geom_col(fill = "#5288db") +
    geom_text(aes(label = round(media, 1)), vjust = -1) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = i) +
    theme(panel.background = element_rect(fill = 'white', color = 'grey'),
          panel.grid = element_blank())
  
  print(p)

}


for (i in 1:5) {
  for (j in 1:5) {
    if (j == 5) {
      j <- 10
    }
    print(i*j)
  }
}













