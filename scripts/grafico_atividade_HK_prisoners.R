# Ambiente
setwd("C:/Users/Naruhiko/OneDrive/Documentos/Programação/atividades-R")
rm(list = ls())

# Biliotecas
library(tidyverse)
library(lubridate)
library(RColorBrewer)

windowsFonts(chosen_one = windowsFont("Berlin Sans FB Demi"))

# Dados
df <- openxlsx::read.xlsx('dados/HKPPD_ver1.1.xlsx', sheet = 2, detectDates = T)

# Tratamento
dft <- df %>% 
  mutate(month_year = floor_date(sentenced_date, 'month')) %>% 
  group_by(month_year) %>% 
  summarise(avg_sentence = mean(sentence_months, na.rm = T),
            qtde = n()) %>% 
  filter(!is.na(month_year))

# Grafico
ggplot(dft) +
  aes(x = month_year, y = qtde) +
  geom_segment(aes(x = month_year, y = 0, xend = month_year, yend = qtde)) +
  geom_point(aes(size = avg_sentence, color = rev(month_year))) +
  scale_size_continuous(range = c(1, 8), breaks = c(10, 20, 30), limits = c(0, 30)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_color_continuous(guide = 'none') +
  labs(title = 'Political prisoners in Hong Kong',
       subtitle = 'Number of people sentenced to prison',
       y = '',
       x = '',
       size = 'Average sentence\nlength (months)',
       caption = 'Data: Hong Kong Political Prisoners Database') +
  theme(panel.background = element_rect(fill = '#fafbff'),
        plot.background = element_rect(fill = '#fafbff'),
        text = element_text(family = 'chosen_one'),
        axis.ticks = element_blank(),
        axis.line.x = element_line(color = 'black'),
        legend.position = 'bottom',
        legend.background = element_rect(fill = '#fafbff'),
        legend.key = element_blank(),
        legend.box = 'horizontal',
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 12),
        plot.caption = element_text(hjust = 0.5, margin = margin(15, 0, 10, 0)),
        axis.title = element_blank()) +
  guides(size = guide_legend(title.position = 'top', title.hjust = 0.5))



