# ambiente
setwd("~/Tutoriais - R")
rm(list = ls())

# Dados
train <- read_csv('dados/enem/train.csv')
test <- read_csv('dados/enem/test.csv')

colunas <- c(
  "NU_INSCRICAO",           "SG_UF_RESIDENCIA",       "NU_IDADE",               "TP_SEXO",               
  "TP_COR_RACA",            "TP_NACIONALIDADE",       "TP_ST_CONCLUSAO",        "TP_ANO_CONCLUIU",       
  "TP_ESCOLA",              "TP_ENSINO",              "TP_DEPENDENCIA_ADM_ESC", "TP_PRESENCA_CN",        
  "TP_PRESENCA_CH",         "TP_PRESENCA_LC",         "NU_NOTA_CN",             "NU_NOTA_CH",            
  "NU_NOTA_LC",             "NU_NOTA_MT",             "TP_LINGUA",              "TP_STATUS_REDACAO",      
  "NU_NOTA_REDACAO",        "Q001",                   "Q002",                   "Q006",                  
  "Q024",                   "Q025",                   "Q026",                   "Q027",                  
  "Q047") 

# Tratamento basico
train_selected_col <- train %>% 
  select(all_of(colunas))
               
colunas <- c("NU_INSCRICAO")

test_selected <- test %>% 
  mutate(NU_NOTA_MT = NA) %>% 
  select(all_of(colunas))

df <- bind_rows(train_selected_col, test_selected)

set.seed(101)
index <- sample(seq(1:nrow(df)))

df_final <- df[index, names(df)]

openxlsx::write.xlsx(df_final, file = 'dados/enem/enem.xlsx', overwrite = T)



