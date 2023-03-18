setwd("~/Tutoriais - R")
rm(list = ls())

library(tidyverse)


rh <- read.csv('dados/HR-Employee-Attrition.csv')


df1 <- data.frame(employee = c(LETTERS[1:5], "A"), 
                  area = c("Contabil", "Engenharia", "Engenharia", "RH", "RH", "Engenharia"),
                  sobrenome = letters[1:6])

df2 <- data.frame(employee = c("B", "A", "D", "C", "A"), 
                  contratacao = c(2004, 2015, 2020, 2002, 1999),
                  surname = c("b", "a", "d", "c", "a"))



nrow(df2)

df3 <- df2 %>% 
  distinct(employee, surname, .keep_all = T)

anti_join(df2, df3, by = c("employee", "surname"))


inner_join(df1, df2, by = c("employee", "sobrenome" = "surname"))


SELECT *
FROM df1
JOIN df2 ON df1.employee = df2.employee AND df1.sobrenome = df2.surname

