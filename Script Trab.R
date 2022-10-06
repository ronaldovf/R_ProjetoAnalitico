#install.packages("readxl")

rm(list=ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
options(scipen = 99)


library(tidyverse)
library(readxl)

dir()
#Leitura dos dados
excel_sheets("Case Técnico - Data Operation - Service Delivery Database.xlsx")
df_dados_cadastrais = read_excel("Case Técnico - Data Operation - Service Delivery Database.xlsx", sheet = "DADOS CADASTRAIS")
df_tecnologias = read_excel("Case Técnico - Data Operation - Service Delivery Database.xlsx", sheet = "TECNOLOGIAS")
df_contatos = read_excel("Case Técnico - Data Operation - Service Delivery Database.xlsx", sheet = "CONTATOS")


#tratamento dos dados
df_dados_cadastrais$`NOME FANTASIA` = c(if_else(is.na(df_dados_cadastrais$`NOME FANTASIA`),paste('NOME FANTASIA ', substr(df_dados_cadastrais$EMPRESA_ID,12,16)),df_dados_cadastrais$`NOME FANTASIA`))
df_dados_cadastrais$`ENDERECO COMPLEMENTO` = c(if_else(is.na(df_dados_cadastrais$`ENDERECO COMPLEMENTO`),paste('COMPLEMENTO ', substr(df_dados_cadastrais$EMPRESA_ID,12,16)),df_dados_cadastrais$`ENDERECO COMPLEMENTO`))

#CONVERTENDO PARA NUMÉRICO
df_dados_cadastrais$`CAPITAL SOCIAL` = c(str_replace_all(df_dados_cadastrais$`CAPITAL SOCIAL`,"[.]", ""))
df_dados_cadastrais$`CAPITAL SOCIAL` = c(str_replace(df_dados_cadastrais$`CAPITAL SOCIAL`,"[,]", "."))
df_dados_cadastrais$`CAPITAL SOCIAL` = c(as.numeric(df_dados_cadastrais$`CAPITAL SOCIAL`))

df_dados_cadastrais$FATURAMENTO = c(str_replace_all(df_dados_cadastrais$FATURAMENTO,"[.]", ""))
df_dados_cadastrais$FATURAMENTO = c(str_replace(df_dados_cadastrais$FATURAMENTO,"[,]", "."))
df_dados_cadastrais$FATURAMENTO = c(as.numeric(df_dados_cadastrais$FATURAMENTO))

df_dados_cadastrais$FUNCIONARIOS = c(str_replace_all(df_dados_cadastrais$FUNCIONARIOS,"[.]", ""))
df_dados_cadastrais$FUNCIONARIOS = c(str_replace(df_dados_cadastrais$FUNCIONARIOS,"[,]", "."))
df_dados_cadastrais$FUNCIONARIOS = c(as.integer(df_dados_cadastrais$FUNCIONARIOS))

#substituindo os dados de capital social e faturamento pela média de todos
media_cap = mean(df_dados_cadastrais$`CAPITAL SOCIAL`, na.rm = TRUE)
df_dados_cadastrais$`CAPITAL SOCIAL` = c(if_else(is.na(df_dados_cadastrais$`CAPITAL SOCIAL`),media_cap,df_dados_cadastrais$`CAPITAL SOCIAL`))

media_fat = mean(df_dados_cadastrais$FATURAMENTO, na.rm = TRUE)
df_dados_cadastrais$FATURAMENTO = c(if_else(is.na(df_dados_cadastrais$FATURAMENTO),media_cap,df_dados_cadastrais$FATURAMENTO))

#Convertendo o campo data de abertura
df_dados_cadastrais$`DATA DE ABERTURA` = c(as.Date(df_dados_cadastrais$`DATA DE ABERTURA`, format = "%d/%m/%Y"))

#gerando o template final

df_final = df_dados_cadastrais %>% 
           inner_join(df_contatos, by = 'EMPRESA_ID')

df_final = df_final %>% 
           select(EMPRESA_ID, `RAZAO SOCIAL`, `NOME FANTASIA`, ENDERECO, `ENDERECO NUMERO`,`ENDERECO COMPLEMENTO`,BAIRRO, 
                  CIDADE, ESTADO, CEP, TAMANHO, `CAPITAL SOCIAL`,FATURAMENTO, FUNCIONARIOS, `DATA DE ABERTURA`, 
                  `TIPO DE UNIDADE`, SITUACAO, `ATIVIDADE PRINCIPAL`,`NATUREZA JURIDICA`, `TELEFONE 1`, `TELEFONE 2`,
                  `TELEFONE 3`, `TELEFONE 4`, `TELEFONE 5`, `WEBSITE 1`, `WEBSITE 2`, `WEBSITE 3`, `WEBSITE 4`, `WEBSITE 5`,
                  NOME, SOBRENOME, CARGO, `E-MAIL`)

#ajustando os campos numéricos por faixa

df_final$`CAPITAL SOCIAL` = c(case_when(
                              df_final$`CAPITAL SOCIAL` > 0 & df_final$`CAPITAL SOCIAL`<=50000 ~ "R$ 0 - R$ 50 K",
                              df_final$`CAPITAL SOCIAL` > 50000 & df_final$`CAPITAL SOCIAL`<=250000 ~ "R$ 51K - R$ 250 K",
                              df_final$`CAPITAL SOCIAL` > 250000 & df_final$`CAPITAL SOCIAL`<=500000 ~ "R$ 251 - R$ 500 K",
                              df_final$`CAPITAL SOCIAL` > 500000 & df_final$`CAPITAL SOCIAL`<=2500000 ~ "R$ 501 - R$ 2,5 M",
                              df_final$`CAPITAL SOCIAL` > 2500000 & df_final$`CAPITAL SOCIAL`<=10000000 ~ "R$ 2,5 M - R$ 10 M",
                              df_final$`CAPITAL SOCIAL` > 10000000 & df_final$`CAPITAL SOCIAL`<=50000000 ~ "R$ 10 M - R$ 50 M",
                              df_final$`CAPITAL SOCIAL` > 50000000 & df_final$`CAPITAL SOCIAL`<=100000000 ~ "R$ 50 M - R$ 100 M",
                              df_final$`CAPITAL SOCIAL` > 100000000 & df_final$`CAPITAL SOCIAL`<=250000000 ~ "R$ 100 M - R$ 250 M",
                              df_final$`CAPITAL SOCIAL` > 250000000 & df_final$`CAPITAL SOCIAL`<=500000000 ~ "R$ 250 M - R$ 500 M",
                              df_final$`CAPITAL SOCIAL` > 500000000 & df_final$`CAPITAL SOCIAL`<=1000000000 ~ "R$ 500 M - R$ 1 B",
                              df_final$`CAPITAL SOCIAL` > 1000000000 ~ "+ R$ 1 B"
                              ))



df_final$FUNCIONARIOS = c(case_when(
                            between(df_final$FUNCIONARIOS, 0, 3) ~ "0 - 3",
                            between(df_final$FUNCIONARIOS, 4, 10) ~ "4 - 10",
                            between(df_final$FUNCIONARIOS, 11, 50) ~ "11 - 50",
                            between(df_final$FUNCIONARIOS, 51, 200) ~ "51 - 200",
                            between(df_final$FUNCIONARIOS, 201, 500) ~ "201 - 500",
                            between(df_final$FUNCIONARIOS, 501, 1000) ~ "501 - 1000",
                            between(df_final$FUNCIONARIOS, 1001, 5000) ~ "1001 - 5000",
                            between(df_final$FUNCIONARIOS, 5001, 1000) ~ "5001 - 1000",
                          ))

df_final$FATURAMENTO = c(case_when(
  df_final$FATURAMENTO > 0 & df_final$FATURAMENTO<=50000 ~ "R$ 0 - R$ 50 K",
  df_final$FATURAMENTO > 50000 & df_final$FATURAMENTO<=250000 ~ "R$ 51K - R$ 250 K",
  df_final$FATURAMENTO > 250000 & df_final$FATURAMENTO<=500000 ~ "R$ 251 - R$ 500 K",
  df_final$FATURAMENTO > 500000 & df_final$FATURAMENTO<=2500000 ~ "R$ 501 - R$ 2,5 M",
  df_final$FATURAMENTO > 2500000 & df_final$FATURAMENTO<=10000000 ~ "R$ 2,5 M - R$ 10 M",
  df_final$FATURAMENTO > 10000000 & df_final$FATURAMENTO<=50000000 ~ "R$ 10 M - R$ 50 M",
  df_final$FATURAMENTO > 50000000 & df_final$FATURAMENTO<=100000000 ~ "R$ 50 M - R$ 100 M",
  df_final$FATURAMENTO > 100000000 & df_final$FATURAMENTO<=250000000 ~ "R$ 100 M - R$ 250 M",
  df_final$FATURAMENTO > 250000000 & df_final$FATURAMENTO<=500000000 ~ "R$ 250 M - R$ 500 M",
  df_final$FATURAMENTO > 500000000 & df_final$FATURAMENTO<=1000000000 ~ "R$ 500 M - R$ 1 B",
  df_final$FATURAMENTO > 1000000000 ~ "+ R$ 1 B"
))

#Qual a quantidade total de empresas?

df_final %>% 
select (EMPRESA_ID) %>% 
distinct() %>% 
summarize(qtd_empresas = n())

#2.	Quantos contatos existem na base?
df_contatos %>% 
  distinct() %>% 
  summarize(qtd_contatos = n())

#4.	Quantos websites únicos existem na base? 
df_web = df_final %>% select(`WEBSITE 1`, `WEBSITE 2`, `WEBSITE 3`, `WEBSITE 4`, `WEBSITE 5`) %>% 
  pivot_longer(
    cols = starts_with("WEBSITE"),=
    values_to = "WEB",
    values_drop_na = TRUE
  ) %>% 
  select (WEB) %>% 
  distinct()

df_web %>% summarize(qtd_websites = n())

#5.	Quantos e-mail existem na base?
df_final %>% 
  select(`E-MAIL`) %>% 
  distinct() %>% 
  summarize(qtd_emails = n())

df_final = df_final %>% separate(`DATA DE ABERTURA`, into = c("ANO", "MES", "DIA"), sep="-")

write_csv2(df_final, file="df_final.csv")
