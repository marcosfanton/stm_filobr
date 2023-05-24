####Pacotes####
library(tidyverse)
library(here)
library(tidytext) # Manipulação de texto
library(stringi) # Manipulação de texto
library(textcat) # Detecção de resumos em outros idiomas
library(stm) # Structural topic model
library(furrr) # Rodar múltiplos modelos 
library(tidystm) # Extração de efeitos do modelo
library(MetBrewer) # Paleta de cores
library(scales) # Uso de porcentagem em gráficos

# Importação do banco limpo em "limpeza_catalogo.R"
dados <- read.csv("dados/catalogo.csv")

#Preparação do banco para STM####
#Transforma as variáveis desejadas em fatores
fatores <- c("NM_ENTIDADE_ENSINO",
             "NM_GRAU_ACADEMICO",
             "NM_SUBTIPO_PRODUCAO", 
             "NM_REGIAO", 
             "SG_UF_IES", 
             "G_ORIENTADOR", 
             "G_DISCENTE")

#Filtragem de observações com base na qualidade dos resumos e seleção de variáveis
teste <- dados |> #Banco com total de trabalhos por Área de Conhecimento Filosofia (N = 12525)
  mutate_at(fatores, factor) |> 
  mutate(lang = textcat::textcat(DS_RESUMO)) |> 
  dplyr::filter(
    stringi::stri_count_words(dados$DS_RESUMO) > 15, # -172 trabalhos com resumos insuficientes (n = 12353)
    is.na(NM_GRAU_ACADEMICO) | NM_GRAU_ACADEMICO != "mestrado profissional", #-356 trabalhos do mestrado acadêmico (n = 11997)
    lang == "portuguese", # -70 trabalhos com resumos em outros idiomas (n = 11927)
    !is.na(G_ORIENTADOR), # -529 trabalhos sem identificação de gênero do orientador (n = 11398)
    !is.na(NM_PRODUCAO)) |> 
  dplyr::distinct(NM_PRODUCAO, DS_RESUMO, .keep_all = TRUE) |> 
  dplyr::select(AN_BASE, NM_PRODUCAO, DS_RESUMO, DS_PALAVRA_CHAVE, G_ORIENTADOR) #Seleção de variáveis para STM
 


teste <- dados |> #Banco com total de trabalhos por Área de Conhecimento Filosofia (N = 12525)
  mutate_at(fatores, factor) |> 
  mutate(lang = textcat::textcat(DS_RESUMO)) 

teste1 <- teste |> filter(stri_count_words(dados$DS_RESUMO) > 15) 

teste2 <- teste1 |> filter(is.na(NM_GRAU_ACADEMICO) | NM_GRAU_ACADEMICO != "mestrado profissional") 

teste3 <- teste2 |> filter(lang == "portuguese")

teste4 <- teste3 |> filter(is.na(G_ORIENTADOR))



teste1 <- teste |>  
  filter(!is.na(G_ORIENTADOR))

 

# Banco com total de trabalhos (n = 12525)
filter(stri_count_words(dados$resumo) > 15) |> #Eliminação de 172 resumos (n = 12353)
  mutate(lang = textcat(resumo)) |> 
  filter(lang == "portuguese") |> #Eliminação de 73 resumos em outros idiomas (n = 12280)
  distinct(titulo, resumo, .keep_all = TRUE) |> #Eliminação de 8 trabalhos duplicados (n = 12272)
  mutate(gorientador = as.factor(gorientador)) |>
  drop_na(titulo) |>   #Eliminação de 1 trabalho sem título (n = 12271)
  drop_na(gorientador) |>  #Eliminação de 492 trabalhos sem identificação de gênero (n = 11779)
  filter(is.na(grauacademico) | grauacademico != "mestrado profissional") |> #354 trabalhos de mestrado profissional (n = 11425)
  select(doc_id, titulo, resumo, ano, pchaves, gorientador)

