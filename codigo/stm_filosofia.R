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
#Filtragem de observações com base na qualidade dos resumos e seleção de variáveis
dados <- dados |> #Banco com total de trabalhos por Área de Conhecimento Filosofia (N = 12525)
  mutate(G_ORIENTADOR = as.factor(G_ORIENTADOR), # Tranforma em fator variável gênero
    lang = textcat::textcat(DS_RESUMO)) |> # Cria identificar de idioma com base nos resumos
  dplyr::filter(
    stringi::stri_count_words(dados$DS_RESUMO) > 15, # -172 trabalhos com resumos insuficientes (n = 12353)
    is.na(NM_GRAU_ACADEMICO) | NM_GRAU_ACADEMICO != "mestrado profissional", #-356 trabalhos do mestrado acadêmico (n = 11997)
    lang == "portuguese", # -70 trabalhos com resumos em outros idiomas (n = 11927)
    !is.na(G_ORIENTADOR), # -184 trabalhos sem identificação de gênero do orientador (n = 11743)
    !is.na(NM_PRODUCAO)) |> # -1 trabalho sem título (n = 11742)
  dplyr::distinct(NM_PRODUCAO, DS_RESUMO, .keep_all = TRUE) |> # -6 trabalhos repetidos (n = 11736)
  dplyr::select(AN_BASE, NM_PRODUCAO, DS_RESUMO, DS_PALAVRA_CHAVE, G_ORIENTADOR) #Seleção de variáveis para STM e outras análises

#Para não poluir esse arquivo, a análise de ngrams e de stopwords foi realizada em arquivos separados:
#ngrams --> Ver filograms.R
#stopwords --> filolixo.R

# Incorporação de ngrams
#Substituição dos ngrams na variável DS_RESUMO##
filograms <- str_replace_all(filongrams, " ", "")
names(filograms) <- c(filongrams)

dados <- dados |> 
  mutate(DS_RESUMO = str_replace_all(DS_RESUMO, pattern = filograms)) #substitui expressões compostas


