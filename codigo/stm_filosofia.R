####Pacotes####
library(tidyverse)
library(here)
library(tidytext) # Manipulação de texto
library(textcat) # Detecção de resumos em outros idiomas
library(stopwords) # Dicionário de stopwords
library(stm) # Structural topic model
library(furrr) # Rodar múltiplos modelos 
library(tidystm) # Extração de efeitos do modelo
library(MetBrewer) # Paleta de cores
library(scales) # Uso de porcentagem em gráficos

# Importação do banco limpo em "limpeza_catalogo.R"
dados <- read.csv("dados/catalogo.csv")
