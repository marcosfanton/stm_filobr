####Pacotes####
library(tidyverse)
library(here) 
library(stringi) #limpeza de texto
library(textclean) #limpeza de texto
library(genderBR) #rotulação de gênero com base no primeiro nome

#Unificação dos bancos de 1987-2021####
#Todos os bancos foram baixados em .csv na página da CAPES - Dados Abertos 
#e armazenados na pasta dados/catalogo. Ao todo, são 36 arquivos com cerca de 4.43GB
#Importação dos arquivos .csvs
#Bancos de 1987-2012
banco8712 <- purrr::map_dfr(list.files(path = "dados/catalogo/capes8712", 
                                       pattern = "dados", 
                                       full.names = TRUE),
                            readr::read_csv2, 
                            locale = readr::locale(encoding = "ISO-8859-1"),
                            show_col_types = FALSE)
#Bancos de 2013-2021
banco1321 <- purrr::map_dfr(list.files(path = "dados/catalogo/capes1321", 
                                       pattern = "capes", 
                                       full.names = TRUE),
                            readr::read_csv2, 
                            locale = readr::locale(encoding = "ISO-8859-1"),
                            na = c("NI", "NA"),
                            show_col_types = FALSE) 

#Padronização e seleção das variáveis de cada banco para formação de banco único
#Bancos de 1987-2012 - Ver Metadados do Catálogo
vars8712 <- c("AnoBase",
              "NomeIes",
              "CodigoPrograma",
              "TituloTese",
              "Nivel", 
              "PalavrasChave",
              "Autor",
              "Orientador_1",
              "Regiao",
              "Uf",
              "AreaConhecimentoCodigo", 
              "GrandeAreaDescricao",
              "NumeroPaginas",
              "ResumoTese")

#Bancos de 2013-2021 - Ver Metadados do Catálogo
vars1321 <- c("AN_BASE",
              "NM_ENTIDADE_ENSINO", 
              "CD_PROGRAMA",
              "NM_PRODUCAO", 
              "NM_SUBTIPO_PRODUCAO", 
              "NM_GRAU_ACADEMICO",
              "DS_PALAVRA_CHAVE",
              "NM_DISCENTE",
              "NM_ORIENTADOR",
              "NM_REGIAO",
              "SG_UF_IES",
              "CD_AREA_CONHECIMENTO",   
              "NM_GRANDE_AREA_CONHECIMENTO", 
              "NR_PAGINAS",
              "DS_RESUMO")

#Junção dos bancos com as 15 variáveis escolhidas 
catalogo8721 <- bind_rows(
  banco8712 |> select(all_of(vars8712)) |> 
    rename_with(.cols = all_of(vars8712), ~vars1321[vars1321 != "NM_GRAU_ACADEMICO"]), #Essa variável não se encontra nos bancos anteriores a 2013
  banco1321) |> 
  select(all_of(vars1321))
