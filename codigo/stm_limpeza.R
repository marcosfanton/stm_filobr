####Pacotes####
library(tidyverse)
library(here)
library(genderBR) # Dicionário de nomes e gênero
library(stringi)
library(textclean)

#Unificação dos bancos de 1987-2021####
#Todos os bancos foram baixados em CSV na página da CAPES - Dados Abertos 
#e armazenados na pasta dados/catalogo. Ao todo, são 36 arquivos com cerca de 4.43GB
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

#Variáveis dos Bancos de 1987-2012 - Ver Metadados do Catálogo
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
              "NumeroPaginas",
              "ResumoTese")

#Variáveis dos Bancos de 2013-2021 - Ver Metadados do Catálogo
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
              "NR_PAGINAS",
              "DS_RESUMO")

#Junção dos bancos com as 14 variáveis escolhidas 
catalogo8721 <- dplyr::bind_rows(
  banco8712 |> dplyr::select(all_of(vars8712)) |> 
    dplyr::rename_with(.cols = all_of(vars8712), 
                       ~vars1321[vars1321 != "NM_GRAU_ACADEMICO"]), # Essa variável não se encontra nos bancos anteriores a 2013
  banco1321) |> 
  dplyr::select(all_of(vars1321)) |> 
  dplyr::filter(CD_AREA_CONHECIMENTO == "70100004") #Inclusão apenas de trabalhos na área da filosofia

#Salvar arquivo RAW -- CSV 
catalogo8721 |>
  readr::write_csv("dados/catalogo_raw.csv")

# Limpeza do texto e padronização de variáveis
catalogo8721 <- catalogo8721  |> 
  dplyr::mutate(CD_PROGRAMA = as.factor(CD_PROGRAMA), # Torna variável como fator para não ser desconfigurada
                dplyr::across(where(is.character), ~ { # Modifica apenas variáveis do tipo character
                  x <- stringr::str_to_lower(.) # Padroniza todo texto em caixa baixa
                  x <- stringr::str_remove_all(x, "[[:punct:]]") # Remove pontuações
                  x <- stringr::str_remove_all(x, "[[:digit:]]") # Remove números
                  x <- stringi::stri_trans_general(x, "Latin-ASCII") # Transforma todo texto em Latin-ASCII ou expressões equivalente
                  x <- stringr::str_squish(x) # Remove espaços consecutivos
                  x
                  }),
                NM_GRAU_ACADEMICO = case_when( # Atribui titulação com base na variável nm_subtipo_producao
                  AN_BASE <= 2012 & NM_SUBTIPO_PRODUCAO == "mestrado" ~ "mestrado",
                  AN_BASE <= 2012 & NM_SUBTIPO_PRODUCAO == "doutorado" ~ "doutorado",
                  TRUE ~ as.character(NM_GRAU_ACADEMICO)
                ),
                NM_ENTIDADE_ENSINO = str_replace_all(NM_ENTIDADE_ENSINO, 
                                                     pattern = c("universidade federal da paraiba.*" = "universidade federal da paraiba",
                                                                 "universidade estadual do parana reitoria" = "universidade estadual do parana",
                                                                 ".*mesquita.*" = "universidade estadual paulista"))) |> 
  dplyr::rename_all(tolower)


#Variáveis derivadas####
#Gênero de orientadores, orientandos e orientador-orientando com o pacote @GenderBR####
catalogo8721 <- catalogo8721  |>  
  mutate(
    g_orientador = genderBR::get_gender(nm_orientador),
    g_discente = genderBR::get_gender(nm_discente)
  )

#O pacote genderBR não consegue atribuir gênero a alguns nomes de orientadores. 
#Para não enviesar a amostra (excluir todos trabalhos orientados por determinador professor),
#iremos atribuir o gênero manualmente para as observações que não foram definidas.

#Seleção dos nomes sem gênero atribuído - n: 349
semgenero <- catalogo8721 |>  
  filter(!is.na(nm_orientador)) |> # Filtra NAs do nome do orientador
  filter(is.na(g_orientador)) |> #Filtra apenas observações sem atribuição de gênero
  distinct(nm_orientador)   # Mantêm apenas os nomes únicos

#Vetor para mulheres
mulheres <- c("heleieth iara saffioti",
              "olgaria chain feres matos",
              "nelci do nascimento goncalves",
              "jeannemarie gagnebin de bons",
              "acylene maria cabral ferreira",
              "jeannemarie gagnebindebons",
              "jeannemarie gagnebindebons"
              )
#Vetor para homens
homens <- semgenero %>%
  filter(!nm_orientador %in% mulheres) |> # Filtra os nomes de mulheres
  pull() # Extrai vetor de nomes

#Atribuição de gênero para os casos correspondentes no restante do banco
catalogo8721 <- catalogo8721 |> 
  mutate(
    g_orientador = case_when(
    nm_orientador %in% mulheres ~ "Female", 
    nm_orientador %in% homens ~ "Male", 
    TRUE ~ g_orientador # Preserva os valores correspondentes nos demais casos
  ))

# Criação da variável gênero de orientador-orientando
catalogo8721 <- catalogo8721 |> 
  mutate(g_oridis = factor(case_when(
  g_orientador == "Male" & g_discente == "Male" ~ "MM",
  g_orientador == "Male" & g_discente == "Female" ~ "MF",
  g_orientador == "Female" & g_discente == "Male" ~ "FM",
  g_orientador == "Female" & g_discente == "Female" ~ "FF")
  ))

#Banco limpo####
#Salvar banco limpo
catalogo8721 |>
  readr::write_csv("dados/catalogo.csv")
