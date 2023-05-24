####Pacotes####
library(tidyverse)
library(here) 
library(stringi) #limpeza de texto
library(genderBR) #rotulação de gênero com base no primeiro nome

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
                       ~vars1321[vars1321 != "NM_GRAU_ACADEMICO"]), #Essa variável não se encontra nos bancos anteriores a 2013
  banco1321) |> 
  dplyr::select(all_of(vars1321)) |> 
  dplyr::filter(CD_AREA_CONHECIMENTO == "70100004") #Inclusão apenas de trabalhos na área da filosofia

#Salvar arquivo RAW -- CSV 
catalogo8721 |>
  readr::write_csv("dados/catalogo_raw.csv")

#Limpeza do texto e padronização de variáveis
catalogo8721 <- catalogo8721  |> 
  dplyr::mutate(CD_PROGRAMA = as.factor(CD_PROGRAMA), # Torna variável como fator para não ser desconfigurada
                dplyr::across(where(is.character), ~ { # Modifica apenas variáveis do tipo character
             x <- stringr::str_to_lower(.) # Padroniza todo texto em caixa baixa
             x <- stringi::stri_trans_general(x, "Latin-ASCII") # Transforma todo texto em Latin-ASCII ou expressões equivalente
             x <- stringr::str_remove_all(x, "[[:punct:]]") # Remove pontuações
             x <- stringr::str_remove_all(x, "[[:digit:]]") # Remove números
             x <- stringr::str_squish(x) # Remove espaços consecutivos
             x
           }),
           NM_SUBTIPO_PRODUCAO = str_replace_all(NM_SUBTIPO_PRODUCAO, # Padronização dos valores das variáveis
                                        pattern = c("mestrado" = "dissertacao", 
                                                    "doutorado" = "tese")),
           NM_ENTIDADE_ENSINO = str_replace_all(NM_ENTIDADE_ENSINO, 
                                       pattern = c("universidade federal da paraiba.*" = "universidade federal da paraiba",
                                                   "universidade estadual do parana reitoria" = "universidade estadual do parana",
                                                   ".*mesquita.*" = "universidade estadual paulista")))

#Variáveis derivadas####
#Gênero de orientadores, orientandos e orientador-orientando com o pacote @GenderBR####
catalogo8721 <- catalogo8721 %>% 
  mutate(
    G_ORIENTADOR = genderBR::get_gender(NM_ORIENTADOR),
    G_DISCENTE = genderBR::get_gender(NM_DISCENTE), 
    G_ORDIS = factor(case_when(
      G_ORIENTADOR == "Male" & G_DISCENTE == "Male" ~ "MM",
      G_ORIENTADOR == "Male" & G_DISCENTE == "Female" ~ "MF",
      G_ORIENTADOR == "Female" & G_DISCENTE == "Male" ~ "FM",
      G_ORIENTADOR == "Female" & G_DISCENTE == "Female" ~ "FF")
    ))

#Banco limpo####
#Transforma o restante das variáveis desejadas em fatores
fatores <- c("NM_ENTIDADE_ENSINO",  
             "NM_SUBTIPO_PRODUCAO", 
             "NM_REGIAO", 
             "SG_UF_IES", 
             "G_ORIENTADOR", 
             "G_DISCENTE")
catalogo8721 <- catalogo8721  |>  
  mutate_at(fatores, factor) 

#Salvar banco limpo
catalogo8721 |>
  readr::write_csv("dados/catalogo.csv")
#Gênero em orientadores e orientandos com o #GenderBR####
catalogo <- catalogo |> 
  mutate(
    gorientador = get_gender(orientador),
    galuno = get_gender(aluno), 
    goga = factor(case_when(
      (gorientador == "Male" & galuno == "Male") ~ "Homem/Homem",
      (gorientador == "Male" & galuno == "Female") ~ "Homem/Mulher",
      (gorientador == "Female" & galuno == "Male") ~ "Mulher/Homem",
      (gorientador == "Female" & galuno == "Female") ~ "Mulher/Mulher"),
      levels = c("Mulher/Mulher", "Mulher/Homem", "Homem/Mulher", "Homem/Homem"))
  )
#Transforma em fatores variáveis selecionadas
fatores <- c("nomeies", 
             "nivel", 
             "regiao", 
             "uf", 
             "galuno", 
             "gorientador", 
             "goga")
catalogo <- catalogo  |>  
  mutate_at(fatores, factor) 
#Salvar banco
write_csv(catalogo, file = "23-05_catalogofilosofia8721.csv")