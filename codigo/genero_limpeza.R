####Pacotes####
library(tidyverse)
library(here) 
library(stringi) # Limpeza de texto
library(genderBR) # Dicionário de gênero com base no primeiro nome
library(textclean)

# Unificação dos bancos de 1987-2021####
#Todos os bancos foram baixados em CSV na página da CAPES - Dados Abertos 
#e armazenados na pasta dados/catalogo. Ao todo, são 36 arquivos com cerca de 4.43GB
# Bancos de 1987-2012
banco8712 <- purrr::map_dfr(list.files(path = "dados/catalogo/capes8712", 
                                       pattern = "dados", 
                                       full.names = TRUE),
                            readr::read_csv2, 
                            locale = readr::locale(encoding = "ISO-8859-1"),
                            show_col_types = FALSE)
# Bancos de 2013-2021
banco1321 <- purrr::map_dfr(list.files(path = "dados/catalogo/capes1321", 
                                       pattern = "capes", 
                                       full.names = TRUE),
                            readr::read_csv2, 
                            locale = readr::locale(encoding = "ISO-8859-1"),
                            na = c("NI", "NA"),
                            show_col_types = FALSE) 

# Variáveis dos Bancos de 1987-2012 - Ver Metadados do Catálogo
vars8712 <- c("AnoBase",
              "NomeIes",
              "GrandeAreaDescricao",
              "AreaConhecimento",
              "AreaAvaliacao",
              "CodigoPrograma",
              "TituloTese",
              "Nivel", 
              "PalavrasChave",
              "Autor",
              "Orientador_1",
              "Regiao",
              "Uf",
              "AreaConhecimentoCodigo", 
              "NumeroPaginas")

# Variáveis dos Bancos de 2013-2021 - Ver Metadados do Catálogo
vars1321 <- c("AN_BASE",
              "NM_ENTIDADE_ENSINO",
              "NM_GRANDE_AREA_CONHECIMENTO",
              "NM_AREA_CONHECIMENTO",
              "NM_AREA_AVALIACAO",
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
              "NR_PAGINAS")

# Junção dos bancos com as 13 variáveis escolhidas + 1991####
catalogo9121 <- dplyr::bind_rows(
  banco8712 |> dplyr::select(all_of(vars8712)) |> 
    dplyr::rename_with(.cols = all_of(vars8712), 
                       ~vars1321[vars1321 != "NM_GRAU_ACADEMICO"]), # Essa variável não se encontra nos bancos anteriores a 2013
  banco1321) |> 
  dplyr::select(all_of(vars1321)) |> 
  dplyr::filter(AN_BASE >= 1991)  # Inclusão apenas de trabalhos a partir de 1991

# Limpeza do texto e padronização de variáveis####
catalogo9121 <- catalogo9121  |> 
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
                    AN_BASE <= 2012 & NM_SUBTIPO_PRODUCAO == "profissionalizante" ~ "mestrado profissional",
                    TRUE ~ as.character(NM_GRAU_ACADEMICO)
                  )) |> 
  dplyr::rename_all(tolower)

# Variáveis derivadas####
# Gênero de orientadores, orientandos e orientador-orientando com o pacote @GenderBR####
catalogo9121 <- catalogo9121  |>  
  dplyr::mutate(
    g_orientador = genderBR::get_gender(nm_orientador),
    g_discente = genderBR::get_gender(nm_discente),
    g_oridis = factor(case_when(
      g_orientador == "Male" & g_discente == "Male" ~ "MM",
      g_orientador == "Male" & g_discente == "Female" ~ "MF",
      g_orientador == "Female" & g_discente == "Male" ~ "FM",
      g_orientador == "Female" & g_discente == "Female" ~ "FF")
    ))

# Exclui NA's de variáveis da análise
catalogo9121 <- catalogo9121 |> 
  filter(nm_grau_academico != "doutorado profissional") |>  # Exclusão de 38 observações
  filter(nm_grande_area_conhecimento != "") |>  # Exclui 1 observação com fator em branco
  drop_na(nm_grau_academico, 
          g_oridis) # Automaticamente exclui NAs de g_orientador e g_discente (104324|0.924)


# Banco limpo####
# Salvar arquivo RAW -- CSV 
catalogo9121 |>
  readr::write_csv("dados/catalogo/catalogo9121_raw.csv")
