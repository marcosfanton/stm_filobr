# Variáveis dos Bancos de 1987-2012 - Ver Metadados do Catálogo
vars8712 <- c("AnoBase",
              "AreaAvaliacao",
              "TituloTese",
              "PalavrasChave",
              "Autor",
              "Orientador_1",
              "ResumoTese")

# Variáveis dos Bancos de 2013-2021 - Ver Metadados do Catálogo
vars1321 <- c("AN_BASE",
              "NM_AREA_AVALIACAO",
              "NM_PRODUCAO",
              "DS_PALAVRA_CHAVE",
              "NM_DISCENTE",
              "NM_ORIENTADOR",
              "DS_RESUMO")

# Junção dos bancos com as 13 variáveis escolhidas + 1991####
catalogo9121 <- dplyr::bind_rows(
  banco8712 |> dplyr::select(all_of(vars8712)) |> 
    dplyr::rename_with(.cols = all_of(vars8712), 
                       ~vars1321[vars1321 != "NM_GRAU_ACADEMICO"]), # Essa variável não se encontra nos bancos anteriores a 2013
  banco1321) |> 
  dplyr::select(all_of(vars1321)) 

catalogo9121 <- catalogo9121 |> 
  mutate(across(where(is.character), ~ { # Modifica apenas variáveis do tipo character
                x <- stringr::str_to_lower(.) # Padroniza todo texto em caixa baixa
                x <- stringr::str_remove_all(x, "[[:punct:]]") # Remove pontuações
                x <- stringr::str_remove_all(x, "[[:digit:]]") # Remove números
                x <- stringi::stri_trans_general(x, "Latin-ASCII") # Transforma todo texto em Latin-ASCII ou expressões equivalente
                x <- stringr::str_squish(x) # Remove espaços consecutivos
                x
              }))

abstract <- catalogo9121 |> 
  filter(str_detect(DS_RESUMO, "\\blogic"))

abstract |>
  readr::write_csv("dados/catalogo/teses_abstract-logic.csv")

keywords <- catalogo9121 |> 
  filter(str_detect(DS_PALAVRA_CHAVE, "\\blogic"))
keywords |>
  readr::write_csv("dados/catalogo/teses_keywords-logic.csv")

title <- catalogo9121 |> 
  filter(str_detect(NM_PRODUCAO, "\\blogic"))
title |>
  readr::write_csv("dados/catalogo/teses_title-logic.csv")



clean_string <- function(input_string) {
  cleaned_string <- input_string %>%
    str_to_lower() %>%
    stri_trans_general("latin-ascii") %>%
    stri_replace_all_regex("[0-9]", "") %>%
    str_squish()
  return(cleaned_string)
}


catalogo9121 <- catalogo9121 %>%
  mutate(
    DS_PALAVRA_CHAVE = clean_string(DS_PALAVRA_CHAVE),
    DS_RESUMO = clean_string(DS_RESUMO),
    NM_PRODUCAO = clean_string(NM_PRODUCAO)
  )
