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

# Filtragem de observações com base na qualidade dos resumos e seleção de variáveis ####
# Importação do banco limpo em "limpeza_catalogo.R"
dados <- read.csv("dados/catalogo.csv")

dados <- dados |> #Banco com total de trabalhos por Área de Conhecimento Filosofia (N = 12525)
  dplyr::mutate(g_orientador = as.factor(g_orientador), # Tranforma em fator variável gênero
    lang = textcat::textcat(ds_resumo)) |> # Cria identificar de idioma com base nos resumos
  dplyr::filter(
    stringi::stri_count_words(dados$ds_resumo) > 15, # -172 trabalhos com resumos insuficientes (n = 12353)
    is.na(nm_grau_academico) | nm_grau_academico != "mestrado profissional", #-356 trabalhos do mestrado acadêmico (n = 11997)
    lang == "portuguese", # -70 trabalhos com resumos em outros idiomas (n = 11927)
    !is.na(g_orientador), # -184 trabalhos sem identificação de gênero do orientador (n = 11743)
    !is.na(nm_producao)) |> # -1 trabalho sem título (n = 11742)
  dplyr::distinct(nm_producao, ds_resumo, .keep_all = TRUE) |> # -6 trabalhos repetidos (n = 11736)
  dplyr::mutate(doc_id = row_number()) |> 
  dplyr::select(doc_id, an_base, nm_producao, ds_resumo, ds_palavra_chave, g_orientador) # Seleção de variáveis para STM e outras análises

# ngrams e stopwords ####
# Para não poluir esse script, a análise de ngrams e de stopwords foi realizada em script separado.
# Ver filograms.R 
# Incorporação de ngrams relevantes na variável ds_resumo
filongrams <- readr::read_lines("dados/filongrams")
filograms <- str_replace_all(filongrams, " ", "")
names(filograms) <- c(filongrams)
dados <- dados |> 
  mutate(ds_resumo = str_replace_all(ds_resumo, pattern = filograms)) #substitui expressões compostas

# Salvar banco com amostra final para testes com stopwords personalizadas
dados |> 
  readr::write_csv("dados/dados_pre-stm.csv")

# Stopwords personalizada da filosofia ####
filolixo <- readr::read_lines("dados/filolixo")
# Transformação em tibble para uso no anti_join####
filolixo <- tibble(word = unlist(str_split(filolixo, "\n")))

# Preparação do banco - Tokenização e exclusão de stopwords (n: 11736)
filowords <- dados |> 
  tidytext::unnest_tokens(output = word, # Tokenização de palavras do resumo
                          input = ds_resumo, 
                          drop = TRUE) |> # Exclusão da variável DS_RESUMO
  anti_join(get_stopwords("pt"))|> # Stopwords em pt
  anti_join(get_stopwords("en"))|> 
  anti_join(filolixo) |> # Dicionário de stopwords personalizado no script filograms.R <===
  filter(str_detect(word, "^si$|^fe$|...")) |> #remove todas palavras com menos de 3 caracteres (mantém 'si' e 'fe')
  count(doc_id, nm_producao, word, an_base, g_orientador)    # Contagem da frequência absoluta de cada token

# Preparação do banco em matriz esparsa
filosparse <- filowords |> tidytext::cast_sparse(doc_id, word, n) #matriz para análise
# Preparação das covariáveis para análise
covars <- dplyr::distinct(filowords, doc_id, an_base, g_orientador) #matriz de covariáveis

# Modelo STM
#Modelo simples####
topic_model <- stm(filosparse,
                   K = 70,
                   prevalence = ~ g_orientador + s(an_base),
                   seed = 1987,
                   data = covars,
                   init.type = "Spectral")

# Extração de matrizes
# Beta
tidybeta <- tidy(topic_model) 
# Gamma
tidygamma <- tidy(topic_model, matrix = "gamma",
                 document_names = rownames(filosparse))

# Matriz Beta - Palavras mais frequentes de cada tópico 
top_words <- tidybeta %>%
  arrange(desc(beta)) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  summarise(terms = paste(term, collapse = ", ")) %>%
  ungroup()

# Matriz Gamma - Prevalência de tópicos com respectivos termos
gamma_words <- tidygamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_words, by = "topic") %>%
  mutate(topic = paste0("T", topic),
         topic = reorder(topic, gamma))


gamma_words |> 
  top_n(100, gamma) |> 
  ggplot(aes(topic, gamma, label = terms, fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = 0.0001, size = 5) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.05),
                     labels = percent_format()) +
  scale_fill_manual(values = met.brewer("Cross", 100)) + 
  theme_classic() +
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 12),
        text = element_text(size = 2)) +
  labs(x = NULL, y = expression(gamma),
       title = "60 Tópicos de Teses e Dissertações de Filosofia (1987-2021) (n: 11736) sem a exclusão de stopwords personalizadas",
       subtitle = "Elaboração: Os autores | Dados: Catálogo de Teses e Dissertações (CAPES)") 


# Findthoughts (STM) #### 
td_gamma$document <- as.integer(td_gamma$document)

findallthoughts <- td_gamma |> 
  mutate(document = as.integer(document)) |> 
  left_join(dados,
            by = c("document" = "doc_id")) |> # Unifica o banco dados com a matrix gamma
  group_by(document) |> # Agrupa os valores gamma de cada tópico
  slice_max(order_by = gamma, n = 1) |> # Escolhe o tópico com maior gamma de cada documento
  select(document, topic, nm_producao, ds_resumo, gamma) # Seleciona apenas as variáveis de interesse
  

findthoughts <- td_gamma |> 
  mutate(document = as.integer(document)) |> 
  left_join(dados,
            by = c("document" = "doc_id")) |> # Unifica o banco dados com a matrix gamma 
  group_by(topic) |> 
  slice_max(order_by = gamma, n = 5) |> # Encontra os docs mais representativo de cada tópico (com maior gamma)
  select(document, topic, nm_producao, ds_resumo, gamma) # Seleciona apenas as variáveis de interesse


