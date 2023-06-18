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
library(embed) # UMAP
library(umap) # UMAP
library(recipes) # UMAP

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
dados <- read.csv("dados/dados_pre-stm.csv")
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
                   K = 76,
                   prevalence = ~ g_orientador + s(an_base),
                   seed = 1987,
                   data = covars,
                   init.type = "Spectral")

# Extração de matrizes
# Beta
tidybeta <- tidytext::tidy(topic_model) 
# Gamma
tidygamma <- tidytext::tidy(topic_model, matrix = "gamma",
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
        text = element_text(size = 1)) +
  labs(x = NULL, y = expression(gamma),
       title = "76 Tópicos de Teses e Dissertações de Filosofia (1987-2021) (n: 11736) sem a exclusão de stopwords personalizadas",
       subtitle = "Elaboração: Os autores | Dados: Catálogo de Teses e Dissertações (CAPES)") 


# Findthoughts (STM) #### 

findallthoughts <- tidygamma |> 
  mutate(document = as.integer(document)) |> 
  left_join(dados,
            by = c("document" = "doc_id")) |> # Unifica o banco dados com a matrix gamma
  group_by(document) |> # Agrupa os valores gamma de cada tópico
  slice_max(order_by = gamma, n = 1) |> # Escolhe o tópico com maior gamma de cada documento
  select(document, topic, nm_producao, ds_resumo, gamma) # Seleciona apenas as variáveis de interesse
  
findthoughts <- tidygamma |> 
  mutate(document = as.integer(document)) |> 
  left_join(dados,
            by = c("document" = "doc_id")) |> # Unifica o banco dados com a matrix gamma 
  group_by(topic) |> 
  slice_max(order_by = gamma, n = 5) |> # Encontra os docs mais representativo de cada tópico (com maior gamma)
  select(document, topic, nm_producao, ds_resumo, gamma) # Seleciona apenas as variáveis de interesse

#Efeitos#### 
#Efeito ano####
stm_prep_ano <- stm::estimateEffect(1:76 ~ an_base, 
                               stmobj = topic_model, 
                               metadata = covars, 
                               uncertainty = "Global")

stm_prep_ano_tidy <- tidytext::tidy(stm_prep_ano)

sig_effects_ano_tidy <- tidystm::extract.estimateEffect(x = stm_prep_ano, 
                                               covariate = "an_base", 
                                               model = topic_model, 
                                               method = "pointestimate",
                                               labeltype = "prob",
                                               n = 3)

#Gráfico ano
ggplot(sig_effects_ano_tidy, aes(x = covariate.value, y = estimate,
                                 ymin = ci.lower, ymax = ci.upper)) +
  facet_wrap(~label) +
  geom_ribbon(alpha = .5, fill = "blue") +
  geom_line() +
  labs(x = "Ano",
       y = "Proporção de Tópico Esperada",
       fill = "Treated (0/1)") +
  theme(legend.position = "none")

#Efeito gênero de orientador####
stm_prep_gender <- stm::estimateEffect(1:75 ~ g_orientador, 
                                  stmobj = topic_model, 
                                  metadata = covars, 
                                  uncertainty = "Global")
stm_prep_gender_tidy <- tidytext::tidy(stm_prep_gender)

#Efeitos orientador
sig_effects_gender_tidy <- tidystm::extract.estimateEffect(x = stm_prep_gender, 
                                                  covariate = "g_orientador", 
                                                  model = topic_model, 
                                                  method = "pointestimate",
                                                  labeltype = "prob",
                                                  n = 3)
#Gráfico gênero
ggplot(sig_effects_gender_tidy, aes(x = covariate.value, 
                                    y = estimate,
                                    group = covariate)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper), width = 0.1) + 
  facet_wrap(~label, labeller = labeller(label = label_wrap_gen(width = 60))) +
  labs(x = "Gênero",
       y = "Estimativa") +
  theme(legend.position = "none")


#Modelos Múltiplos (código de Julia Silge)####
#Modelo com múltiplos K####
plan(multisession)
many_models <- tidyr::tibble(K = c(70, 72, 73, 74,75, 76, 77, 78, 79, 80)) |> #Teste de modelos com 40 a 80 Tópicos
  dplyr::mutate(topic_model = furrr::future_map(K, ~ stm::stm(filosparse, 
                                                              K = .,
                                                              prevalence = ~g_orientador + s(an_base),
                                                              seed = 1987,
                                                              data = covars,
                                                              init.type = "Spectral"),
                                                .options = furrr_options(seed = TRUE)))
#Gráficos de diagnóstico dos modelos
heldout <- stm::make.heldout(filosparse) 
k_result <- many_models |> # Cria banco com resultados de cada modelo
  dplyr::mutate(exclusivity = purrr::map(topic_model, exclusivity),
                semantic_coherence = purrr::map(topic_model, semanticCoherence, filosparse),
                eval_heldout = purrr::map(topic_model, eval.heldout, heldout$missing),
                residual = purrr::map(topic_model, checkResiduals, filosparse),
                bound =  purrr::map_dbl(topic_model, function(x) max(x$convergence$bound)),
                lfact = purrr::map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
                lbound = bound + lfact,
                iterations = purrr::map_dbl(topic_model, function(x) length(x$convergence$bound)))

#Critérios de avaliação de modelos | Held-out Likelihood, Coerência Semântica, Resíduos e Lower Bound ####
theme_set(theme_minimal(base_family = "Roboto"))

k_result |>  
  transmute(K,
            `Limite Inferior` = lbound,
            Resíduos = map_dbl(residual, "dispersion"),
            `Coerência Semântica` = map_dbl(semantic_coherence, mean),
            `Verossimilhanca retida (held-out likelihood)` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) |> 
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(linewidth = 1.5, alpha = 0.9, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (número de Tópicos)",
       y = NULL,
       title = "Diagnóstico do número de tópicos (K) para o modelo",
       subtitle = "O intervalo entre 60 e 80 tópicos parece ser o mais apropriado | Elaboração: Os autores")

#Gráfico Exclusividade x Coerência Semântica por tópicos
k_result |>  
  select(K, exclusivity, semantic_coherence)  |> 
  filter(K %in% c(70, 72, 73, 74,75, 76, 77, 78, 79, 80))  |> 
  unnest(cols = c(exclusivity, semantic_coherence))  |> 
  mutate(K = as.factor(K)) |> 
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2.5, alpha = 0.9) +
  theme_minimal() +
  scale_color_manual(values = met.brewer("Renoir", 10)) +
  labs(x = "Coerência Semântica",
       y = "Exclusividade",
       title = "Comparação entre Coerência Semântica e Exclusividade",
       subtitle = "O intervalo entre 60 e 80 tópicos parece ser o mais apropriado | Elaboração: Os autores")

#Escolha do modelo com número adequado de tópicos
topic_model <- k_result  |>  
  filter(K == 77) |> 
  pull(topic_model)  %>%   
  .[[1]]

# UMAP####
#Transformação de matriz theta com informações
#Transformação de matriz thetha com informações
stm_gamma <- tidy(topic_model,
                  matrix = "gamma",
                  document_names = rownames(filosparse))  |>  
  group_by(topic) |>  
  arrange(topic, desc(gamma)) |>  
  mutate(topic = as_factor(topic))

gamma_matrix <- stm_gamma |> 
  pivot_wider(names_from = topic, 
              values_from = gamma)

topic_labels <- stm_gamma  |> 
  group_by(document) |> 
  slice_max(order_by = gamma) |> 
  ungroup()

theta_matrix <- gamma_matrix |> 
  left_join(topic_labels  |>  
              select(document, topic), by = "document")


umap_rec <- recipe(~., data = theta_matrix) %>%
  update_role(document, topic, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())
umap_prep <- prep(umap_rec)

#Gráfico
#Extrair docs exemplares
docs_labels <- topic_labels %>%
  group_by(document) %>%
  slice_max(order_by = gamma) %>%
  ungroup() |> 
  pull(document)

# Plot do UMAP
juice(umap_prep)  |> 
  ggplot(aes(UMAP1, UMAP2, label = topic)) +
  geom_point(aes(color = topic), alpha = 0.2, size = 20) +
  geom_text(check_overlap = TRUE) + 
  scale_color_manual(values = met.brewer("Cross", 76)) +
  theme_minimal() +
  theme(legend.position = "none")


