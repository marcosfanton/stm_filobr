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
library(ggsci) # Paleta cores
library(ggtext) # Config de textos


# Filtragem de observações com base na qualidade dos resumos e seleção de variáveis ####
# Importação do banco limpo em "limpeza_catalogo.R"
dados <- read.csv("dados/catalogo.csv")

dados_ <- dados |> #Banco com total de trabalhos por Área de Conhecimento Filosofia (N = 12525)
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
  dplyr::filter(!doc_id %in% c(7854, 7849, 8205)) |># - 3 trabalhos com palavras repetidas (n = 11733)
  select(doc_id, an_base, nm_producao, ds_palavra_chave, ds_resumo, nr_paginas, g_orientador)

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
  dplyr::anti_join(get_stopwords("pt"))|> # Stopwords em pt
  dplyr::anti_join(get_stopwords("en"))|> 
  dplyr::anti_join(filolixo) |> # Dicionário de stopwords personalizado no script filograms.R <===
  dplyr::filter(str_detect(word, "^si$|^fe$|...")) |> #remove todas palavras com menos de 3 caracteres (mantém 'si' e 'fe') 
  dplyr::count(doc_id, nm_producao, word, an_base, g_orientador)    # Contagem da frequência absoluta de cada token

# Remoção de palavras esparsas
# Contagem
palavras_raras <- filowords |>  
    count(doc_id, word) |> 
    group_by(word) |> 
    mutate(doc_freq = n_distinct(doc_id))  |> 
    filter(doc_freq <= 2) |> # Exclusão de 53527 tokens que aparecem em 1 documento apenas 1 vez
  select(word)

# Remoção em filowords
filowords <- filowords |> 
  dplyr::anti_join(palavras_raras)

# Salvar banco para análise
filowords |> 
  readr::write_csv("dados/filowords_stm.csv")
filowords <- read_csv("dados/filowords_stm.csv",
                      show_col_types = FALSE)

# Preparação do banco em matriz esparsa
filosparse <- filowords |> tidytext::cast_sparse(doc_id, word, n) #matriz para análise
# Preparação das covariáveis para análise
covars <- dplyr::distinct(filowords, doc_id, an_base, g_orientador) #matriz de covariáveis

# Modelo STM
#Modelo simples####
topic_model <- stm(filosparse,
                   K = 80,
                   prevalence = ~ g_orientador + s(an_base),
                   seed = 1987,
                   data = covars,
                   init.type = "Spectral")

# Extração de matrizes####
# Beta
tidybeta <- tidytext::tidy(topic_model) |> 
  mutate(topic = as_factor(topic))
# Gamma
tidygamma <- tidytext::tidy(topic_model, 
                            matrix = "gamma",
                            document_names = topic_model$meta$doc) |> 
  right_join(covars, by = c("document" = "doc_id")) |> 
  arrange(topic, desc(gamma)) |> 
  mutate(topic = as_factor(topic))

# Matriz Beta - Palavras mais frequentes de cada tópico 
top_words <- tidybeta %>%
  arrange(desc(beta)) %>%
  group_by(topic) %>%
  top_n(5, beta) %>%
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

# Matriz Gamma - Prevalência de tópicos com respectivos termos por década
gamma_words <- tidygamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  left_join(top_words, by = "topic") %>%
  mutate(topic = paste0("T", topic),
         topic = reorder(topic, gamma))

# Gráfico beta ####
tidybeta  |> 
  group_by(topic) %>%
  top_n(4, beta) %>%
  ungroup() %>%
  mutate(topic = paste0("Tópico ", topic),
         term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(x = term, y = beta, fill = as.factor(topic))) +
  theme_classic() +
  geom_col(alpha = 0.9, show.legend = FALSE) +
  scale_fill_manual(values = met.brewer("Nizami", 80)) +
  facet_wrap(~ topic, scales = "free_y") +
  labs(x = "",
       y = expression(beta)) +
  coord_flip() +
  scale_x_reordered() 

# Gráfico gamma #### 
gamma_words |> 
  top_n(80, gamma) |> 
  ggplot(aes(topic, gamma, 
             label = terms, 
             fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = -0.000001, size = 3) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.04),
                     labels = percent_format()) +
  theme_classic() +
  scale_fill_manual(values = met.brewer("Cross", 80))  +
  labs(x = NULL, 
       y = NULL,
       title = "80 Tópicos do *corpus* de teses e dissertações de Filosofia  com a probabilidade média esperada para cada tópico (&#947;)") +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        legend.position = "none",
        text = element_text(size = 10)) 
  
# Findthoughts (STM) #### 
findallthoughts_m80 <- tidygamma |> 
  mutate(document = as.integer(document)) |> 
  left_join(dados,
            by = c("document" = "doc_id")) |> # Unifica o banco dados com a matrix gamma
  group_by(document) |> # Agrupa os valores gamma de cada tópico
  slice_max(order_by = gamma, n = 1) |> # Escolhe o tópico com maior gamma de cada documento
  select(document, topic, nm_producao, ds_resumo, gamma) # Seleciona apenas as variáveis de interesse

findthoughts_m80 <- tidygamma |> 
  mutate(document = as.integer(document)) |> 
  left_join(dados,
            by = c("document" = "doc_id")) |> # Unifica o banco dados com a matrix gamma 
  group_by(topic) |> 
  slice_max(order_by = gamma, n = 5) |> # Encontra os docs mais representativo de cada tópico (com maior gamma)
  select(document, topic, nm_producao, ds_resumo, gamma) # Seleciona apenas as variáveis de interesse

# Gráfico semântica x exclusividade para cada tópico
excl <- exclusivity(topic_model)
semcoh <- semanticCoherence(topic_model, filosparse)
diag_df <- tibble(excl, semcoh, topic = factor(1:80))
ggplot(diag_df, aes(x = semcoh, y = excl, label = topic))+
  geom_text() +
  theme_classic()


#Salvar modelos em .txt e .csv
findthoughts_m80 |> 
  readr::write_csv("dados/findthoughts_m80.csv")

#Salvar resultados em .txt
sink('dados/summary_topicmodel80.txt')
print(summary(topic_model))
sink()


#Efeitos#### 
# Efeito ano####
stm_effect_ano <- stm::estimateEffect(1:80 ~ an_base, 
                                    stmobj = topic_model, 
                                    metadata = covars, 
                                    uncertainty = "Global")

ext_stm_effect_ano <- tidystm::extract.estimateEffect(x = stm_effect_ano, 
                                                        covariate = "an_base", 
                                                        model = topic_model, 
                                                        method = "pointestimate",
                                                        labeltype = "prob",
                                                        n = 3)

# Gráfico ano
ext_stm_effect_ano |> 
ggplot(aes(x = covariate.value,
           y = estimate,
           ymin = ci.lower,
           ymax = ci.upper)) +
  facet_wrap(~topic) +
  theme_classic() +
  geom_ribbon(alpha = .7, color = "#7da7ea", fill = "#7da7ea") +
  geom_line(color = "#1d4497") +
  labs(x = "Ano",
       y = "Proporção de Tópico Esperada",
       title = "Efeito de estimação pontual e intervalos de confiança dos tópicos ao longo do tempo") +
  theme(legend.position = "none")





#Cálculo da variação dos tópicos
calc_ano <- ext_stm_effect_ano |> 
  filter(covariate.value %in% c(1991, 2021)) |> 
  select(topic, covariate.value, estimate) |> 
  pivot_wider(names_from = "covariate.value", 
              values_from  = "estimate") |> 
  mutate(diferenca = round(`1991` - `2021`,4)) 

# Efeito gênero de orientador####
effect_gender <- stm::estimateEffect(1:80 ~ g_orientador, 
                                       stmobj = topic_model, 
                                       metadata = covars, 
                                       uncertainty = "Global")

t_effect_gender <- tidystm::extract.estimateEffect(x = effect_gender, 
                                                           covariate = "g_orientador", 
                                                           model = topic_model, 
                                                           method = "pointestimate",
                                                           labeltype = "prob",
                                                           n = 3)
# Gráfico gênero
ggplot(t_effect_gender, aes(x = covariate, 
                                    y = estimate,
                                    fill = covariate.value)) +
  geom_bar(stat = "identity", position = "fill")+
  facet_wrap(~topic) +
  coord_flip()
  geom_line(color = "#1d4497") +
  geom_point(color = "#1d4497", size = 1) +
 # scale_x_discrete(labels = c("Female", "Male")) +
  facet_wrap(~topic, labeller = labeller(label = label_wrap_gen(width = 60))) +
  theme_classic() +
  labs(x = "Gênero",
       y = "Estimativa",
       title = "Efeito de estimação pontual dos tópicos por gênero do orientador") +
  theme(legend.position = "none")

#Cálculo da variação de tópicos por gênero
gen_dif <- t_effect_gender |> 
  select(topic, covariate.value, estimate) |> 
  pivot_wider(names_from = "covariate.value", 
              values_from  = "estimate") |> 
  mutate(diferenca = round(Male - Female, 4)) 

# Rotulação de categorias ####
# Ver arquivo topic_model80.txt ou similar

# Categorias Hugo e Marcos
categorias <- dplyr::tibble(
  topic = as_factor(unlist(list(
    c(2, 3,9,11,12,18,20,25,30,32,33,39,45,46,47,48,49,57,77), # Política
    c(5,13,28,29,42,52,63,73,79), # Fenomenologia 
    c(27,40,58,59,66,71,76), # Mente e Linguagem
    c(6,7,19,34,43,53,60,61), # Moral
    c(14,15,21,23,51,54,65,68,69,70,75,78), # Metafísica
    c(4,10,24,36,37,50,55,62), # Estética 
    c(8,16,17,22,26,31,35,41,44,46, 56,64,67,72,74,80), # Filosofia da ciência
    c(1,38) # Excluir
  ))),
  category = as_factor(c(
    rep("Filosofia Social e Política", 19),
    rep("Fenomenologia e Hermenêutica", 9),
    rep("Filosofia da Mente e da Linguagem", 7),
    rep("Ética", 8),
    rep("Metafísica", 12),
    rep("Estética", 8),
    rep("Filosofia da Ciência", 16),
    rep("Excluídos", 2)
  )
  ))

# Categorias Carolina
categorias <- dplyr::tibble(
  topic = as_factor(unlist(list(
    c(2,3,12,18,20,25,30,32,33,39,45,46,47,48,49,53,68,69,77), # Política
    c(5,8,14,16,17,22,26,31,35,44,56,63,64,65,66,67,71,74,76,80), # Lógica, Linguagem e Ciência
    c(6,7,15,19,34,40,42,43,52,57,60,61,79), # Moral
    c(11,21,23,54,70,73,75,78), # Metafísica
    c(4,10,24,36,37,50,51,55,62), # Estética 
    c(9,13,27,28,29,41,58,59,72), # Psicologia e Filosofia da Mente
    c(1,38) # Excluir
  ))),
  category = as_factor(c(
    rep("Filosofia Social e Política", 19),
    rep("Lógica, Linguagem e Ciência", 20),
    rep("Filosofia Moral", 13),
    rep("Metafísica", 8),
    rep("Estética", 9),
    rep("Psicologia e Filosofia da Mente", 9),
    rep("Excluídos", 2)
  )
  ))

tidygamma <- tidygamma |> 
  left_join(categorias, by = "topic")

category_labels <- tidygamma  |> 
  group_by(document) |> 
  slice_max(order_by = gamma) |> 
  ungroup() |> 
  distinct(document, .keep_all = TRUE)

category_labels |> 
  drop_na() |> 
  ggplot(aes(x = an_base, color = category)) +
  geom_freqpoly(binwidth = 1, linewidth = 1.2) +
  scale_x_continuous(limits = c(1987, 2021)) +
  scale_y_continuous(limits = c(0, 200), position = "right") +
  scale_color_manual(values = met.brewer("Cross", 8))  

prop_by_category <- category_labels |> 
  drop_na() |> 
  group_by(category, g_orientador) |> 
  summarise(count = n()) |> 
  ungroup() |> 
  mutate(prop = count / sum(count))
ggplot(prop_by_category, aes(x = category, y = prop, fill = g_orientador)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Proportion", fill = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = met.brewer("Austria", 2))  

category_labels  |> 
  ggplot(aes(x = an_base, color = category, group = category)) +
  geom_line(stat = "bin", binwidth = 1, size = 1.2, alpha = 0.5) +
  geom_point(aes(y = ..count..), stat = "bin", binwidth = 1, size = 1.5, alpha = 0.7) +
  scale_x_continuous(limits = c(1987, 2021)) +
  scale_y_continuous(limits = c(0, 200), position = "right") +
  scale_color_manual(values = met.brewer("Austria", 8)) +
  theme_minimal() +
  labs(x = "Ano", 
       color = "Categoria",
       title = "Número de trabalhos de filosofia defendidos por categoria",
       subtitle = "Elaboração: Os autores | Dados: Catálogo de Teses e Dissertações (CAPES)")
  
# UMAP####
#Transformação de matriz theta com informações
#Transformação de matriz thetha com informações
#Gamma matrix sem categorias
gamma_matrix <- tidygamma |> 
  drop_na() |> 
  pivot_wider(names_from = topic, 
              values_from = gamma)

#Gamma matrix com categorias 
gamma_matrix <- tidygamma |>
  drop_na() |> 
  select(document, category, topic, gamma) |> 
  group_by(category, document) |> 
  summarise(gamma = mean(gamma), .groups = 'drop') |> 
  pivot_wider(id_cols = document, 
              names_from = category, 
              values_from = gamma)

gamma_matrix1 <- as.matrix.data.frame(gamma_matrix[,2:8], rownames.force = TRUE)
rownames(gamma_matrix1) <- unique(gamma_matrix$document)

me_gamma.pca <- prcomp(gamma_matrix1)
me_gamma_umap <- umap(me_gamma.pca$x, 
                      n_neighbors = 4,
                      n_epochs = 500,
                      spread = 6,
                      min_dist = .7,
                      seed = 1876
)
me_gamma_umap.pred <- predict(me_gamma_umap, me_gamma.pca$x)
me_gamma_umap.pred  |>  head(5)

# Outra análise
theta_matrix <- gamma_matrix |> 
  left_join(category_labels  |>  
              select(document, topic, category), by = "document")

umap_rec <- recipe(~., data = theta_matrix)  |> 
  update_role(document, topic, category, new_role = "id") |> 
  step_normalize(all_predictors())  |> 
  step_umap(all_predictors())
umap_prep <- prep(umap_rec)

#Gráfico
#Extrair docs exemplares
docs_labels <- category_labels  |> 
  group_by(document)  |> 
  slice_max(order_by = gamma) |> 
  ungroup() |> 
  pull(document)

# Plot do UMAP
juice(umap_prep)  |> 
  ggplot(aes(UMAP1, UMAP2, fill = category, label = topic)) +
  geom_point(aes(color = category), alpha = 0.5, size = 6) +
  geom_text(check_overlap = TRUE, size = 5) +
 # scale_color_manual(values = met.brewer("Austria", 8)) +
  theme_minimal() 


juice(umap_prep)  |> 
  ggplot(aes(UMAP1, UMAP2, label = topic)) +
  geom_point(aes(color = topic), shape = 19, alpha = 0.2, size = 14) +
  geom_text(check_overlap = TRUE) + 
  scale_color_manual(values = met.brewer("Cross", 80)) +
  theme_minimal() +
  theme(legend.position = "none")

#Salvar gráfico
ggsave(
  "figs/umap_m80_tema1.png",
  bg = "white",
  width = 20,
  height = 12,
  dpi = 300,
  plot = last_plot())

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
  geom_point(aes(color = topic), shape = 19, alpha = 0.2, size = 14) +
  geom_text(check_overlap = TRUE) + 
  scale_color_manual(values = met.brewer("Cross", 77)) +
  theme_minimal() +
  theme(legend.position = "none")




#Modelos Múltiplos (código de Julia Silge)####
#Modelo com múltiplos K####
plan(multisession)
many_models <- tidyr::tibble(K = c(75, 77, 78, 79, 80, 85)) |> #Teste de modelos com 40 a 80 Tópicos
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
       subtitle = "O intervalo entre 70 e 80 tópicos parece ser o mais apropriado | Elaboração: Os autores")

#Gráfico Exclusividade x Coerência Semântica por tópicos
k_result |>  
  select(K, exclusivity, semantic_coherence)  |> 
  filter(K %in% c(30, 40, 50, 60, 70, 75, 77, 78, 79, 80, 90, 100))  |> 
  unnest(cols = c(exclusivity, semantic_coherence))  |> 
  mutate(K = as.factor(K)) |> 
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2.5, alpha = 0.9) +
  theme_minimal() +
  scale_color_manual(values = met.brewer("Renoir", 12)) +
  labs(x = "Coerência Semântica",
       y = "Exclusividade",
       title = "Comparação entre Coerência Semântica e Exclusividade",
       subtitle = "O intervalo entre 65 e 80 tópicos parece ser o mais apropriado | Elaboração: Os autores")

#Escolha do modelo com número adequado de tópicos
topic_model <- k_result  |>  
  filter(K == 85) |> 
  pull(topic_model)  %>%   
  .[[1]]

