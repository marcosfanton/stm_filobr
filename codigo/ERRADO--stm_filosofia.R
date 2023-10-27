####Pacotes####
library(tidyverse)
library(here)
library(tidytext) # Manipulação de texto
library(stm) # Structural topic model
library(furrr) # Rodar múltiplos modelos 
library(MetBrewer) # Paleta de cores
library(tidystm) # Extração de efeitos do modelo

library(scales) # Uso de porcentagem em gráficos
library(embed) # UMAP
library(umap) # UMAP
library(recipes) # UMAP
library(ggtext) # Config de textos
library(gganimate) # Produção de gif
library(ggstream)  # Produção de grafico stream
library(geomtextpath)

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
  dplyr::filter(!doc_id %in% c(7854, 7849, 8205)) |> # - 3 trabalhos com palavras repetidas (n = 11733)
  dplyr::filter(an_base >= 1991) |>  # - 7 trabalhos (11726)
  dplyr::mutate(doc_id = row_number()) |> # Reconfiguração do id dos docs (ao rodar STM, os ids são desconfigurados)
  select(doc_id, an_base, nm_producao, ds_palavra_chave, ds_resumo, nr_paginas,  g_orientador)

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
covars <- filowords |> dplyr::distinct(doc_id, an_base, g_orientador) #matriz de covariáveis

#********* STM *********####
# Múltiplos modelos - 40-100####
plan(multisession)
many_models <- tibble(K = c(40, 50, 60, 70, 80, 90, 100)) |> #Teste de modelos com 40 a 80 Tópicos
  mutate(topic_model = furrr::future_map(K, ~ stm(filosparse, 
                                                  K = .,
                                                  prevalence = ~ g_orientador + s(an_base),
                                                  seed = 1987,
                                                  data = covars,
                                                  init.type = "Spectral"),
                                         .options = furrr_options(seed = TRUE)))
# Gráficos de diagnóstico####
heldout <- make.heldout(filosparse)
k_result <- many_models |> # Cria banco com resultados de cada tópico
  mutate(exclusivity = map(topic_model, exclusivity),
         semantic_coherence = map(topic_model, semanticCoherence, filosparse),
         eval_heldout = map(topic_model, eval.heldout, heldout$missing),
         residual = map(topic_model, checkResiduals, filosparse),
         bound =  map_dbl(topic_model, function(x) max(x$convergence$bound)),
         lfact = map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
         lbound = bound + lfact,
         iterations = map_dbl(topic_model, function(x) length(x$convergence$bound)))

# Gráfico 
k_result |>  
  transmute(K,
            `Lower bound` = lbound,
            `Residual` = map_dbl(residual, "dispersion"),
            `Semantic coherence` = map_dbl(semantic_coherence, mean),
            `Held-out likelihood` = map_dbl(eval_heldout, "expected.heldout"))  |> 
  gather(Metric, Value, -K) |> 
  ggplot(aes(K, 
             Value, 
             color = Metric)) +
  geom_point(size = 2)+
  geom_line(linewidth = 1.5, alpha = 0.9, show.legend = FALSE) +
  scale_color_manual(values = met.brewer("Austria", 4))  +
  theme_classic()+
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K",
       y = NULL)+
  theme(legend.position = "none",
        text = element_text(size = 30))

ggsave(
  "figs/stm_diagnostico.png",
  bg = "white",
  width = 17,
  height = 12,
  dpi = 1200,
  plot = last_plot())

#Modelo simples####
topic_model <- stm(filosparse,
                   K = 80,
                   prevalence = ~ g_orientador + s(an_base),
                   seed = 1987,
                   data = covars,
                   init.type = "Spectral")
# OU
#Escolha do modelo com número adequado de tópicos a partir do resultado de modelos múltiplos 
topic_model <- k_result  |>  
  filter(K == 80) |> 
  pull(topic_model) %>% 
  .[[1]]
# Extração de matrizes####
# Carregar modelo
load(file = "dados/stm_model80t.rda")
# Beta
tidybeta <- tidytext::tidy(topic_model) |> 
  mutate(topic = as_factor(topic))
# Gamma
tidygamma <- tidytext::tidy(topic_model, 
                            matrix = "gamma") |> 
  right_join(covars, by = c("document" = "doc_id")) |> 
  arrange(topic, desc(gamma)) |> 
  mutate(topic = as_factor(topic))

# Matriz Beta - Palavras mais frequentes de cada tópico 
top_words <- tidybeta  |> 
  arrange(desc(beta))  |> 
  group_by(topic) |> 
  top_n(5, beta) |> 
  summarise(terms = paste(term, collapse = ", ")) |> 
  ungroup()

# Matriz Gamma - Prevalência de tópicos com respectivos termos
gamma_words <- tidygamma |> 
  group_by(topic) |> 
  summarise(gamma = mean(gamma)) |> 
  arrange(desc(gamma)) |> 
  left_join(top_words, by = "topic") |> 
  mutate(topic = reorder(topic, gamma))

# Gráfico beta ####
tidybeta  |> 
  group_by(topic)  |> 
  top_n(5, beta) |> 
  ungroup() |> 
  mutate(topic = paste0("Tópico ", topic),
         term = reorder_within(term, beta, topic)) |> 
  ggplot(aes(x = term, y = beta, fill = as.factor(topic))) +
  theme_classic() +
  geom_col(alpha = 0.9, show.legend = FALSE) +
  scale_fill_manual(values = met.brewer("Nizami", 80)) +
  facet_wrap(~ topic, scales = "free") +
  labs(x = "",
       y = expression(beta)) +
  coord_flip() +
  scale_x_reordered() 

# Gráfico gamma #### 
gamma_words |> 
  ggplot(aes(topic, gamma, 
             label = terms, 
             fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = -0.000001, size = 6) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.038),
                     labels = percent_format()) +
  theme_classic() +
  scale_fill_manual(values = met.brewer("Cross", 80))  +
  labs(x = NULL, 
       y = NULL,
       title = "80 Topics with the Estimated Topic Proportion (&#947;)") +
  theme(plot.title = element_markdown(face = "bold"),
        legend.position = "none",
        text = element_text(size = 20)) 

# Salvar gráfico
ggsave(
  "figs/stm_model80t.png",
  bg = "white",
  width = 22,
  height = 20,
  dpi = 900,
  plot = last_plot())

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

# Rotulação de categorias ####
# Ver arquivo topic_model80.txt ou similar
# Categorias 
categorias <- dplyr::tibble(
  topic = as_factor(unlist(list(
    c(3,9,12,18,20,25,30,32,33,39,45,47,48,49,56,68,75), # Política OK
    c(5,13,28,29,42,62,71,77,79), # Fenomenologia OK
    c(26,27,40,41,58,64,70,72,74,78), # Mente e Linguagem OK
    c(6,7,19,34,43,52,59), # Moral OK
    c(14,15,21,23,46,51,53,55,57,65,69,73,76), # Metafísica OK
    c(4,11,24,36,37,50,54,60,61), # Estética OK
    c(8,16,17,22,31,35,44,63,66,67), # Filosofia da ciência
    c(1,2,10,38,80) # Excluir
  ))),
  category = as_factor(c(
    rep("Social and Political Philosophy", 17),
    rep("Phenomenology and Hermeneutics", 9),
    rep("Philosophy of Mind and Language", 10),
    rep("Ethics", 7),
    rep("Metaphysics", 13),
    rep("Aesthetics", 9),
    rep("Philosophy of Science", 10),
    rep("Excluídos", 5)
  )
  ))

# Inclusão das categorias na matrix gamma
tidygamma <- tidygamma |> 
  left_join(categorias, by = "topic") 

tidygamma <- tidygamma |> 
  filter(category != "Excluídos")

# UMAP ####
# Preparação do banco
gammawide <- tidygamma  |>  
  summarise(gamma = mean(gamma), .by = c(category, document))  |> 
  pivot_wider(id_cols = document,
              names_from = category, 
              values_from = gamma) 

# Preparação da rotulagem de categorias e tópicos com base na média de gamma
# Tópicos
label_topic <- tidygamma |> 
  group_by(document, topic) |> 
  summarise(gamma = mean(gamma)) |> 
  slice_max(gamma, n = 1, with_ties = FALSE) |> 
  select(-gamma)

# Junção de rótulos 
gammawide <- gammawide |> 
  left_join(label_topic, by = "document") |> 
  left_join(categorias, by = "topic") 

# UMAP via recipe
umap_recipe <- recipe(~., data = gammawide)  |> 
  update_role(document, topic, category, new_role = "id") |> 
  step_normalize(all_predictors())  |> 
  step_umap(all_predictors())
# Modelo
umap_model <- prep(umap_recipe)

# Plot do UMAP
juice(umap_model)  |> 
  ggplot(aes(UMAP1, UMAP2)) +
  geom_point(aes(fill = category), alpha = .8, size = 6, shape = 21) +
  geom_text(aes(label = topic), check_overlap = TRUE, size = 3, color = "white") +
  scale_fill_manual(values = met.brewer("Cross", 7)) +
  theme_minimal() +
  labs(title = "",
       fill = "") +
  theme(plot.title = element_markdown(face = "bold"),
        legend.position = "bottom")

ggsave(
  "figs/stm_umap_80_7cat_topics.png",
  bg = "white",
  width = 12,
  height = 10,
  dpi = 900,
  plot = last_plot())

# Tabela | Categoria-Tópicos####
gamma_words <- gamma_words |> left_join(categorias,
                       by = "topic") 

# Tabelas | ver stm_descricao####
tab_category <- gamma_words |>
    filter(category == "Philosophy of Science") |> 
  arrange(desc(gamma)) |> 
  gt() |>
  cols_hide(category) |> 
  cols_move_to_end(gamma) |> 
  cols_label(   # Títulos
    topic = "Topic",
    terms = "Terms (\U03B2)",
    gamma = "Gamma(\U03B3)%)"
  ) |>
  tab_header(
    title = "Topics sorted as Philosophy of Science") |> 
  cols_align(
    align = "left") |>
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

gtsave(tab_category,
       "figs/stm_table_science.png",
       vwidth = 2000, vheight = 3000)


#Efeitos#### 
# Efeito ano####
stm_efeitoano <- stm::estimateEffect(1:80 ~ an_base, 
                                    stmobj = topic_model, 
                                    metadata = covars, 
                                    uncertainty = "Global")

stm_ano <- tidystm::extract.estimateEffect(x = stm_efeitoano, 
                                                        covariate = "an_base", 
                                                        model = topic_model, 
                                                        method = "pointestimate",
                                                        labeltype = "prob",
                                                        n = 3)

# Limpeza do banco e rotulação de categorias  
stm_ano <- stm_ano |> 
  mutate(topic = as_factor(topic),
         label = str_replace_all(label, "\\(Covariate Level: 1991\\)", "")) |> 
  left_join(categorias, by = "topic") |> 
  filter(category != "Excluídos")

# Sumariza por categoria | COM EFEITOS####
stmcat_ano <- stm_ano |> 
  summarize(total = sum(estimate), 
            .by = c(category, covariate.value)) 

# Gráfico | Categorias-Tópicos COM EFEITOS####
stmcat_ano |>
  ggplot(aes(x = covariate.value,
             y = total,
             color = category)) +
  geom_point(alpha=0.5, size=0.5) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              se = FALSE,
              linewidth = .9) +
  theme_classic() +
  labs(x = "Year",
       y = "Estimation",
       color = "Category",
       title = "Trends of Categories in Dissertations",
       subtitle = "Brazilian Philosophy Graduate Programs (1988-2021)") +
  scale_color_manual(values = met.brewer("Cross", 7)) +
  scale_y_continuous(position = "right") +
  scale_x_continuous(limits = c(1988, 2021)) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(plot.title = element_markdown(face = "bold"),
        text = element_text(size = 15))

# Gráfico 
 stm_ano |> 
  ggplot(aes(x = covariate.value,
             y = estimate,
             color = topic)) +
  facet_wrap(~category) +
  theme_classic() +
  geom_line() +
  labs(x = "Year",
       y = "Point Estimation for Each Topic",
       title = "Trends of topics within categories",
       subtitle = "Brazilian Philosophy Graduate Programs (1988-2021)") +
  scale_color_manual(values = met.brewer("Nizami", 80)) +
  theme(legend.position = "none",
        plot.title = element_markdown(face = "bold"))

# Gráfico | Tempo-Tópicos####
stm_ano |> 
  mutate(label = str_extract(label, "\\w+\\s*,\\s*\\w+")) |> 
  ggplot(aes(x = covariate.value,
           y = estimate,
           ymin = ci.lower,
           ymax = ci.upper)) +
  facet_wrap(~label) +
  theme_classic() +
  geom_ribbon(alpha = .7, color = "#7da7ea", fill = "#7da7ea") +
  geom_line(color = "#1d4497") +
  labs(x = "Year",
       y = "Point Estimation Effect",
       title = "Trends of Topics in Dissertations (1991-2021)") +
  theme(legend.position = "none",
        plot.title = element_markdown(face = "bold"),
        text = element_text(size = 15))

ggsave(
  "figs/stm_80t_topicyeareffect.png",
  bg = "white",
  width = 23,
  height = 20,
  dpi = 900,
  plot = last_plot())

# Gráfico | Tempo-Categoria por trabalho####
# Rotulação de cada documento por categoria
cat_count <- tidygamma  |> 
  group_by(document, category)  |> 
  summarize(gamma = mean(gamma))  |> 
  ungroup()  |> 
  group_by(document)  |> 
  slice_max(gamma, n = 1, with_ties = FALSE)  |> 
  left_join(tidygamma  |>  
              select(document, an_base, g_orientador),
            by = "document") |> 
  ungroup() |> 
  unique()

# Sumariza por trabalho
cat_count_ano <- cat_count |> 
  summarize(n = n(), .by = c(an_base, category)) # Número de trabalhos

# Gráfico
cat_count_ano |>
  ggplot(aes(x = an_base,
             y = n,
             color = category)) +
  geom_point(alpha=0.5, size=1.5) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              se = FALSE,
              linewidth = 1.8) +
  theme_classic() +
  labs(x = "Year",
       y = "Number of Dissertations",
       color = "Category",
       title = "Trends of Categories in Dissertations",
       subtitle = "Brazilian Philosophy Graduate Programs (1988-2021)") +
  scale_color_manual(values = met.brewer("Cross", 7)) +
  scale_y_continuous(limits = c(0, 200), position = "right") +
  scale_x_continuous(limits = c(1988, 2021)) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(legend.position = "top",
        plot.title = element_markdown(face = "bold"),
        text = element_text(size = 15))

# Cálculo da variação dos tópicos#### 
calc_ano <- ext_stm_effect_ano |> 
  filter(covariate.value %in% c(1991, 2021)) |> 
  select(topic, covariate.value, estimate) |> 
  pivot_wider(names_from = "covariate.value", 
              values_from  = "estimate") |> 
  mutate(diferenca = round(`1991` - `2021`,4)) 





# Efeito gênero de orientador####
stm_efeitogenero <- stm::estimateEffect(1:80 ~ g_orientador, 
                                       stmobj = topic_model, 
                                       metadata = covars, 
                                       uncertainty = "Global")

stm_genero <- tidystm::extract.estimateEffect(x = stm_efeitogenero, 
                                                           covariate = "g_orientador", 
                                                           model = topic_model, 
                                                           method = "pointestimate",
                                                           labeltype = "prob",
                                                           n = 3)
# Cálculo da proporção de gênero para cada tópico####
prop_topicgenero <- stm_genero |> 
  group_by(topic) |> 
  mutate(total = sum(estimate)) |> 
  group_by(topic, covariate.value) |> 
  mutate(proporcao = round(estimate/sum(total)*100,2)) |> 
  arrange(covariate.value, desc(proporcao)) 

# Gráfico Tópico-gênero####
prop_topicgenero |>
  ggplot(aes(x = fct_inorder(as_factor(topic)),
             y = proporcao,
             fill = covariate.value)) +
  geom_col() +
  geom_hline(yintercept = 50, color = "white") +
  scale_y_continuous(labels = percent_format(scale = 1),
                     expand = c(0,0)) +
  theme_classic() +
  coord_flip() +
  scale_fill_manual(values = met.brewer("Austria", 2))  +
  labs(x = "",
       y = "",
       title = "Description of the Gender Distribution among the 80 topics",
       subtitle = "Theses Supervised by <span style= 'color:#16317d;'>**Men**</span> e <span style= 'color:#a40000;'>**Women**</span>") +
  theme(legend.position = "none",
        plot.title = element_markdown(face = "bold"),
        plot.subtitle = element_markdown(),
        text = element_text(size = 15))

ggsave(
  "figs/stm_80t_topicgenero.png",
  bg = "white",
  width = 20,
  height = 18,
  dpi = 600,
  plot = last_plot())

# Gráfico Gênero-Categoria####

# Inclusão das categorias
prop_catgenero <- stm_genero |> 
  mutate(topic = as_factor(topic))  |> 
  left_join(categorias, by = "topic") |> 
  filter(category != "Excluídos") |> 
  group_by(category, covariate.value) |>
  summarise(total = sum(estimate)) |> 
  mutate(proporcao = round(total/sum(total)*100,2)) |> 
  arrange(covariate.value, desc(proporcao)) 


prop_catgenero |>
  ggplot(aes(x = fct_inorder(category),
             y = proporcao,
             fill = covariate.value)) +
  geom_col() +
  geom_hline(yintercept = 50, color = "white") +
  scale_y_continuous(labels = percent_format(scale = 1),
                     expand = c(0,1)) +
  theme_classic() +
  coord_flip() +
  scale_fill_manual(values = met.brewer("Austria", 2))  +
  labs(x = "",
       y = "",
       title = "Description of the Gender Distribution among Categories",
       subtitle = "Theses Supervised by <span style= 'color:#16317d;'>**Men**</span> e <span style= 'color:#a40000;'>**Women**</span>") +
  theme(legend.position = "none",
        plot.title = element_markdown(face = "bold"),
        plot.subtitle = element_markdown(),
        axis.text.y = element_markdown(size = 20, 
                                       face = "bold",
                                       color = "black"),
        text = element_text(size = 25))

ggsave(
  "figs/stm_80t_categorygender.png",
  bg = "white",
  width = 23,
  height = 18,
  dpi = 600,
  plot = last_plot())

# Tabela | 11-11 tópicos-gênero####
tab_80 <- prop_genero  |> 
  select(topic, covariate.value, label, proporcao)  |> 
  mutate(label = str_replace_all(label, "\\(Covariate Level: Male\\)", "")) |> 
  pivot_wider(names_from = covariate.value,
              values_from = c(topic, proporcao)) 

# Prepara o gamma
tab_gamma <- gamma_words |> 
  mutate(topic = as_factor(str_replace_all(topic, "T", "")))

# Tabela dos 10-10
tab_22 <- left_join(tab_80,
                    tab_gamma,
                    by = c("topic_Female" = "topic")) |> 
  slice(1:11, 70:80) |> 
  select(-c(terms, topic_Male)) |> 
  mutate(gamma = round(gamma*100,4))

# TABELA 3 
tabela_22 <- tab_22 |> 
  gt() |> 
  cols_move_to_start(c(category,topic_Female)) |> 
  cols_hide(proporcao_Male) |> 
  cols_label(   # Títulos
    topic_Female = "Topic",
    label = "Terms (\U03B2)",
    gamma = "\U03B3(%)",
    proporcao_Female = "Woman (%)",
    category = "Category"
  ) |>
  tab_header(
    title = "Topics with Higher and Lower Prevalence of Women Supervisors") |> 
  cols_align(
    align = "center",
    columns = everything()) |>  
  data_color(
    columns = proporcao_Female,
    target_columns = everything(),
    palette = "inferno"
  ) |>
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

gtsave(tabela_22,
       "figs/stm_table_topicgender.png",
       vwidth = 2000, vheight = 3000)
