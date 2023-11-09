####Pacotes####
library(tidyverse)
library(here)
library(tidytext) # Manipulação de texto
library(stm) # Structural topic model
library(furrr) # Rodar múltiplos modelos 
library(MetBrewer) # Paleta de cores
library(geomtextpath)
library(gt) # Tabela
library(recipes) # UMAP
library(umap) # UMAP
library(embed) # UMAP
library(ggtext) # Config de textos
library(tidystm) # Extração de efeitos do modelo
library(scales) # Percentagem em gráficos

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
# Preparação do banco em matriz esparsa####
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

# Escolha do modelo#### 
#Escolha do modelo a partir do resultado de modelos múltiplos 
topic_model <- k_result  |>  
  filter(K == 80) |> 
  pull(topic_model) %>% 
  .[[1]]
#OU
#Modelo simples####
topic_model <- stm(filosparse,
                   K = 80,
                   prevalence = ~ g_orientador + s(an_base),
                   seed = 1987,
                   data = covars,
                   init.type = "Spectral")

saveRDS(topic_model, "dados/stm_modelo80.rda")
# LOAD Modelo 80 tópicos
load(file = "dados/stm_model80t.rda")

# Extração matrizes####
# Extração beta####
tidybeta <- tidytext::tidy(topic_model) |> 
  mutate(topic = as_factor(topic))

# Escolha de top words
top_words <- tidybeta  |> 
  arrange(desc(beta))  |> 
  group_by(topic) |> 
  top_n(10, beta) |> 
  summarise(terms = paste(term, collapse = ",")) |> 
  ungroup()

# Extração Gamma####
# Matriz Gamma
tidygamma <- tidytext::tidy(topic_model, 
                            matrix = "gamma") |> 
  right_join(covars, by = c("document" = "doc_id")) |> 
  arrange(topic, desc(gamma)) |> 
  mutate(topic = as_factor(topic))

# 
gamma_words <- tidygamma |> 
  group_by(topic) |> 
  summarise(gamma = mean(gamma)) |> 
  arrange(desc(gamma)) |> 
  left_join(top_words, by = "topic") |> 
  mutate(topic = reorder(topic, gamma))

# Gráfico gamma #### 
gamma_words |> 
  ggplot(aes(topic, gamma, 
             label = terms, 
             fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_text(hjust = 0, nudge_y = -0.000001, size = 6) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.038)) +
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

# Rotulação de categorias ####
# Ver arquivo topic_model80.txt ou similar
# Categorias 
categorias <- dplyr::tibble(
  topic = as_factor(unlist(list(
    c(3,9,12,18,20,25,30,32,33,39,45,47,48,49,50,56,68,75), # Política OK
    c(14,15,21,23,51,53,55,57,65,69,73,76), # Metafísica OK (46)
    c(5,13,28,29,42,62,71,77,79), # Fenomenologia OK
    c(27,40,41,58,64,70,72,74,78), # Mente e Linguagem OK 
    c(8,16,17,22,26,31,35,44,63,66,67), # Filosofia da ciência OK
    c(4,11,24,36,37,54,60,61), # Estética OK
    c(6,7,19,34,43,52,59), # Ética OK
    c(1,2,10,38,46,80) # Excluir
  ))),
  category = as_factor(c(
    rep("Social and Political Philosophy", 18),
    rep("Metaphysics", 12),
    rep("Phenomenology and Hermeneutics", 9),
    rep("Philosophy of Mind and Language", 9),
    rep("Philosophy of Science", 11),
    rep("Aesthetics", 8),
    rep("Ethics", 7),
    rep("Excluded", 6)
  )
  ))

# Inclusão das categorias na matrix gamma
tidygamma <- tidygamma |> 
  left_join(categorias, by = "topic") 

# Tabelão####
# Categoria | Tópico | Beta | 5 trabalhos | Gamma 
tabelao <- tidygamma |> 
  mutate(document = as.integer(document)) |> 
  left_join(dados,
            by = c("document" = "doc_id")) |> # Unifica o banco dados com a matrix gamma 
  group_by(topic) |> 
  slice_max(order_by = gamma, n = 10) |> # Encontra os docs mais representativo de cada tópico (com maior gamma)
  mutate(nm_producao = paste(nm_producao, collapse = ", ")) |> 
  ungroup() |> 
  distinct(topic, .keep_all = TRUE) |> 
  select(category, topic, nm_producao) |> 
  left_join(gamma_words, by = "topic") |> 
  mutate(gamma = round(gamma * 100,2)) |> 
  select(category, topic, terms, nm_producao,gamma) |>  # Seleciona apenas as variáveis de interesse
  arrange(category, desc(gamma)) 

# Tabelão.doc####
# TABELA 01 - TOPIC | TERMS | GAMMA
tabelao1 <- tabelao |> 
  group_by(category) |>
  gt() |> 
  cols_hide(nm_producao) |> 
  opt_table_font(
    font = "Times New Roman") 

#Salvar
gtsave(tabelao1, 
       "tabelao1.docx", 
       path = "dados",
       vwidth = 1400,
       vheight = 1700)

# Cálculo prevalência categorias
total_categorias <- tabelao |> 
  summarize(total = sum(gamma), .by = c(category))
  
# Salvar tabela  
total_categorias |>
  readr::write_csv("dados/total_category.csv")

# Gráfico Beta ####
ordem_topicos <- tabelao$topic
tidybeta <- tidybeta |> 
  group_by(topic)  |> 
  top_n(5, beta) |> 
  ungroup() |> 
  mutate(topic = factor(topic, levels = ordem_topicos),
         term = reorder_within(term, beta, topic)) |>  
  left_join(categorias, by = "topic") |> 
  filter(category != "Excluded")

tidybeta |> 
  ggplot(aes(x = term, 
             y = beta, 
             fill = category)) +
  theme_classic() +
  geom_col(alpha = 0.9) +
  scale_fill_manual(values = met.brewer("Cross", 7)) +
  facet_wrap(~ topic, scales = "free", ncol = 5) +
  labs(x = "",
       y = "",
       fill = "") +
  theme(legend.position = "top",
        text = element_text(size = 25),
        axis.text.x = element_blank()) +
  coord_flip() +
  scale_x_reordered() 

# Salvar
ggsave(
  "figs/stm_tabelaobeta.png",
  bg = "white",
  width = 19,
  height = 30,
  dpi = 900,
  plot = last_plot())

# TABELA 01 - TOPIC | TERMS | GAMMA
tabelao2 <- tabelao |> 
  select(category, topic, nm_producao) |> 
  group_by(category) |>
  gt() |> 
  opt_table_font(
    font = "Times New Roman") 

#Salvar
gtsave(tabelao2, 
       "tabelao2.docx", 
       path = "dados",
       vwidth = 1400,
       vheight = 1700)

# UMAP ####
# Preparação do banco
# Exclusão de Excluídos para análise 
tidygamma <- tidygamma |> 
  filter(category != "Excluded")

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
umap_model <- prep(umap_recipe)

# Gráfico UMAP####
juice(umap_model)  |> 
  ggplot(aes(UMAP1, UMAP2)) +
  geom_point(aes(fill = category), alpha = .8, size = 6, shape = 21) +
 # geom_text(aes(label = topic), check_overlap = TRUE, size = 3, color = "white") +
  scale_fill_manual(values = met.brewer("Cross", 7)) +
  theme_classic() +
  labs(fill = "") +
  theme(legend.position = "bottom",
        text = element_text(size = 16)) 

ggsave(
  "figs/stm_umap.png",
  bg = "white",
  width = 11,
  height = 8,
  dpi = 1200,
  plot = last_plot())

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
  filter(category != "Excluded")

# Gráfico ano-tópicos#### 
ordem_topicos <- tabelao$topic
 stm_ano |> 
   mutate(topic = factor(topic, levels = ordem_topicos)) |> 
  ggplot(aes(x = covariate.value,
             y = scale(estimate),
             ymin = ci.lower,
             ymax = ci.upper,
             color = category)) +
  facet_wrap(~topic, ncol = 6) +
  theme_classic() +
  geom_line(linewidth = 2) +
  labs(x = "Year(1991-2021)",
       y = "Point Estimation",
       color = "") +
  scale_color_manual(values = met.brewer("Cross", 7)) +
  theme(legend.position = "top",
        text = element_text(size = 30),
        axis.text.x = element_blank())

 ggsave(
   "figs/stm_topics-ano.png",
   bg = "white",
   width = 18,
   height = 20, #Limite
   dpi = 1200,
   plot = last_plot())
 
# Tabela da variação dos tópicos no tempo#### 
trends_ano <- stm_ano |> 
   arrange(covariate.value, 
           desc(estimate)) |> 
   group_by(covariate.value) |> 
   slice_head(n = 10) |> 
   mutate(topic = as.numeric(topic),
          covariate.value = substr(as.character(covariate.value),
                                   3,
                                   nchar(as.character(covariate.value))),
          row = row_number()) |> 
   select(row, topic, covariate.value) |> 
   tidyr::pivot_wider(names_from = covariate.value,
                      values_from  = topic) |> 
   select(-row)
# Tabelao ano
tabelao_ano <- trends_ano |> 
   gt() |> 
   opt_table_font(
     font = "Times New Roman") 

#Salvar
gtsave(tabelao_ano, 
       "tabelao_ano.docx", 
       path = "dados",
       vwidth = 2400,
       vheight = 1700)
 
# Gráfico Tempo-Categoria-Trabalho####
# Rotulação de cada documento por categoria
categorias_tempo <- tidygamma  |> 
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
categorias_tempo_ano <- categorias_tempo |> 
  summarize(n = n(), .by = c(an_base, category)) # Número de trabalhos

# Gráfico
categorias_tempo_ano |>
  ggplot(aes(x = an_base,
             y = n,
             color = category)) +
  geom_point(alpha=0.6, size=2) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              se = FALSE,
              linewidth = 2) +
  theme_classic() +
  labs(x = "",
       y = "Dissertations",
       color = "") +
  scale_color_manual(values = met.brewer("Cross", 7)) +
  scale_y_continuous(limits = c(0, 200), position = "right") +
  scale_x_continuous(limits = c(1990, 2021)) +
  guides(color = guide_legend(override.aes = list(size = 3))) +
  theme(legend.position = "top",
        plot.title = element_markdown(face = "bold"),
        text = element_text(size = 28))

ggsave(
  "figs/stm_category_ano.png",
  bg = "white",
  width = 17,
  height = 11,
  dpi = 1200,
  plot = last_plot())

# Salvar tabela  
evolucao_categorias <- categorias_tempo_ano |> 
  group_by(an_base) |> 
  mutate(frequencia = round(n/sum(n)*100,2))

evolucao_categorias |>
  readr::write_csv("dados/evolucao_categorias.csv")

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
  mutate(total = sum(estimate),
         topic = as.factor(topic)) |> 
  group_by(topic, covariate.value) |> 
  mutate(proporcao = round(estimate/sum(total)*100,2),
         covariate.value = recode(covariate.value,
                                  "Female" = "Woman",
                                  "Male" = "Man")) |> 
  arrange(covariate.value, desc(proporcao)) |> 
  left_join(categorias, by = "topic")  |> 
  ungroup()
  

# Gráfico Tópico-gênero####
prop_topicgenero |>
  ggplot(aes(x = fct_inorder(as_factor(topic)),
             y = proporcao,
             fill = covariate.value)) +
  geom_col() +
  geom_hline(yintercept = 50, color = "white") +
  scale_y_continuous(expand = c(0,1)) +
  theme_classic() +
  coord_flip() +
  scale_fill_manual(values = met.brewer("Austria", 2))  +
  labs(x = "",
       y = "%",
       fill = "") +
  theme(legend.position = "top",
        text = element_text(size = 25))

ggsave(
  "figs/stm_topic-genero.png",
  bg = "white",
  width = 17,
  height = 21,
  dpi = 1200,
  plot = last_plot())

# Tabela proporção
tabela_topicgenero <- prop_topicgenero |> 
  filter(covariate.value == "Woman" & category != "Excluded") |> 
  select(category, topic, proporcao) |> 
  arrange(desc(proporcao)) |> 
  gt() |>  
  opt_table_font(font = "Times New Roman") 

# Salvar
gtsave(tabela_topicgenero, 
       "tabelao_genero.docx", 
       path = "dados",
       vwidth = 2400,
       vheight = 1700)


# Gráfico Gênero-Categoria####
# Inclusão das categorias
prop_catgenero <- stm_genero |> 
  mutate(topic = as_factor(topic))  |> 
  left_join(categorias, by = "topic") |> 
  filter(category != "Excluídos") |> 
  group_by(category, covariate.value) |>
  summarise(total = sum(estimate)) |> 
  mutate(proporcao = round(total/sum(total)*100,2)) |> 
  arrange(covariate.value, desc(proporcao)) |> 
  gt()

# Salvar
gtsave(prop_catgenero, 
       "tabela_genero_categorias.docx", 
       path = "dados",
       vwidth = 2400,
       vheight = 1700)

# Tabela | 12-12 tópicos-gênero####
tab_genero <- prop_topicgenero  |> 
  select(topic, covariate.value, label, proporcao)  |> 
  mutate(label = str_replace_all(label, "\\(Covariate Level: Male\\)", "")) |> 
  pivot_wider(names_from = covariate.value,
              values_from = c(topic, proporcao)) |> 
  left_join(categorias, by = c("topic_Woman" = "topic"))
  

# Prepara o gamma
tab_gamma <- gamma_words |> 
  mutate(topic = as.numeric(topic))
  

# Tabela dos 12-12
tab_24 <- left_join(tab_genero,
                    gamma_words,
                    by = c("topic_Woman" = "topic")) |> 
  slice(1:12, 69:80) |> 
  select(-c(label, terms, topic_Man, proporcao_Man)) |> 
  mutate(gamma = round(gamma*100,3))

# TABELA 3 
tabela_genero12 <- tab_24 |> 
  gt() |> 
  cols_move_to_start(c(category, topic_Woman)) |> 
  cols_label(   # Títulos
    topic_Woman = "Topic",
    gamma = "\U03B3(%)",
    proporcao_Woman = "Woman (%)",
    category = "Category"
  ) |>
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  ) |> 
  opt_table_font(
  font = "Times New Roman") 

#Salvar
gtsave(tabela_genero12, 
       "tabela_genero.docx", 
       path = "dados",
       vwidth = 1400,
       vheight = 1700)
