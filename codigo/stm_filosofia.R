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
covars <- filowords |> dplyr::distinct(doc_id, an_base, g_orientador) #matriz de covariáveis

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
  mutate(topic = paste0("T", topic),
         topic = reorder(topic, gamma))

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
    c(3,9,11,12,18,20,25,30,32,33,39,45,46,47,48,49,57,69,77), # Política OK
    c(5,13,28,29,42,52,63,73,79), # Fenomenologia OK
    c(27,40,41,44,58,59,66,71,72,74,76), # Mente e Linguagem OK
    c(6,7,19,34,43,53,60,61), # Moral OK
    c(14,15,21,23,51,54,65,68,70,75,78), # Metafísica OK
    c(1,4,10,24,36,37,50,55,62), # Estética OK
    c(8,16,17,22,26,31,35,56,64,67,80), # Filosofia da ciência
    c(2,38) # Excluir
  ))),
  category = as_factor(c(
    rep("Social and Political Philosophy", 19),
    rep("Phenomenology and Hermeneutics", 9),
    rep("Philosophy of Mind and Language", 10),
    rep("Ethics", 8),
    rep("Metaphysics", 11),
    rep("Aesthetics", 9),
    rep("Philosophy of Science", 12),
    rep("Excluídos", 2)
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
umap_model <- prep(umap_recipe)

# Plot do UMAP
juice(umap_model)  |> 
  ggplot(aes(UMAP1, UMAP2)) +
  geom_point(aes(fill = category), alpha = .8, size = 6, shape = 21) +
  # geom_text(check_overlap = TRUE, size = 3, color = "white") +
  scale_fill_manual(values = met.brewer("Cross", 7)) +
  theme_minimal() +
  labs(title = "UMAP Projection of document-category relationships",
       subtitle = "Dissertations from Brazilian Philosophy Graduate Programs (1988-2021)",
       fill = "Category") +
  theme(plot.title = element_markdown(face = "bold"),
        legend.position = "bottom")

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
         label = str_replace_all(label, "\\(Covariate Level: 1988\\)", "")) |> 
  left_join(categorias, by = "topic") |> 
  filter(category != "Excluídos")




# Sumariza por categoria | COM EFEITOS####
stmcat_ano <- stm_ano |> 
  summarize(total = sum(estimate), 
            .by = c(category, covariate.value)) 

# Gráfico
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

# Gráfico ano | Tópicos
stm_ano |> 
ggplot(aes(x = covariate.value,
           y = estimate,
           ymin = ci.lower,
           ymax = ci.upper)) +
  facet_wrap(~topic) +
  theme_classic() +
  geom_ribbon(alpha = .7, color = "#7da7ea", fill = "#7da7ea") +
  geom_line(color = "#1d4497") +
  labs(x = "Ano",
       y = "Proporção Esperada de cada Tópico",
       title = "Efeito de estimação pontual e intervalos de confiança dos tópicos ao longo do tempo") +
  theme(legend.position = "none",
        plot.title = element_markdown(face = "bold"))

ggsave(
  "figs/filostm_80t_ano.png",
  bg = "white",
  width = 22,
  height = 14,
  dpi = 600,
  plot = last_plot())

#Cálculo da variação dos tópicos
calc_ano <- ext_stm_effect_ano |> 
  filter(covariate.value %in% c(1991, 2021)) |> 
  select(topic, covariate.value, estimate) |> 
  pivot_wider(names_from = "covariate.value", 
              values_from  = "estimate") |> 
  mutate(diferenca = round(`1991` - `2021`,4)) 

# Efeito gênero de orientador####
stm_genero <- stm::estimateEffect(1:80 ~ g_orientador, 
                                       stmobj = topic_model, 
                                       metadata = covars, 
                                       uncertainty = "Global")

tidystm_genero <- tidystm::extract.estimateEffect(x = stm_genero, 
                                                           covariate = "g_orientador", 
                                                           model = topic_model, 
                                                           method = "pointestimate",
                                                           labeltype = "prob",
                                                           n = 3)
# Cálculo da proporção de cada tópico
prop_tidystm_genero <- tidystm_genero |> 
  group_by(topic) |> 
  mutate(total = sum(estimate)) |> 
  group_by(topic, covariate.value) |> 
  mutate(proporcao = round(estimate/sum(total)*100,2)) |> 
  arrange(covariate.value, desc(proporcao)) 

# Gráfico gênero
prop_tidystm_genero |>
ggplot(aes(x = fct_inorder(as_factor(topic)),
           y = proporcao,
           fill = covariate.value)) +
  geom_col() +
  scale_y_continuous(labels = percent_format(scale = 1))+
  theme_classic() +
  coord_flip() +
  scale_fill_d3() +
  labs(x = "",
       y = "",
       title = "Distribuição de trabalhos orientados por <span style= 'color:#FF7F0EFF; font-size:12pt;'>**Homens**</span> e <span style= 'color:#1F77B4FF;font-size:12pt;'>**Mulheres**</span> entre os 80 tópicos") +
  theme(legend.position = "none",
        plot.title = element_markdown(face = "bold"),
        text = element_text(size = 10))

# Sumariza por categoria | SEM EFEITOS####
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

cat_count_ano <- cat_count |> 
  summarize(n = n(), .by = c(an_base, category))

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
