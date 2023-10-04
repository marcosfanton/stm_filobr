####Pacotes####
library(tidyverse)
library(here)
library(geomtextpath) # Labels nos gráficos
library(MetBrewer) # Paleta de cores
library(ggtext) # Config de textos
library(geobr) # Mapa Brasil

library(scales) # Uso de porcentagem em gráficos
library(embed) # UMAP
library(umap) # UMAP
library(recipes) # UMAP
library(gganimate) # Produção de gif
library(ggstream)  # Produção de grafico stream
library(gt) # Construção de tabelas

# Filtragem de observações com base na qualidade dos resumos e seleção de variáveis ####
# Importação do banco limpo em "limpeza_catalogo.R"
dados <- read.csv("dados/catalogo.csv") |> 
  select(an_base, 
         nm_grau_academico, 
         nm_entidade_ensino, 
         nm_regiao, 
         g_orientador, 
         g_discente, 
         g_oridis) |> 
  filter(nm_grau_academico != "mestrado profissional")

# Gráfico 01 | Evolução do n. de Teses e Dissertações Filosofia####
ev_total <-  dados |> 
  summarize(n = n(),
            .by = c(an_base, nm_grau_academico)) |> 
  bind_rows(dados |> 
              group_by(an_base)  |> 
              summarise(nm_grau_academico = "Total", n = n())) |> 
  mutate(nm_grau_academico = str_replace_all(nm_grau_academico,
                                             pattern = c("mestrado" = "Dissertation",
                                                         "doutorado" = "Thesis")))

# Gráfico evolução 
ev_total |> 
  filter(an_base >= 1991) |> 
ggplot(aes(x = an_base, y = n, color = nm_grau_academico)) +
  geom_point(alpha = 0.6) +
  geom_line(alpha = 0.6) +
  geom_labelsmooth(aes(label = nm_grau_academico), 
                   text_smoothing = 30, 
                   fill = "#F6F6FF",
                   method = "loess", 
                   formula = y ~ x,
                   hjust = 0.7,
                   size = 6, 
                   linewidth = 2, 
                   boxlinewidth = 0.6) +
  scale_color_manual(values = met.brewer("Degas", 3))  +
  scale_x_continuous(limits = c(1990, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(position = "right") +
  theme_classic() +
  labs(x = "",
       y = "") +
  theme(legend.position = "none",
        text = element_text(size = 30)) +
  coord_cartesian(clip = 'off')  # Permite dados além dos limites do gráfico (seta,p.ex.)

ggsave(
  "figs/stm_evoldefenses.png",
  bg = "white",
  width = 16,
  height = 10,
  dpi = 1200,
  plot = last_plot())

# Gráfico 02 | Mapa Brasil####
# Baixar dados populacionais por Estado
# Site: Elaboração: Atlas do Desenvolvimento Humano no Brasil. Pnud Brasil, Ipea e FJP, 2022.
# Site: Fontes: dados do IBGE e de registros administrativos, conforme especificados nos metadados disponíveis disponíveis em: http://atlasbrasil.org.br/acervo/biblioteca.

# Baixar mapa de regiões
regiao <- geobr::read_region(year = 2020)
# Sumarizar dados por região
dados_regiao <- dados |> 
  group_by(nm_regiao) |> 
  summarize(trabalhos = n()) |> 
  mutate(nm_regiao = recode(nm_regiao, # MANTER OS NOMES PARA FUNCIONAR
                            "centrooeste" = "Centro Oeste",
                            "nordeste" = "Nordeste",
                            "norte" = "Norte",
                            "sudeste" = "Sudeste",
                            "sul" = "Sul")) 
# Unificar bancos
regiao <- dplyr::left_join(regiao, 
                           dados_regiao, 
                           by = c("name_region" = "nm_regiao"))

# Gráfico - Regiao
ggplot(regiao) +
  geom_sf(aes(fill = trabalhos), color = "NA") +
  labs(size = 30) +
  theme_void() +
  theme(plot.title = element_markdown(face = "bold"),
        legend.position = "none") +
  scale_fill_distiller(palette = "Blues", direction = 1) +
  geom_sf_text(aes(label = trabalhos), size = 12) 

ggsave(
  "figs/fig2_brazilmap.png",
  bg = "white",
  width = 15,
  height = 10,
  dpi = 1200,
  plot = last_plot())

# Gráfico 3 | Desigualdade de gênero####
dados |> 
  mutate(g_oridis = recode(g_oridis,
                           "FF" = "W/W",
                           "FM" = "W/M",
                           "MF" = "M/W",
                           "MM" = "M/M")) |> 
  drop_na() |> 
  ggplot(aes(x = an_base, 
             fill = g_oridis)) +
  geom_bar(position = "fill") +
  theme_classic() +
  labs(x = "",
       y = "",
       fill = "Supervisor/Candidate") +
  scale_x_continuous(limits = c(1990, 2021)) +
  scale_y_continuous(labels=scales::percent, position = "right") +
  scale_fill_manual(values = met.brewer("Degas", 3))  +
  theme(legend.position = "top",
        legend.text=element_text(size=36),
        text = element_text(size = 36, family = "Times New Roman")) + 
  coord_cartesian(clip = 'off')  # Permite dados além dos limites do gráfico (seta,p.ex.)

ggsave(
  "figs/graf6.png",
  bg = "white",
  width = 17,
  height = 12,
  dpi = 300,
  plot = last_plot())


# Gráfico 02 | Descrição Orientadores ao longo do tempo####
#Contagem da média
dados |> 
  group_by(g_orientador) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))
# Gráfico
dados |> 
  ggplot(aes(x = an_base, 
             fill = g_orientador)) +
  geom_bar(position = "fill") +
  theme_void() +
  labs(title = "Gender Inequality in Philosophy Graduate Programs in Brazil",
       subtitle = "Dissertations supervised by <span style= 'color:#16317d; font-size:20pt;'>**Men**</span> and <span style= 'color:#a40000;font-size:20pt;'>**Women**</span> (1991-2021)",
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Austria", 2))  +
  scale_x_continuous(limits = c(1990, 2021), expand = c(0, 0)) +
  scale_y_continuous(position = "right") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.1),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.09),
        legend.position = "none",
        axis.text.y=element_blank(),
        text = element_text(size = 20)) +
  geom_richtext(aes(x = 2017, 
                    y = 0.4, 
                    label ="&#187;Mean:80.3%&#171;"),
                stat = "unique",
                size = 10,
                fill = "#F6F6FF",
                color = "black")

ggsave(
  "figs/stm_generoano.png",
  bg = "white",
  width = 16,
  height = 12,
  dpi = 1200,
  plot = last_plot())

# Gráfico 03 | Palavras mais frequentes em keywords####
# Diagnóstico de palavras mais frequentes 
filo_freqwords <- dados |> 
  tidytext::unnest_tokens(output = word, input = ds_palavra_chave) |> # Separação de palavras dos resumos
  filter(!str_length(word) <= 2) |> # Eliminação de palavras com 2 ou menos caracteres
  mutate(word = as_factor(word)) |> 
  count(word, sort = TRUE) |> # Contagem de palavras
  filter(n >= 100) |> 
  filter(!str_detect(word, "filosofia")) # Elimina a palavra filosofia

# Gráfico
filo_freqwords |> 
  top_n(25) |> 
  ggplot(aes(y = reorder(word,n), 
             x = n,
             fill = word)) +
  scale_x_continuous(expand = c(.01, .01)) +
  geom_col() +
  geom_text(aes(label = n),
            hjust = 1.2,
            color = "white",
            fontface = "bold",
            size = 10) +
  scale_fill_manual(values = met.brewer("Lakota", 25), guide = "none")  +
  theme_void() +
  labs(title = "Description of the Top 25 Most Frequent Keywords in Dissertations",
       x = "") +
  theme(plot.title = element_markdown(face = "bold"),
        axis.text.y = element_text(size = 25, hjust = 1),
        text = element_text(size = 18))

ggsave(
  "figs/stm_freqkeyword.png",
  bg = "white",
  width = 13,
  height = 14,
  dpi = 1200,
  plot = last_plot())

# Gráfico 04 | Modelo 80 Tópicos####
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
       title = "80 Topics with the Estimated Topic Proportion (&#947;)")
theme(plot.title = element_markdown(face = "bold"),
      plot.subtitle = element_markdown(),
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

# Gráfico 05 | Top 20 tópicos #### 
gamma_words |> 
  top_n(20, gamma) |> 
  ggplot(aes(topic, gamma, 
             label = terms, 
             fill = topic)) +
  geom_col(show.legend = FALSE) +
  geom_label(hjust = 0, 
             nudge_y = -0.005, 
             size = 10, 
             color = "white") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, 0.04),
                     labels = percent_format()) +
  theme_classic() +
  scale_fill_manual(values = met.brewer("Cross", 20))  +
  labs(x = NULL, 
       y = NULL,
       title = "20 Top Topics (&#947;)")+
  theme(plot.title = element_markdown(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none",
        text = element_text(size = 20)) 

# Salvar gráfico
ggsave(
  "figs/stm_umap_87cat.png",
  bg = "white",
  width = 12,
  height = 10,
  dpi = 900,
  plot = last_plot())

# Gráfico | Evolução Categorias-Ano-Trabalho####
# Banco de documentos-ano
doc_ano <- tidygamma |> 
  distinct(document, .keep_all = TRUE) |> 
  select(document, an_base)
# Rotulagem de cada documento por categoria com maior gamma médio
cat_ano <- tidygamma |> 
  group_by(document, category) |> 
  summarise(gamma = mean(gamma)) |> 
  slice_max(gamma, n = 1, with_ties = FALSE) |> 
  left_join(doc_ano, by = "document") |> 
  ungroup()
# Cálculo do número de trabalhos por ano
evol_cat <-  cat_ano |> 
  summarize(n = n(),
            .by = c(an_base, category)) 
# Gráfico
evol_cat |>
  ggplot(aes(x = an_base,
             y = n,
             color = category)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              se = FALSE,
              linewidth = 2) +
  scale_x_continuous(limits = c(1991, 2021)) +
  scale_y_continuous(position = "right")+
  theme_classic() +
  labs(x = "Year",
       y = "Dissertations",
       color = "Category",
       title = "Trends of Categories in Dissertations (1991-2021)")+
  scale_color_manual(values = met.brewer("Cross", 7)) +
  theme(legend.position = "top",
        plot.title = element_markdown(face = "bold"),
        text = element_text(size = 15))

# Salvar gráfico
ggsave(
  "figs/stm_trends_cat.png",
  bg = "white",
  width = 12,
  height = 10,
  dpi = 900,
  plot = last_plot())

# Tabela | 10 melhores e piores gênero####
tab_20 <- left_join(tab_80,
                    tab_gamma,
                    by = c("topic_Female" = "topic")) |> 
  slice(1:10, 71:80) |> 
  select(-c(terms, topic_Male)) |> 
  mutate(gamma = round(gamma*100,4))

# TABELA 3 
tabela_20 <- tab_20 |> 
  gt() |> 
  cols_move_to_start(topic_Female) |> 
  cols_label(   # Títulos
    topic_Female = "Topic",
    label = "Terms (\U03B2)",
    gamma = "\U03B3(%)",
    proporcao_Female = "Woman (%)",
    proporcao_Male = "Man (%)"
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

gtsave(tabela_20,
       "figs/stm_table_topicgender.png",
       vwidth = 2000, vheight = 3000)

# Tabelas | Categorias-Tópicos####
gamma_words <- tidygamma |> 
  group_by(topic) |> 
  summarise(gamma = mean(gamma)) |> 
  arrange(desc(gamma)) |> 
  left_join(top_words, by = "topic") |> 
  mutate(topic = reorder(topic, gamma))

gamma_words <- gamma_words |> left_join(categorias,
                                        by = "topic") 

# Tabela Filosofia da Ciência
tab_science <- gamma_words |>
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

gtsave(tab_science,
       "figs/stm_table_science.png",
       vwidth = 2000, vheight = 3000)

# Tabela Estética####
tab_estetica <- gamma_words |>
  filter(category == "Aesthetics") |> 
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
    title = "Topics sorted as Aesthetics") |> 
  cols_align(
    align = "left") |>
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

gtsave(tab_estetica,
       "figs/stm_table_aesthetics.png",
       vwidth = 2000, vheight = 3000)

# Tabela Política####
tab_fenomenologia <- gamma_words |>
  filter(category == "Social and Political Philosophy") |> 
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
    title = "Topics sorted as Social and Political Philosophy") |> 
  cols_align(
    align = "left") |>
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

gtsave(tab_politica,
       "figs/stm_table_politics.png",
       vwidth = 2000, vheight = 3000)

# Tabela Fenomenologia####
tab_fenomenologia <- gamma_words |>
  filter(category == "Phenomenology and Hermeneutics") |> 
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
    title = "Topics sorted as Phenomenology and Hermeneutics") |> 
  cols_align(
    align = "left") |>
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

gtsave(tab_fenomenologia,
       "figs/stm_table_phenomenology.png",
       vwidth = 2000, vheight = 3000)


# Tabela Ética####
tab_etica <- gamma_words |>
  filter(category == "Ethics") |> 
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
    title = "Topics sorted as Ethics") |> 
  cols_align(
    align = "left") |>
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

gtsave(tab_etica,
       "figs/stm_table_ethics.png",
       vwidth = 2000, vheight = 3000)

# Tabela Mente e Linguagem####
tab_linguagem <- gamma_words |>
  filter(category == "Philosophy of Mind and Language") |> 
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
    title = "Topics sorted as Philosophy of Mind and Language") |> 
  cols_align(
    align = "left") |>
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

gtsave(tab_linguagem,
       "figs/stm_table_language.png",
       vwidth = 2000, vheight = 3000)

# Tabela Metafísica####
tab_metafisica <- gamma_words |>
  filter(category == "Metaphysics") |> 
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
    title = "Topics sorted as Metaphysics") |> 
  cols_align(
    align = "left") |>
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

gtsave(tab_metafisica,
       "figs/stm_table_metaphysics.png",
       vwidth = 2000, vheight = 3000)

# Tabela Filosofia da Ciência####
tab_ciencia <- gamma_words |>
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
    title = "Topics sorted as Epistemology and Philosophy of Science") |> 
  cols_align(
    align = "left") |>
  tab_options(
    table_body.hlines.style = "none",
    column_labels.border.top.color = "black",
    column_labels.border.bottom.color = "black",
    table_body.border.bottom.color = "black"
  )

gtsave(tab_ciencia,
       "figs/stm_table_science.png",
       vwidth = 2000, vheight = 3000)
