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
  select(doc_id, an_base, nm_producao, nm_grau_academico, ds_palavra_chave, ds_resumo, nr_paginas, g_orientador, g_discente, g_oridis)

# Gráfico 01 | Evolução do n. de Teses e Dissertações Filosofia####
evol_total <-  dados |> 
  summarize(n = n(),
            .by = c(an_base, nm_grau_academico)) |> 
  bind_rows(dados  |> 
              group_by(an_base)  |> 
              summarise(nm_grau_academico = "total", n = n()))

# Gráfico evolução 
evol_total |> 
ggplot(aes(x = an_base, y = n, color = nm_grau_academico)) +
  geom_point(alpha = 0.4) +
  geom_line(alpha = 0.4) +
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
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(position = "right") +
  theme_classic() +
  labs(title = "Evolution of Defenses in Philosophy Graduate Programs over the Years",
       subtitle = "Theses and dissertations defended between 1991-2021 | n: 11.726",
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "none",
        text = element_text(size = 20)) +
  coord_cartesian(clip = 'off')  # Permite dados além dos limites do gráfico (seta,p.ex.)

ggsave(
  "figs/stm_trabalhosano.png",
  bg = "white",
  width = 16,
  height = 12,
  dpi = 1200,
  plot = last_plot())

# Gráfico 02 | Descrição Orientadores ao longo do tempo####
dados |> 
  group_by(g_orientador) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

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
# Diagnóstico de palavras mais frequentes#### 
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
                     limits = c(0, 0.038),
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
  "figs/stm_model80t_20.png",
  bg = "white",
  width = 22,
  height = 14,
  dpi = 600,
  plot = last_plot())
