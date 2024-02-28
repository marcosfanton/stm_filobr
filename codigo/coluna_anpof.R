####Pacotes####
library(tidyverse)
library(here)
library(MetBrewer) # Paleta de cores
library(scales) # Uso de porcentagem em gráficos
library(gt)

# Extração do banco de dados total - n. 12525 
dados <- read.csv("dados/catalogo.csv") |> 
  select(an_base, 
         nm_entidade_ensino, 
         nm_regiao, 
         nm_grau_academico, 
         sg_uf_ies, nm_producao, 
         ds_palavra_chave, 
         ds_resumo, 
         nr_paginas, 
         g_orientador, 
         g_discente, 
         g_oridis) |> 
  filter(an_base >= 1991)

# Tabela Descritiva#### 
# Sumarizar dados por região
dados_regiao <- dados |> 
  group_by(nm_regiao) |> 
  summarize(total = n()) |> 
  mutate(nm_regiao = recode(nm_regiao,
                            "norte" = "Norte",
                            "nordeste" = "Nordeste",
                            "centrooeste" = "Centro-Oeste",
                            "sudeste" = "Sudeste",
                            "sul" = "Sul"),
         frequencia = round(total / sum(total) * 100, 2),
         variavel = "Região") |> 
  arrange(desc(total)) |> 
  rename(categorias = nm_regiao) 

# Sumarizar dados por Grau Acadêmico
dados_nivel <- dados |> 
  group_by(nm_grau_academico) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total / sum(total) * 100, 2),
         variavel = "Nível",
         nm_grau_academico = str_to_title(nm_grau_academico)) |> 
  arrange(desc(total)) |> 
  rename(categorias = nm_grau_academico) 

# Sumarizar dados por Gênero Orientador
dados_orientador <- dados |> 
  group_by(g_orientador) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total / sum(total) * 100, 2),
         variavel = "Gênero do(a) Orientador(a)",
         g_orientador = recode(g_orientador,
                               "Female" = "Mulher",
                               "Male" = "Homem")) |> 
  arrange(desc(total)) |> 
  rename(categorias = g_orientador) 

# Sumarizar dados por Gênero Orientador
dados_discente <- dados |> 
  group_by(g_discente) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total / sum(total) * 100, 2),
         variavel = "Gênero do(a) Pesquisador(a)",
         g_discente = recode(g_discente,
                               "Female" = "Mulher",
                               "Male" = "Homem")) |> 
  arrange(desc(total)) |> 
  rename(categorias = g_discente) 

# Sumarizar dados por Relação de Orientação
dados_relacao <- dados |> 
  group_by(g_oridis) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total / sum(total) * 100, 2),
         variavel = "Relação de Orientação",
         g_oridis = recode(g_oridis,
                           "FF" = "Mulher/Mulher",
                           "FM" = "Mulher/Homem",
                           "MF" = "Homem/Mulher",
                           "MM" = "Homem/Homem"))|> 
  arrange(desc(total)) |> 
  rename(categorias = g_oridis) 

# Junção das tabelas 
tabela1 <- bind_rows(dados_nivel,
                     dados_regiao, 
                     dados_orientador,
                     dados_discente,
                     dados_relacao) 
  
tab1 <- 
  tabela1 |> 
  gt(groupname_col = "variavel",
     rowname_col = "categorias") |> 
  cols_label(
    total = "N",
    frequencia = "%",
    variavel = "Variável") |>  
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".",
    dec_mark = ",") |>   
  tab_header(
    title = md("**Tabela 1:** Características de trabalhos finais defendidos em PPG's de Filosofia no Brasil (1991-2021) | N: 12.353 trabalhos")
  ) |> 
  tab_source_note(
    source_note = "Elaboração: Dataphilo. Dados: CAPES."
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) |>
  tab_stub_indent(
    rows = everything(),
    indent = 5
  ) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) |> 
   sub_missing(
    columns = everything(),
    rows = everything(),
    missing_text = "Não Identificado"
  )  |> 
  cols_width(categorias ~ px(200),
             variavel ~ px(80),
             total ~ px(80),
             frequencia ~ px(80)) |>  
  opt_table_font(font = google_font(name = "Gentium Book Basic")) |> 
    tab_options(heading.title.font.size = px(12),
                table.font.size = px(12),
                row_group.padding = px(1),
                data_row.padding =  px(1.5),
                source_notes.font.size = px(10)) |> 
  tab_style(
  cell_borders(sides = c("bottom", "top"), color = "black"),
  locations = cells_title()) |>
  tab_style(
    cell_borders(sides = c("bottom", "top"), color = "black"),
    locations = cells_source_notes()) |> 
    opt_table_lines("none")
  
#Salvar
gtsave(tab1, 
       "tabela1_anpof.png", 
       path = "figs",
       expand = 20)

# Gráfico Relação Orientador-Pesquisador####
dados |>
  drop_na(g_oridis) |> 
  mutate(g_oridis = recode(g_oridis,
                           "FF" = "Mulher/Mulher",
                           "FM" = "Mulher/Homem",
                           "MF" = "Homem/Mulher",
                           "MM" = "Homem/Homem"),
         g_oridis = factor(g_oridis, levels = c("Mulher/Mulher", "Mulher/Homem", "Homem/Mulher", "Homem/Homem"))) |> 
  ggplot(aes(x = an_base, 
             fill = g_oridis)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Homem/Homem" = "#2d0b59",
                               "Homem/Mulher" = "#59106e",
                               "Mulher/Homem" = "#d24644",
                               "Mulher/Mulher" = "#e8602d")) +
  theme_classic() +
  labs(x = "",
       y = "",
       fill = "",
       title = "Desigualdade de Gênero na Pós-Graduação em Filosofia (1991-2021)",
       subtitle = "Proporção de defesas conforme relação de orientação: Orientador(a)/Pesquisador(a)",
       caption = "Elaboração: Dataphilo | Dados: CAPES") + 
  scale_x_continuous(limits = c(1990, 2022)) +
  scale_y_continuous(labels=scales::percent, position = "right") +
  theme(legend.position = "top",
        text = element_text(size = 22, family = "Gentium Book Plus"),
        plot.title = element_markdown(size = 22, face = "bold", hjust = 0.5),
        plot.subtitle = element_markdown(size = 20, hjust = 0.5),
        legend.text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off')  # Permite dados além dos limites do gráfico (seta,p.ex.)

# Salvar gráfico
ggsave(
  "figs/graf_relacao_anpof.png",
  bg = "white",
  width = 11,
  height = 6,
  dpi = 1200,
  plot = last_plot())

# Tabela Tópicos ##### 
traducao <- c(
  "Modern Epistemology" = "Epistemologia Moderna",
  "Nietzsche-Ethics" = "Ética nietzscheana",
  "Kant-Metaphysics" = "Metafísica kantiana",
  "Kant-Ethics" = "Ética kantiana",
  "Plato I" = "Platão I",
  "Philosophy Of Religion" = "Filosofia da Religião",
  "Democracy" = "Democracia",
  "Aristotle-Ethics" = "Ética aristotélica",
  "Hegel And Husserl" = "Hegel e Husserl")

tabela2 <- tabelao |> 
  slice_max(gamma, n = 15) |> 
  select(labels, terms, gamma) |> 
  mutate(
    labels = str_to_title(labels),
    labels = str_replace_all(labels, traducao),
    terms = str_replace_all(terms, ",", ", "))

tab2 <- 
  tabela2 |> 
  gt() |> 
  cols_label(
    labels = "Tópicos",
    #category = "Categoria",
    terms = "5 termos mais prováveis (\U03B2)",
    gamma = "\u03b3 (%)") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".",
    dec_mark = ",") |>   
  tab_header(
    title = md("**Tabela 2:** Os 15 Tópicos mais pesquisados em trabalhos finais da Pós-Graduação em Filosofia no Brasil (1991-2021)"),
    subtitle = NULL
  ) |> 
  tab_source_note(
    source_note = "Elaboração: Dataphilo. Dados: CAPES."
  ) |> 
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_row_groups()) |>
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()) |> 
  tab_style(
    style = cell_text(
      size = px(20)
    ),
    locations = cells_title("title")) |> 
  cols_width(terms ~ px(300),
             labels ~ px(150),
             gamma ~ px(80)) |> opt_table_font(font = google_font(name = "Gentium Book Basic")) |> 
  tab_options(heading.title.font.size = px(12),
              table.font.size = px(12),
              row_group.padding = px(1),
              data_row.padding =  px(1.5),
              source_notes.font.size = px(10)) |> 
  tab_style(
    cell_borders(sides = c("bottom", "top"), color = "black"),
    locations = cells_title()) |>
  tab_style(
    cell_borders(sides = c("bottom", "top"), color = "black"),
    locations = cells_source_notes()) |> 
  opt_table_lines("none") |> 
  opt_row_striping()

#Salvar
gtsave(tab2, 
       "tabela2_anpof.png", 
       path = "figs",
       expand = 20)


# Tradução das categorias, se quiser. 

category = case_when( # Transformar as categorias em português
  category == "Phenomenology and Hermeneutics" ~ "Fenomenologia e Hermenêutica",
  category == "Ethics" ~ "Ética",
  category == "Social and Political Philosophy" ~ "Filosofia Social e Política",
  category == "Metaphysics" ~ "Metafísica",
  category == "Philosophy of Science" ~ "Epistemologia e Filosofia da Ciência",
  category == "Philosophy of Mind and Language" ~ "Filosofia da Mente e da Linguagem",
  TRUE ~ as.character(category)
),