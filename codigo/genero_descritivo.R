####Pacotes####
library(tidyverse)
library(here)
library(skimr)
library(MetBrewer)
library(ggforce)
library(ggridges)
library(extrafont)
library(ggtext)
library(ggstream)
library(janitor)
library(epiR)
library(gt)
library(janitor)
library(ggsci) # Paleta cores

# Banco - N: 1117943
dados <- read.csv("dados/catalogo/catalogo9121_raw.csv") 

# Transforma variáveis de interesse em categóricas
fatores <- c("nm_grande_area_conhecimento", 
             "nm_area_avaliacao", 
             "nm_grau_academico",
             "nm_entidade_ensino",
             "nm_regiao", 
             "sg_uf_ies", 
             "g_orientador", 
             "g_discente", 
             "g_oridis")

dados <- dados  |> 
  mutate(across(all_of(fatores), as.factor))

dados |>
  readr::write_csv("dados/catalogo/catalogo9121_genero.csv")

# Tabela 1 | Grande área e Humanas ####
# Cálculo por Grande Área####
dados_areas <- dados |> 
  group_by(nm_grande_area_conhecimento) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2)) 

# Cálculo por orientador
dados_areas_go <- dados |> 
  group_by(nm_grande_area_conhecimento, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_areas_go <- dados_areas_go |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o))

# Cálculo por discente
dados_areas_gd <- dados |> 
  group_by(nm_grande_area_conhecimento, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2))

dados_areas_gd <- dados_areas_gd |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d))

# Cálculo por oridis
dados_areas_god <- dados |> 
  group_by(nm_grande_area_conhecimento, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

dados_areas_god <- dados_areas_god |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Tabela 1 | Agrupamento Grande Área####
lista_grande_area <- list(dados_areas, 
                          dados_areas_go, 
                          dados_areas_gd, 
                          dados_areas_god)

tab_grande_area <- purrr::reduce(lista_grande_area, 
                                 left_join, 
                                 by = "nm_grande_area_conhecimento") |> 
  mutate(descritor = "Grande Área") |> 
  rename("areas" = "nm_grande_area_conhecimento")

# Tabela 1 | Cálculo por Humanas ####
dados_humanas <- dados |> 
  filter(nm_grande_area_conhecimento == "Ciências Humanas") |> 
  mutate(nm_area_avaliacao = droplevels(nm_area_avaliacao)) |> 
  

# Cálculo Total
dados_humanas_total <- dados_humanas |> 
  group_by(nm_area_avaliacao) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Cálculo por orientador
dados_humanas_go <- dados_humanas |> 
  group_by(nm_area_avaliacao, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_humanas_go <- dados_humanas_go |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o))

# Cálculo por discente
dados_humanas_gd <- dados_humanas |> 
  group_by(nm_area_avaliacao, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2))

dados_humanas_gd <- dados_humanas_gd |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d))

# Cálculo por Orientador-Discente
dados_humanas_god <- dados_humanas |> 
  group_by(nm_area_avaliacao, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

dados_humanas_god <- dados_humanas_god |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Tabela 1 | Agrupamento Humanas ####
lista_humanas <- list(dados_humanas_total, 
                      dados_humanas_go, 
                      dados_humanas_gd, 
                      dados_humanas_god)

tab_humanas <- purrr::reduce(lista_humanas, 
                                 left_join, 
                                 by = "nm_area_avaliacao") |> 
  mutate(descritor = "Ciências Humanas")|> 
  rename("areas" = "nm_area_avaliacao")

# Tabela 1 | União tab_grande_area + tab_humanas####
tab <- bind_rows(tab_grande_area, tab_humanas)

# TABELA 01####                                                             
tab |> 
  gt(rowname_col = "areas",
     groupname_col = "descritor") |>
  cols_merge(
    columns = c(total, frequencia), # Total
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_o_Male, frequencia_o_Male), # Orientadores Homens
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_o_Female, frequencia_o_Female), # Orientadoras Mulheres
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_d_Male, frequencia_d_Male), # Discentes Homens
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_d_Female, frequencia_d_Female), # Discentes Mulheres
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_Female/Female, frequencia_od_Female/Female), # Mulher-Mulher
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_Female/Male, frequencia_od_Female/Male), # Mulher-Homem
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_Male/Female, frequencia_od_Male/Female), # Homem-Mulher
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_Male/Male, frequencia_od_Male/Male), # Homem-Homem
    pattern = "{1} ({2}%)") |>
  tab_spanner(   # Títulos
    label = "Orientador(a)",  
    columns = c(total_o_Male, total_o_Female)) |>
  tab_spanner(
    label = "Discente",
    columns = c(total_d_Male, total_d_Female)) |> 
  tab_spanner(
    label = "Orientador(a)/Discente",
    columns = c(total_god_Female/Female, total_god_Female/Male, total_god_Male/Female,total_god_Male/Male)) |> 
  cols_label(
    total = "Trabalhos",
    total_o_Male = "Homem",
    total_o_Female = "Mulher",
    total_d_Female = "Mulher",
    total_d_Male = "Homem",
    total_od_Female/Female = "Mulher/Mulher",
    total_od_Female/Male = "Mulher/Homem",
    total_od_Male/Female = "Homem/Mulher",
    total_od_Male/Male = "Homem/Homem"
  ) |> 
  tab_header(
    title = "Tabela 1. Descrição do gênero de orientadores e discentes das teses e dissertações defendidas no Brasil entre 1991-2021 de acordo com as Grandes Áreas da CAPES"
  ) |> 
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".") 

# Tabela 2 | Os 10 piores####

# Cálculo por discente
dados_piores_gd <- dados |> 
  group_by(nm_area_avaliacao, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2)) |> 
  ungroup()

# Organização e extração dos 10 piores cursos
dados_piores_gd <- dados_piores_gd |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d)) |> 
  slice_min(frequencia_d_Female, n = 10) |> 
  mutate(nm_area_avaliacao = droplevels(nm_area_avaliacao)) 

# Lista para filtrar os dados
lista_piores <- levels(dados_piores_gd$nm_area_avaliacao)

# Cálculo Total
dados_piores <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Cálculo por orientador
dados_piores_go <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_piores_go <- dados_piores_go |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o)) 

# Cálculo por Orientador-Discente
dados_piores_god <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

dados_piores_god <- dados_piores_god |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Tabela 2 | Agrupamento em um dataframe ####
lista_piores_df <- list(dados_piores, 
                      dados_piores_go, 
                      dados_piores_gd, 
                      dados_piores_god)

tab_piores <- purrr::reduce(lista_piores_df, 
                             left_join, 
                             by = "nm_area_avaliacao") |> 
  arrange(frequencia_d_Female) |> 
  mutate(nm_area_avaliacao = stringr::str_to_title(nm_area_avaliacao), 
         nm_area_avaliacao = stringr::str_replace(nm_area_avaliacao, "Iv", "IV"),
         nm_area_avaliacao = stringr::str_replace(nm_area_avaliacao, "Ciencia Da Computacao", "Ciência da Computação"),
         nm_area_avaliacao = stringr::str_replace(nm_area_avaliacao, "Fisica", "Física"),
         nm_area_avaliacao = stringr::str_replace(nm_area_avaliacao, "Iii", "III"),
         nm_area_avaliacao = stringr::str_replace(nm_area_avaliacao, "Matematica Probabilidade E Estatistica", "Matemática, Probabilidade E Estatística"),
         nm_area_avaliacao = stringr::str_replace(nm_area_avaliacao, "Administracao Ciencias Contabeis E Turismo", "Administração, Ciências Contábeis E Turismo"),
         nm_area_avaliacao = stringr::str_replace(nm_area_avaliacao, "Geociencias", "Geociências")) |> 
  rename("Áreas de Avaliação" = "nm_area_avaliacao")

# TABELA 2 ####
tab_piores |> 
  gt() |> 
  cols_hide(c(total, frequencia)) |> 
  cols_merge(
    columns = c(total_o_Male, frequencia_o_Male), # Orientadores Homens
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_o_Female, frequencia_o_Female), # Orientadoras Mulheres
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_d_Male, frequencia_d_Male), # Discentes Homens
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_d_Female, frequencia_d_Female), # Discentes Mulheres
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_FF, frequencia_od_FF), # Mulher-Mulher
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_FM, frequencia_od_FM), # Mulher-Homem
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_MF, frequencia_od_MF), # Homem-Mulher
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_MM, frequencia_od_MM), # Homem-Homem
    pattern = "{1} ({2}%)") |>
  tab_spanner(   # Títulos
    label = "Orientador(a)",  
    columns = c(total_o_Male, total_o_Female)) |>
  tab_spanner(
    label = "Discente",
    columns = c(total_d_Male, total_d_Female)) |> 
  tab_spanner(
    label = "Orientador(a)/Discente",
    columns = c(total_od_FF, total_od_FM, total_od_MF,total_od_MM)) |> 
  cols_label(
    total = "Trabalhos",
    total_o_Male = "Homem",
    total_o_Female = "Mulher",
    total_d_Female = "Mulher",
    total_d_Male = "Homem",
    total_od_FF = "Mulher/Mulher",
    total_od_FM = "Mulher/Homem",
    total_od_MF = "Homem/Mulher",
    total_od_MM = "Homem/Homem",
  ) |> 
  tab_header(
    title = "Tabela 2. Descrição das 10 áreas de avaliação com menor prevalência de teses e dissertações defendidas por mulheres discentes no Brasil entre 1991-2021"
  ) |> 
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".") 

# Tabela 3 | Universidades####

# Organização e extração dos 10 piores cursos
# Cálculo Total
dados_ies <- dadosfi |> 
  group_by(nm_entidade_ensino) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2)) |> 
  slice_max(total, n = 15)

# Lista para filtrar os dados
lista_ies <- levels(dados_ies$nm_entidade_ensino)

# Cálculo por discente
dados_ies_d <- dadosfi |> 
  filter(nm_entidade_ensino %in% lista_ies) |> 
  group_by(nm_entidade_ensino, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2)) |> 
  ungroup()

dados_ies_d <- dados_ies_d |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d)) 

# Cálculo por orientador
dados_ies_o <- dadosfi |> 
  filter(nm_entidade_ensino %in% lista_ies) |> 
  group_by(nm_entidade_ensino, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_ies_o <- dados_ies_o |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o)) 

# Cálculo por Orientador-Discente
dados_ies_god <- dadosfi |> 
  filter(nm_entidade_ensino %in% lista_ies) |> 
  group_by(nm_entidade_ensino, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

dados_ies_god <- dados_ies_god |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Tabela 3 | Agrupamento em um dataframe ####
lista_ies_df <- list(dados_ies, 
                     dados_ies_o, 
                     dados_ies_d, 
                     dados_ies_god)

tab_ies <- purrr::reduce(lista_ies_df, 
                         left_join, 
                         by = "nm_entidade_ensino") |> 
  mutate(nm_entidade_ensino = stringr::str_to_title(nm_entidade_ensino)) |> 
  mutate_all(~replace_na(.,0)) |> 
  rename("IES" = "nm_entidade_ensino") 

# TABELA 3 ####
tab_ies |> 
  gt() |> 
  cols_merge(
    columns = c(total, frequencia), # Total
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_o_Male, frequencia_o_Male), # Orientadores Homens
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_o_Female, frequencia_o_Female), # Orientadoras Mulheres
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_d_Male, frequencia_d_Male), # Discentes Homens
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_d_Female, frequencia_d_Female), # Discentes Mulheres
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_FF, frequencia_od_FF), # Mulher-Mulher
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_FM, frequencia_od_FM), # Mulher-Homem
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_MF, frequencia_od_MF), # Homem-Mulher
    pattern = "{1} ({2}%)") |>
  cols_merge(
    columns = c(total_od_MM, frequencia_od_MM), # Homem-Homem
    pattern = "{1} ({2}%)") |>
  tab_spanner(   # Títulos
    label = "Orientador(a)",  
    columns = c(total_o_Male, total_o_Female)) |>
  tab_spanner(
    label = "Discente",
    columns = c(total_d_Male, total_d_Female)) |> 
  tab_spanner(
    label = "Orientador(a)/Discente",
    columns = c(total_od_FF, total_od_FM, total_od_MF,total_od_MM)) |> 
  cols_label(
    total = "Trabalhos",
    total_o_Male = "Homem",
    total_o_Female = "Mulher",
    total_d_Female = "Mulher",
    total_d_Male = "Homem",
    total_od_FF = "Mulher/Mulher",
    total_od_FM = "Mulher/Homem",
    total_od_MF = "Homem/Mulher",
    total_od_MM = "Homem/Homem",
  ) |> 
  tab_header(
    title = "Tabela 3. Descrição do gênero de orientadores e discentes das teses e dissertações defendidas nas 10 IES mais produtivas no Brasil entre 1991-2021") |> 
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".") 

# GRÁFICOS#### 
# Evolução da pós-graduação no Brasil - 1991-2021####
theme_set(theme_minimal(base_family = "Roboto"))
# Gráfico 
dados |> 
  ggplot(aes(x = an_base)) +
  geom_freqpoly(binwidth = 1, linewidth = 1.2) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(limits = c(0, 100000), position = "right") +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação brasileira",
       subtitle = "Teses e Dissertações defendidas entre 1991-2021 | n: 1.270.0009 trabalhos",
       caption = "Elaboração: Os autores | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold"), #letra do título
        plot.subtitle = element_markdown(face = "bold"),
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off')  # Permite dados além dos limites do gráfico (seta,p.ex.)

#Gráfico | Total | PROFESSOR - LINHA #### 
dados |> 
  ggplot(aes(x = an_base, color = nm_grau_academico)) +
  geom_line(stat = "count", linewidth = 1.2) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 3)) +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação",
       subtitle = " Trabalhos orientados por pesquisadores <span style= 'color:#1d4497; font-size:24pt; font-weight: bold;'>Homens</span> e <span style= 'color:#b83326;font-size:24pt;'>Mulheres</span> entre 1991 e 2020",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off') 

# Gráfico | Total | Gênero | PROFESSOR - BARRA####
dados |> 
  drop_na(g_orientador) |> 
  ggplot(aes(x = g_orientador, y = an_base, fill = nm_grau_academico)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil | Grande Área",
       subtitle = "Proporção de trabalhos *orientados* por <span style= 'color:#1d4497; font-size:32pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:32pt;'>**Mulheres**</span> (1987-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1991, 2021), expand = c(0, 0)) +
  #scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 0), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35))

#Gráfico Total por Gênero do ALUNO - LINHA
dados |> 
  ggplot(aes(x = an_base, color = g_discente)) +
  geom_line(stat = "count", size = 1.2) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 2)) +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação",
       subtitle = "Trabalhos defendidos por pesquisadores <span style= 'color:#1d4497; font-size:24pt; font-weight: bold;'>Homens</span> e <span style= 'color:#b83326;font-size:24pt;'>Mulheres</span> entre 1991 e 2020",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        legend.position = "none",
        text = element_text(size = 25)) + 
  coord_cartesian(clip = 'off') 

#Gráfico Total por Gênero do ALUNO - BARRA
dados |> 
  drop_na(g_discente) |> 
  ggplot(aes(x = an_base, fill = g_discente)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil",
       subtitle = "Proporção de trabalhos *defendidos* por <span style= 'color:#1d4497; font-size:32pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:32pt;'>**Mulheres**</span> (1987-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1987, 2021), expand = c(0, 0)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35))

#Gráficos por GRANDE ÁREA####
#Gráfico Grande área por pesquisador
nomes_grandearea <- c("ciencias agrarias" = "Ciências Agrárias",
                      "ciencias biologicas" = "Ciências Biológicas",
                      "ciencias da saude" = "Ciências da Saúde",
                      "ciencias exatas e da terra" = "Ciências Exatas e da Terra",
                      "ciencias humanas" = "Ciências Humanas",
                      "ciencias sociais aplicadas" = "Ciências Sociais Aplicadas",
                      "engenharias" = "Engenharias",
                      "linguistica letras e artes" = "Linguística, Letras e Artes",
                      "multidisciplinar" = "Multidisciplinar")

# Gráfico de grande área PROFESSOR - LINHA
dados |> 
  ggplot(aes(x = an_base, group = g_orientador, color = g_orientador)) + 
  geom_line(stat = "count") +
  scale_x_continuous(limits = c(1990, 2021), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 2)) +
  theme_minimal() +
  facet_wrap(~nm_grande_area_conhecimento, labeller = as_labeller(nomes_grandearea)) +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação por Grande Área",
       subtitle = "Trabalhos orientados por pesquisadores <span style= 'color:#1d4497; font-size:24pt; font-weight: bold;'>Homens</span> e <span style= 'color:#b83326;font-size:24pt;'>Mulheres</span> entre 1990 e 2020",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        legend.position = "none",
        text = element_text(size = 35)) + 
  coord_cartesian(clip = 'off') 

# Gráfico Grande Área por Gênero do PROFESSOR - BARRA
dados |> 
  ggplot(aes(x = an_base, fill = g_orientador)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil",
       subtitle = "Proporção de trabalhos *orientados* por <span style= 'color:#1d4497; font-size:35pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:35pt;'>**Mulheres**</span> (1991-2021)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1991, 2022)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~nm_grande_area_conhecimento, labeller = as_labeller(nomes_grandearea)) 

# Gráfico Grande área por ALUNO - LINHA
dados |> 
  ggplot(aes(x = an_base, group = g_discente, color = g_discente)) + 
  geom_line(stat = "count") +
  scale_x_continuous(limits = c(1990, 2022), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 2)) +
  theme_minimal() +
  facet_wrap(~nm_grande_area_conhecimento, labeller = as_labeller(nomes_grandearea)) +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação por Grande Área",
       subtitle = "Trabalhos defendidos por pesquisadores <span style= 'color:#1d4497; font-size:24pt; font-weight: bold;'>Homens</span> e <span style= 'color:#b83326;font-size:24pt;'>Mulheres</span> entre 1991 e 2021",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        legend.position = "none",
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off') 


#Gráfico Grande Área por Gênero do ALUNO - BARRA
dados |> 
  ggplot(aes(x = an_base, fill = g_discente)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil | Grande Área",
       subtitle = "Proporção de trabalhos *defendidos* por <span style= 'color:#1d4497; font-size:35pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:35pt;'>**Mulheres**</span> (1991-2021)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1990, 2022)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~nm_grande_area_conhecimento, labeller = as_labeller(nomes_grandearea)) 

#10 piores de desigualdade para orientador####

#Gráfico PIORES ÁREAS  por Gênero do PROFESSOR - BARRA
dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |>  
  ggplot(aes(x = an_base, fill = g_orientador)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "10 Áreas de Avaliação com Alta Desigualdade de Gênero",
       subtitle = "Proporção de trabalhos *orientados* por <span style= 'color:#1d4497; font-size:30pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:30pt;'>**Mulheres**</span> (1991-2021)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1990, 2022), labels = NULL) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~nm_area_avaliacao, ncol = 2) 

#Gráfico PIORES ÁREAS por gênero do ALUNO - LINHA
dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |>  
  ggplot(aes(x = an_base, fill = g_discente)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "10 Áreas de Avaliação com Alta Desigualdade de Gênero",
       subtitle = "Proporção de trabalhos defendidos por <span style= 'color:#1d4497; font-size:32pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:32pt;'>**Mulheres**</span> (1991-2021)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1990, 2022), labels = NULL) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~nm_area_avaliacao, ncol = 2) 

#Gráfico HUMANAS por Gênero do PROFESSOR - BARRA
dados |> 
  filter(nm_grande_area_conhecimento == "ciencias humanas") |>  
  ggplot(aes(x = an_base, fill = g_orientador)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil | Ciências Humanas",
       subtitle = "Proporção de trabalhos *orientados* por <span style= 'color:#1d4497; font-size:35pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:35pt;'>**Mulheres**</span> (1987-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1990, 2022), expand = c(0.02, 0.02)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~nm_area_avaliacao) 


# Gráfico | Relação Professor-Aluno####
# Total
dados |> 
  ggplot(aes(x = an_base, 
             fill = g_oridis)) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1991-2021)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1990, 2022), expand = c(0, 0)) +
  scale_y_continuous(labels=scales::percent, position = "right") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), #letra do título
        plot.subtitle = element_markdown(size = 25, hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        legend.position = "top",
        axis.text.y = element_text(size = 15),
        legend.title=element_text(size=20), 
        legend.text = element_text(size = 15),
        text = element_text(size = 30))

# Relação Professor-Aluno Grandes Áreas
dados |> 
  ggplot(aes(x = an_base, 
             fill = g_oridis)) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1991-2021)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1990, 2022), expand = c(0, 0)) +
  scale_y_continuous(labels=scales::percent, position = "right") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), #letra do título
        plot.subtitle = element_markdown(size = 25, hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        legend.position = "top",
        axis.text.y = element_text(size = 15),
        legend.title=element_text(size=20), 
        legend.text = element_text(size = 15),
        text = element_text(size = 30)) +
  facet_wrap(~nm_grande_area_conhecimento) 

# Relação Professor-Aluno Humanidades
dados |> 
  filter(nm_grande_area_conhecimento == "ciencias humanas") |>  
  ggplot(aes(x = an_base, 
             fill = g_oridis)) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação das Ciências Humanas",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1991-2021)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1990, 2022)) +
  scale_y_continuous(labels=NULL, position = "right") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), #letra do título
        plot.subtitle = element_markdown(size = 25, hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        legend.position = "top",
        text = element_text(size = 25),
        axis.text.y = element_text(size = 15),
        legend.title=element_text(size=20), 
        legend.text = element_text(size = 15)) +
  facet_wrap(~nm_area_avaliacao) 

# Relação Professor-Aluno 10 Piores
dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |>  
  ggplot(aes(x = an_base, 
             fill = g_oridis)) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação das Ciências Humanas",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1990-2020)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1990, 2022)) +
  scale_y_continuous(labels=NULL, position = "right") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), #letra do título
        plot.subtitle = element_markdown(size = 25, hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        legend.position = "top",
        text = element_text(size = 25),
        axis.text.y = element_text(size = 15),
        legend.title=element_text(size=20), 
        legend.text = element_text(size = 15)) +
  facet_wrap(~nm_area_avaliacao) 


#RAZÃO DE PREVALÊNCIA#### 
dadosfil <- read.csv("dados/catalogo.csv") |> 
  drop_na(g_oridis)

matriz <- dadosfil |>  
  filter(between(an_base, 2011,2021)) |> 
  tabyl(g_orientador, g_discente) |> 
  adorn_totals(c("row", "col"))

dat.v <- matrix(c(470,1537,936,4156), ncol =2)

resultado <- epi.2by2(dat = dat.v, method = "cross.sectional",
         conf.level = 0.95, units = 100, outcome = "as.columns")

# FILOSOFIA #### 
# Uso do catálogo específico da Filosofia 
dadosfi <- readr::read_csv("dados/catalogo.csv") |> 
  drop_na(g_oridis)

# Transforma variáveis de interesse em categóricas
fatores <- c("nm_grau_academico",
             "nm_entidade_ensino",
             "nm_regiao", 
             "sg_uf_ies", 
             "g_orientador", 
             "g_discente", 
             "g_oridis")

dadosfi <- dadosfi  |> 
  mutate(across(all_of(fatores), as.factor))

# Gráfico | Evol Prop. Orientadora | Grandes áreas####
dados_evol_go <- dados |> 
  group_by(nm_grande_area_conhecimento, an_base, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_evol_go |> 
  filter(g_orientador == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia_o,
             color = nm_grande_area_conhecimento)) +
  geom_line(linewidth = 1.2, alpha = 0.3) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 9)) +
  labs(title = "Evolução da proporção de trabalhos orientados por mulheres na Pós-Graduação por Grande Área",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off')

# Gráfico | Evol Prop. Discente | Grandes áreas####
dados_evol_gd <- dados |> 
  group_by(nm_grande_area_conhecimento, an_base, g_discente) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_evol_gd |> 
  filter(g_discente == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia_o,
             color = nm_grande_area_conhecimento)) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous( position = "right") +
 # scale_colour_manual(values = met.brewer("Nizami", 9)) +
  labs(title = "Evolução da proporção de trabalhos defendidos por mulheres na Pós-Graduação por Grande Área",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off')

# Gráfico | Evol Prop. Discente | Humanas####
dados_evol_humanas <- dados |> 
  filter(nm_grande_area_conhecimento == "ciencias humanas") |> 
  mutate(nm_area_avaliacao = droplevels(nm_area_avaliacao)) |> 
  group_by(nm_area_avaliacao, an_base, g_discente) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_evol_humanas |> 
  filter(g_discente == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia_o,
             color = nm_area_avaliacao)) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous( position = "right") +
  #scale_colour_manual(values = met.brewer("Nizami", 9)) +
  labs(title = "Evolução da proporção de trabalhos defendidos por mulheres na Pós-Graduação nas Ciências Humanas",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off')


# Gráfico | Evol Prop. Discente | 10 piores####
dados_evol_piores <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, an_base, g_discente) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

dados_evol_piores |> 
  filter(g_discente == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = nm_area_avaliacao)) +
  geom_line(linewidth = 1.2, alpha = 0.2) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous( position = "right") +
  #scale_colour_manual(values = met.brewer("Nizami", 10)) +
  labs(title = "Evolução da proporção de trabalhos defendidos por mulheres na Pós-Graduação nas Ciências Humanas",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off')

# Avaliação aumento-diminuição em dados percentuais
avaliacao_percentual <- dados_evol_piores  |> 
  filter(an_base %in% c(1991, 2021) & g_discente == "Female")  |> 
  group_by(nm_area_avaliacao)  |> 
  arrange(an_base)  |> 
  summarise(diminuicao_perc = ((last(frequencia) - first(frequencia)) / first(frequencia)) * 100)


# Gráfico | Docente vs Discente mulher #### 
# Cálculo por orientador
dados_go <- dados |> 
  group_by(nm_grande_area_conhecimento, nm_area_avaliacao, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2)) |> 
  filter(g_orientador == "Female")

# Cálculo por discente
dados_gd <- dados |> 
  group_by(nm_grande_area_conhecimento, nm_area_avaliacao, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2)) |> 
  filter(g_discente == "Female") 

dados_god <- left_join(dados_go, 
                       dados_gd, 
                       by = c("nm_grande_area_conhecimento", "nm_area_avaliacao")) 

dados_god |> ggplot(aes(x = frequencia_o, 
                         y = frequencia_d)) +
  geom_point(aes(colour = nm_grande_area_conhecimento),
             shape = 20,
             size = 4.5) +
  ggrepel::geom_text_repel(aes(label = nm_area_avaliacao,
                               color = nm_grande_area_conhecimento),
                           show.legend = FALSE,
                           min.segment.length = .7,
                           box.padding = 0.3,
                           size = 5,
                           nudge_x = 0.1,
                           nudge_y = 1.6) +
  labs(title = "Prevalência de mulheres orientadoras e mulheres discentes entre as áreas de avaliação da CAPES (1991-2021)", 
       x = "Prevalência de mulheres orientadoras (%)",
       y = "Prevalência de mulheres discentes (%)",
       color = "Grande Área") +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_color_d3() +
  guides(colour = guide_legend(override.aes = list(size=8))) +
  theme_classic() +
  theme(plot.title = element_markdown(face = "bold"),
        legend.position = c(.85, .38),
        text = element_text(size = 20),
        legend.title.align = 0.25,
        legend.background = element_rect(color = "black", 
                                      linewidth = 0.5, 
                                      linetype = "solid")) 
