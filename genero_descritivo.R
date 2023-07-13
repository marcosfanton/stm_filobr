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

# Banco 
dados <- read.csv("dados/catalogo/catalogo9121_raw.csv")

# Recodificação dos níveis da filosofia e teologia
dados <- dados  |> 
  mutate(nm_area_avaliacao = recode(nm_area_avaliacao, 
                                     "filosofia teologiasubcomissao filosofia" = "filosofia", 
                                    "filosofiateologiasubcomissao filosofia" = "filosofia",
                                    "filosofiateologiasubcomissao teologia" = "teologia",
                                    "ciencias da religiao e teologia" = "teologia"))

# Transforma variáveis de interesse em fator
fatores <- c("nm_grande_area_conhecimento", 
             "nm_area_avaliacao", 
             "nm_grau_academico", 
             "nm_regiao", 
             "sg_uf_ies", 
             "g_orientador", 
             "g_discente", 
             "g_oridis")

dados <- dados  |> 
  mutate(across(all_of(fatores), as.factor))

# Tabela 1 ####
# Cálculo por Grande Área####
dados_areas <- dados |> 
  group_by(nm_grande_area_conhecimento) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Cálculo por orientador
dados_gorientador <- dados |> 
  group_by(nm_grande_area_conhecimento, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_gorientador <- dados_gorientador |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o))

# Cálculo por discente
dados_g_discente <- dados |> 
  group_by(nm_grande_area_conhecimento, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2))

dados_g_discente <- dados_g_discente |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d))

# Cálculo por oridis
dados_g_oridis <- dados |> 
  group_by(nm_grande_area_conhecimento, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

dados_g_oridis <- dados_g_oridis |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Agrupamento em um dataframe
lista_grande_area <- list(dados_areas, 
                          dados_gorientador, 
                          dados_g_discente, 
                          dados_g_oridis)

tab_grande_area <- purrr::reduce(lista_dados, 
                                 left_join, 
                                 by = "nm_grande_area_conhecimento") |> 
  mutate(nm_grande_area_conhecimento = stringr::str_to_title(nm_grande_area_conhecimento),
         nm_grande_area_conhecimento = stringr::str_replace(nm_grande_area_conhecimento, "Ciencias", "Ciências"),
         nm_grande_area_conhecimento = stringr::str_replace(nm_grande_area_conhecimento, "Biologicas", "Biológicas"),
         nm_grande_area_conhecimento = stringr::str_replace(nm_grande_area_conhecimento, "Saude", "Saúde"),
         nm_grande_area_conhecimento = stringr::str_replace(nm_grande_area_conhecimento, "Linguistica", "Linguística,"),
         descritor = "Grande Área") |> 
  rename("areas" = "nm_grande_area_conhecimento")

# Cálculo por Humanas ####
dados_humanas <- dados |> 
  filter(nm_grande_area_conhecimento == "ciencias humanas") |> 
  mutate(nm_area_avaliacao = droplevels(nm_area_avaliacao))

# Cálculo Total
dados_humanas_total <- dados_humanas |> 
  group_by(nm_area_avaliacao) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Cálculo por orientador
dados_humanas_gorientador <- dados_humanas |> 
  group_by(nm_area_avaliacao, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

dados_humanas_gorientador <- dados_humanas_gorientador |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o))

# Cálculo por discente
dados_humanas_g_discente <- dados_humanas |> 
  group_by(nm_area_avaliacao, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2))

dados_humanas_g_discente <- dados_humanas_g_discente |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d))

# Cálculo por Orientador-Discente
dados_humanas_g_oridis <- dados_humanas |> 
  group_by(nm_area_avaliacao, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

dados_humanas_g_oridis <- dados_humanas_g_oridis |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Agrupamento em um dataframe
lista_humanas <- list(dados_humanas_total, 
                          dados_humanas_gorientador, 
                          dados_humanas_g_discente, 
                          dados_humanas_g_oridis)

tab_humanas <- purrr::reduce(lista_humanas, 
                                 left_join, 
                                 by = "nm_area_avaliacao") |> 
  mutate(nm_area_avaliacao = stringr::str_to_title(nm_area_avaliacao),
         nm_area_avaliacao = stringr::str_replace(nm_area_avaliacao, "Ciencia Politica E Relacoes Internacionais", "Ciência Política E RI"),
         nm_area_avaliacao = stringr::str_replace(nm_area_avaliacao, "Antropologia Arqueologia", "Antropologia E Arqueologia"),
         nm_area_avaliacao = stringr::str_replace(nm_area_avaliacao, "Educacao", "Educação"),
         nm_area_avaliacao = stringr::str_replace(nm_area_avaliacao, "Historia", "História"),
         descritor = "Ciências Humanas")|> 
  rename("areas" = "nm_area_avaliacao")

# União dos tabs - tab_grande_area + tab_humanas

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
    title = "Tabela 1. Descrição do gênero de orientadores e discentes das teses e dissertações defendidas no Brasil entre 1991-2021 de acordo com as Grandes Áreas da CAPES"
  ) |> 
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".") 


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

#Gráfico Total por Gênero PROFESSOR
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

#Gráfico Total por Gênero do PROFESSOR - BARRA
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
  drop_na(g_discente) |> 
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

#Gráfico de grande área PROFESSOR - LINHA
dados |> 
  drop_na(g_orientador) |> 
  filter(grandearea != "") |> 
  ggplot(aes(x = ano, group = g_orientador, color = g_orientador)) + 
  geom_line(stat = "count") +
  scale_x_continuous(limits = c(1990, 2021), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 2)) +
  theme_minimal() +
  facet_wrap(~grandearea, labeller = as_labeller(nomes_grandearea)) +
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

#Gráfico Grande Área por Gênero do PROFESSOR - BARRA
dados |> 
  filter(grandearea != "") |> 
  drop_na(gorientador) |> 
  ggplot(aes(x = ano, fill = gorientador)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil",
       subtitle = "Proporção de trabalhos *orientados* por <span style= 'color:#1d4497; font-size:35pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:35pt;'>**Mulheres**</span> (1987-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  #scale_x_continuous(limits = c(1991, 2021)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~grandearea, labeller = as_labeller(nomes_grandearea)) 


#Gráfico Grande área por ALUNO - LINHA
dados |> 
  filter(grandearea != "") |> 
  drop_na(galuno) |> 
  ggplot(aes(x = ano, group = galuno, color = galuno)) + 
  geom_line(stat = "count") +
  scale_x_continuous(limits = c(1987, 2020), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 2)) +
  theme_minimal() +
  facet_wrap(~grandearea, labeller = as_labeller(nomes_grandearea)) +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação por Grande Área",
       subtitle = "Trabalhos defendidos por pesquisadores <span style= 'color:#1d4497; font-size:24pt; font-weight: bold;'>Homens</span> e <span style= 'color:#b83326;font-size:24pt;'>Mulheres</span> entre 1987 e 2020",
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
  filter(grandearea != "") |> 
  drop_na(galuno) |> 
  ggplot(aes(x = ano, fill = galuno)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil | Grande Área",
       subtitle = "Proporção de trabalhos *defendidos* por <span style= 'color:#1d4497; font-size:35pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:35pt;'>**Mulheres**</span> (1987-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  #scale_x_continuous(limits = c(1990, 2021)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~grandearea, labeller = as_labeller(nomes_grandearea)) 

#Percentual em todas áreas específicas para orientador####
areas_desiguais <- dados |> 
  group_by(codigoarea) |> 
  filter(n() > 1500) |>  #Com mais de 1500 trabalhos
  tabyl(codigoarea, gorientador, show_na = FALSE) |>  #NAs da variável gorientador vai diminuir o n. de observações
  adorn_totals(c("row", "col")) |> 
  adorn_percentages() |> 
  adorn_ns() |> 
  arrange(Female)

#Vinte piores de desigualdade para orientador
nomes_20piores <- c(#"30400007" = "Engenharia Elétrica",
  #"40102009" = "Cirurgia",
  "30500001" = "Engenharia Mecânica",
  #"31200001" = "Engenharia Aeroespacial",
  "10500006" = "Física",
  "60300000" = "Economia",
  #"50200003" = "Engenharia Florestal",
  "50300008" = "Engenharia Agrícola",
  "71000003" = "Teologia",
  #"30800005" = "Engenharia de Produção",
  "70100004" = "Filosofia",
  "10100008" = "Matemática",
  "30300002" = "Engenharia Metalúrgica",
  "30100003" = "Engenharia Civil",
  #"10700005" = "Geociências",
  #"31000002" = "Engenharia Transportes",
  "10300007" = "Ciências da Computação",
  #"90300009" = "Biotecnologia",
  "50100009" = "Agronomia",
  "60100001" = "Direito")
vinte_piores <- c(#30400007, #Engenharia Elétrica
  #40102009, #Cirurgia
  30500001, #E Mecânica
  #31200001, #E Aeroespacial
  10500006, #Física
  60300000, #Economia
  #50200003, #E Florestal
  50300008, #E Agrícola
  71000003, #Teologia
  #30800005, #E de Produção
  70100004, #Filosofia
  10100008, #Matemática
  30300002, #E Metalúrgica
  30100003, #E Civil
  #10700005, #Geociências
  #31000002, #E Transportes
  10300007, #C da Computação
  #90300009, #Biotecnologia
  50100009, #Agronomia
  60100001)

#Gráfico PIORES ÁREAS  por Gênero do PROFESSOR - BARRA
dados1 |> 
  filter(codigoarea %in% vinte_piores) |>  
  drop_na(gorientador) |> 
  ggplot(aes(x = ano, fill = gorientador)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "12 Áreas de Avaliação com Alta Desigualdade de Gênero",
       subtitle = "Proporção de trabalhos *orientados* por <span style= 'color:#1d4497; font-size:30pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:30pt;'>**Mulheres**</span> (1995-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1995, 2021), labels = NULL) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~codigoarea, labeller = as_labeller(nomes_20piores), ncol = 3) 

#Gráfico PIORES ÁREAS por gênero do ALUNO - LINHA
dados |> 
  filter(codigoarea %in% vinte_piores) |>  
  drop_na(galuno) |> 
  ggplot(aes(x = ano, color = galuno)) + 
  geom_line(stat = "count") +
  scale_x_continuous(limits = c(1995, 2020), breaks = seq(1995, 2020, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 2)) +
  theme_minimal() +
  facet_wrap(~codigoarea, labeller = as_labeller(nomes_20piores)) +
  labs(title = "As 16 Áreas de Avaliação com Maior Desigualdade de Gênero",
       subtitle = "Trabalhos orientados por pesquisadores <span style= 'color:#1d4497; font-size:24pt; font-weight: bold;'>Homens</span> e <span style= 'color:#b83326;font-size:24pt;'>Mulheres</span> entre 1987 e 2020",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        legend.position = "none",
        text = element_text(size = 20)) + 
  coord_cartesian(clip = 'off') 

#Gráfico PIORES ÁREAS por gênero do ALUNO - BARRA
dados |> 
  filter(codigoarea %in% vinte_piores) |>  
  drop_na(galuno) |> 
  ggplot(aes(x = ano, fill = galuno)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "As 16 Áreas de Avaliação com Maior Desigualdade de Gênero",
       subtitle = "Proporção de trabalhos defendidos por <span style= 'color:#1d4497; font-size:32pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:32pt;'>**Mulheres**</span> (1991-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1995, 2021), expand = c(0, 0)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 20)) +
  facet_wrap(~codigoarea, labeller = as_labeller(nomes_20piores)) 

#Ciências Humanas#### 
#Nomes das Ciências Humanas
nomes_humanidades <- c("70900000" = "Ciência Política",
                       "70700001" = "Psicologia",
                       "70600007" = "Geografia",
                       "70800006" = "Educação",
                       "70500002" = "História",
                       #"70400008" = "Arqueologia",
                       "70300003" = "Antropologia",
                       "70200009" = "Sociologia",
                       "71000003" = "Teologia",
                       "70100004" = "Filosofia") 

#Áreas Específicas das Ciências Humanas
humanidades_codigo  <-  c(70900000, #Ciência Política
                          70800006, #Educação
                          70700001, #Psicologia
                          70600007, #Geografia
                          70500002, #História
                          #70400008, #Arqueologia
                          70300003, #Antropologia
                          70200009, #Sociologia
                          71000003, #Teologia
                          70100004)   #Filosofia  

#Gráfico HUMANAS por Gênero do PROFESSOR - BARRA
dados |> 
  filter(codigoarea %in% humanidades_codigo) |>  
  drop_na(gorientador) |> 
  ggplot(aes(x = ano, fill = gorientador)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil | Ciências Humanas",
       subtitle = "Proporção de trabalhos *orientados* por <span style= 'color:#1d4497; font-size:35pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:35pt;'>**Mulheres**</span> (1987-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1987, 2020), expand = c(0.02, 0.02)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 35)) +
  facet_wrap(~codigoarea, labeller = as_labeller(nomes_humanidades)) 


#Gráfico das HUMANAS por ALUNO - LINHA
dados |> 
  filter(codigoarea %in% humanidades_codigo) |>  
  drop_na(galuno) |> 
  ggplot(aes(x = ano, color = galuno)) + 
  geom_line(stat = "count") +
  scale_x_continuous(limits = c(1990, 2020), breaks = seq(1990, 2020, 5)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 2)) +
  theme_minimal() +
  facet_wrap(~codigoarea, labeller = as_labeller(nomes_humanidades)) +
  labs(title = "Ciências Humanas e a Desigualdade de Gênero",
       subtitle = "Trabalhos defendidos por pesquisadores <span style= 'color:#1d4497; font-size:14pt; font-weight: bold;'>Homens</span> e <span style= 'color:#b83326;font-size:14pt;'>Mulheres</span> entre 1987 e 2020",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        legend.position = "none",
        text = element_text(size = 14)) + 
  coord_cartesian(clip = 'off') 

#Gráfico das HUMANAS por ALUNO - BARRA
dados |> 
  filter(codigoarea %in% humanidades_codigo) |>  
  drop_na(galuno) |> 
  ggplot(aes(x = ano, fill = galuno)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação do Brasil | Ciências Humanas",
       subtitle = "Proporção de trabalhos *defendidos* por <span style= 'color:#1d4497; font-size:35pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:35pt;'>**Mulheres**</span> (1987-2020)",
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
        text = element_text(size = 35)) +
  facet_wrap(~codigoarea, labeller = as_labeller(nomes_humanidades)) 

#Gráficos Relação Professor-Aluno####
#Total
dados |> 
  drop_na(goga) |> 
  filter(ano > 1990) |> 
  ggplot(aes(x = ano, 
             fill = factor(goga, levels = c("Mulher/Mulher", "Mulher/Homem", "Homem/Mulher", "Homem/Homem")))) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1991-2020)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1989, 2021), expand = c(0, 0)) +
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

#Relação Professor-Aluno Grandes Áreas
dados |> 
  drop_na(goga) |> 
  filter(grandearea != "") |> 
  filter(ano > 1990) |> 
  ggplot(aes(x = ano, 
             fill = factor(goga, levels = c("Mulher/Mulher", "Mulher/Homem", "Homem/Mulher", "Homem/Homem")))) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1991-2020)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1989, 2021), expand = c(0, 0)) +
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
  facet_wrap(~grandearea, labeller = as_labeller(nomes_grandearea)) 

#Relação Professor-Aluno Humanidades
dados |> 
  drop_na(goga) |> 
  filter(codigoarea %in% humanidades_codigo) |>  
  ggplot(aes(x = ano, 
             fill = factor(goga, levels = c("Mulher/Mulher", "Mulher/Homem", "Homem/Mulher", "Homem/Homem")))) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação das Ciências Humanas",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1990-2020)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1990, 2021)) +
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
  facet_wrap(~codigoarea, labeller = as_labeller(nomes_humanidades)) 

#Relação Professor-Aluno 20 Piores
dados |> 
  drop_na(goga) |> 
  filter(codigoarea %in% vinte_piores) |>  
  ggplot(aes(x = ano, 
             fill = factor(goga, levels = c("Mulher/Mulher", "Mulher/Homem", "Homem/Mulher", "Homem/Homem")))) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação das Ciências Humanas",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1990-2020)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1990, 2021)) +
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
  facet_wrap(~codigoarea, labeller = as_labeller(nomes_20piores)) 


#Tabelas#### 
tab <- function(dataset, var){
  dataset  |> 
    group_by({{var}})  |>
    drop_na({{var}}) |> 
    summarise(n = n()) |> 
    mutate(TotalN = cumsum(n),
           relativa = (scales::percent(n / sum(n), accuracy = .1, trim = FALSE)),
           acumulada = (scales::percent(cumsum(n/sum(n)), accuracy = .1, trim = FALSE)))
}

teste <- tab(dados, gorientador)

scales::percent(n/sum(n), accuracy = .1, trim = FALSE)
#Matriz 2x2 para cálculo de razão de prevalência
matriz <- dados |>  
  drop_na(galuno, gorientador) |> 
  tabyl(gorientador, galuno) |> 
  adorn_totals(c("row", "col"))

decadas <- dados |> mutate(
  cincoanos = cut(ano,
                  c(0, 1995, 2000, 2005, 2010, 2015, Inf),
                  c("1987-1995", "1996-2000", "2001-2005", "2006-2010", "2011-2015", "2016-2020"))) |> 
  drop_na(gorientador, galuno) |> 
  select(cincoanos, galuno, gorientador)

decadas |> count(cincoanos, gorientador, galuno) 



matriz1 <- matriz %>%
  remove_rownames() %>%
  column_to_rownames(var = "gorientador")

epi.2by2(dat = doismilvinte, method = "cross.sectional",
         conf.level = 0.95, units = 100, outcome = "as.columns")








dadosreg <- dados |>  
  group_by(ano) |> 
  summarize(naluno = sum(galuno, na.rm = TRUE),
            nprofe = sum(gorientador, na.rm = TRUE))
#Cálculo profe por ano
dadosprofe <- dados  |> 
  filter(codigoarea == 70100004) |> 
  count(ano, gorientador) |> 
  drop_na()
#Cálculo aluno por ano
dadosaluno <- dados  |> 
  filter(codigoarea == 70100004) |> 
  count(ano, galuno) |> 
  drop_na()

mergedados <- 
  left_join(dadosprofe, dadosaluno, by = c("ano" = "ano"))

model <- lm(naluno ~ nprofe, data = dadosreg)  


#GRÁFICO GÊNERO BANCO13_20####
dadosprofi <- banco1320 |> 
  filter(CD_AREA_CONHECIMENTO == 70100004) |> 
  select(AN_BASE, NM_GRAU_ACADEMICO, NM_ORIENTADOR, NM_DISCENTE)

dadosprofi <- dadosprofi |> 
  mutate(
    gorientador = get_gender(NM_ORIENTADOR),
    galuno = get_gender(NM_DISCENTE), 
    goga = factor(case_when(
      (gorientador == "Male" & galuno == "Male") ~ "Homem/Homem",
      (gorientador == "Male" & galuno == "Female") ~ "Homem/Mulher",
      (gorientador == "Female" & galuno == "Male") ~ "Mulher/Homem",
      (gorientador == "Female" & galuno == "Female") ~ "Mulher/Mulher"),
      levels = c("Mulher/Mulher", "Mulher/Homem", "Homem/Mulher", "Homem/Homem"))
  )

#Gráfico das HUMANAS por ALUNO - BARRA
dadosprofi |> 
  drop_na(galuno) |> 
  ggplot(aes(x = AN_BASE, fill = galuno)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de Gênero na Pós-Graduaçaõ em Filosofia",
       subtitle = "Proporção de trabalhos defendidos por <span style= 'color:#1d4497; font-size:32pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:32pt;'>**Mulheres**</span> (2012-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(2012, 2021), expand = c(0, 0)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 20)) +
  facet_wrap(~NM_GRAU_ACADEMICO) 

dadosprofi |> 
  drop_na(galuno) |> 
  filter(galuno == "Female") |> 
  ggplot(aes(x = AN_BASE, color = NM_GRAU_ACADEMICO)) +
  geom_line(stat = "count", size = 1.2) +
  scale_x_continuous(limits = c(2012, 2020)) +
  scale_y_continuous( position = "right") +
  scale_colour_manual(values = met.brewer("Nizami", 3)) +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5), 
        plot.subtitle = element_markdown(face = "bold", hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)))+
  coord_cartesian(clip = 'off') 


#Salvar gráfico#####
ggsave(
  "prop_12piores.png",
  bg = "white",
  width = 22,
  height = 22,
  dpi = 900,
  plot = last_plot())

dados |> 
  drop_na(ano, goga) |> 
  filter(ano >= 2010) |> 
  tabyl(ano, goga) |> 
  adorn_totals("row") |>
  adorn_percentages() |> 
  adorn_pct_formatting() |> 
  adorn_ns() |> 
  gt() 

dados |> 
  count(ano, nivel) |> 
  gt()

dadosfil 



