####Pacotes####
library(tidyverse)
library(here)
library(MetBrewer) # Paleta de cores
library(scales) # Uso de porcentagem em gráficos
library(ggforce)
library(ggridges)
library(extrafont)
library(ggtext)
library(ggstream)
library(geobr)
library(sf)
library(brpop) # Dados da população brasileira
library(janitor) # Estatísticas sumárias

# Extração do banco de dados total - n. 12525 
dados <- read.csv("dados/catalogo.csv") |> 
  select(an_base, nm_entidade_ensino, nm_regiao, nm_subtipo_producao, sg_uf_ies, nm_producao, ds_palavra_chave, ds_resumo, nr_paginas, g_orientador, g_discente, g_oridis)

# Gráfico - Evolução total da produção#### 
dados |> 
  ggplot(aes(x=an_base)) +
  geom_freqpoly(binwidth = 1,  linewidth = 1.2) +
  scale_x_continuous(limits = c(1988, 2021), breaks = seq(1980, 2020, 5)) +
  scale_y_continuous(limits = c(0, 900), position = "right") +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação em Filosofia",
       subtitle = "Teses e Dissertações defendidas entre 1987-2021",
       caption = "Elaboração: Os autores | Dados: CAPES", 
       x = "",
       y = "") +
  theme(plot.title = element_markdown(face = "bold"), #letra do título
        plot.caption = element_markdown(margin = margin(t = 3)),
        plot.subtitle = element_markdown(face = "bold"),
        text = element_text(size = 35)) + 
  coord_cartesian(clip = 'off') + #permite dados além dos limites do gráfico (seta,p.ex.)
  geom_text(aes(x = 2015, y = 80, 
                label = "Total = 11.736 trabalhos"),
            stat = "unique", size = 5, family = "Roboto Bold")

# Gráfico - Tese e Dissertação#### 
dados |> 
  ggplot(aes(x = an_base, color = nm_subtipo_producao)) +
  geom_line(stat = "count", linewidth = 1.2) +
  scale_x_continuous(limits = c(1987, 2022), breaks = seq(1980, 2021, 5)) +
  scale_y_continuous(limits = c(0, 700), position = "right") +
  scale_colour_manual(labels=c('Dissertação', 'Tese'),
                      values = met.brewer("Nizami", 2)) +
  labs(title = "Evolução da defesa de trabalhos na Pós-Graduação em Filosofia",
       subtitle = "<span style= 'color:#1d4497; font-size:14pt; font-weight: bold;'>Teses</span> e <span style= 'color:#b83326;font-size:14pt;'>Dissertações</span> defendidas entre 1987 e 2020",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       col = "Nível") +
  theme(plot.title = element_markdown(face = "bold"),#letra do título
        plot.subtitle = element_markdown(face = "bold"),
        plot.caption = element_markdown(margin = margin(t = 3)),
        legend.position = "none",
        text = element_text(size = 14)) + 
  coord_cartesian(clip = 'off') 

# Gráfico - Região#### 
ggplot(dados, aes(x = an_base, colour = nm_regiao)) + 
  geom_line(stat = "count", size = 1.2) +
  scale_x_continuous(limits = c(1987, 2021), breaks = seq(1980, 2020, 5), expand = c(0, .2)) +
  scale_y_continuous(limits = c(0, 450), position = "right") +
  scale_colour_manual(labels = c("Centro-Oeste", "Nordeste", "Norte", "Sudeste", "Sul"),
                      values = met.brewer("Nizami", 5)) +
  labs(title = "Desigualdades regionais",
       subtitle = "Trabalhos defendidos na pós-graduação por região (1987-2020)",
       caption = "Elaboração: Dataphilo | Dados: CAPES", 
       x = "",
       y = "",
       col = "Região") +
  theme(plot.title = element_markdown(face = "bold"), #letra do título
        plot.caption = element_markdown(margin = margin(t = 3)),
        legend.position = "top",
        text = element_text(size = 14),
        legend.key.width=unit(1, "cm")) +
  guides(colour = guide_legend(override.aes = list(size=2.5)))

# Gráfico - Mapa Brasil (Região e Estado)####
# Baixar dados populacionais por Estado
# Site: Elaboração: Atlas do Desenvolvimento Humano no Brasil. Pnud Brasil, Ipea e FJP, 2022.
# Site: Fontes: dados do IBGE e de registros administrativos, conforme especificados nos metadados disponíveis disponíveis em: http://atlasbrasil.org.br/acervo/biblioteca.
# População por Estado
populacao_estado <- readxl::read_excel("dados/populacao_br.xlsx") |> 
  mutate(regiao = case_when(
    territorio %in% c("Acre", "Amapá", "Amazonas", "Pará", "Rondônia", "Roraima", "Tocantins") ~ "Norte",
    territorio %in% c("Alagoas", "Bahia", "Ceará", "Maranhão", "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte", "Sergipe") ~ "Nordeste",
    territorio %in% c("Distrito Federal", "Goiás", "Mato Grosso", "Mato Grosso do Sul") ~ "Centro Oeste",
    territorio %in% c("Espírito Santo", "Minas Gerais", "Rio de Janeiro", "São Paulo") ~ "Sudeste",
    territorio %in% c("Paraná", "Rio Grande do Sul", "Santa Catarina") ~ "Sul"))

# População por região
populacao_regiao <- populacao_estado |> 
  group_by(regiao) |> 
  summarize(populacao = sum(populacao))

# Baixar mapa de regiões
regiao <- geobr::read_region(year = 2020)

# Sumarizar dados por região
dados_regiao <- dados |> 
  group_by(nm_regiao) |> 
  summarize(trabalhos = n()) |> 
  mutate(nm_regiao = recode(nm_regiao,
         "centrooeste" = "Centro Oeste",
         "nordeste" = "Nordeste",
         "norte" = "Norte",
         "sudeste" = "Sudeste",
         "sul" = "Sul")) 

# Cálculo densidade regional
densidade_regiao <- dplyr::left_join(populacao_regiao, 
                           dados_regiao, 
                           by = c("regiao" = "nm_regiao")) |> 
  mutate(densidade = (trabalhos / populacao)*100000)

# Unificar bancos
regiao <- dplyr::left_join(regiao, 
                           dados_regiao, 
                           by = c("name_region" = "nm_regiao"))

# Gráfico - Regiao
ggplot(regiao) +
geom_sf(aes(fill=trabalhos), color = "NA") +
  labs(title="Teses e dissertações defendidas na Pós-Graduação de Filosofia por região (1988-2021)",
       subtitle = "Elaboração: Os autores | Dados: Catálogo de Teses e Dissertações (CAPES)", 
       size = 12) +
  scale_fill_distiller(palette = "Blues", direction = 1, name = NULL, limits = c(50,7000)) +
  geom_sf_text(aes(label = trabalhos), size = 6) +
  theme_void() 

# Baixar mapa de Estados
estados <- geobr::read_state(year = 2020)

# Sumarizar dados por região
dados_estado <- dados |> 
  group_by(sg_uf_ies) |> 
  summarize(trabalhos = n()) |> 
  mutate(sg_uf_ies = str_to_upper(sg_uf_ies))

# Cálculo densidade Estados
densidade_estados <- dplyr::left_join(populacao_estado, 
                                     dados_estado, 
                                     by = c("territorio" = "sg_uf_ies")) |> 
  mutate(densidade = (trabalhos / populacao)*100000)

# Unificar bancos
estados <- dplyr::left_join(estados, 
                           dados_estado, 
                           by = c("abbrev_state" = "sg_uf_ies"))

# Gráfico - Estados
ggplot(estados) +
  geom_sf(aes(fill=trabalhos)) +
  labs(title="Trabalhos defendidos na Pós-Graduação de Filosofia por Estado (1988-2021)", 
       subtitle = "Elaboração: Os autores | Dados: Catálogo de Teses e Dissertações (CAPES)", 
       size=12) +
  scale_fill_distiller(palette = "Blues", direction = 1, na.value = "white", name=NULL, limits = c(0,4000)) +
  geom_sf_text(aes(label = trabalhos), size = 5) +
  theme_void() 

# Gráfico - Instituições#### 
# Cálculo de percentuais por instituição
ifes <- dados |> 
  mutate(
    nm_entidade_ensino = forcats::fct_lump(nm_entidade_ensino, n = 18)) |> 
  count(nm_entidade_ensino, sort = TRUE) |> 
  mutate(
    nm_entidade_ensino = forcats::fct_rev(fct_inorder(nm_entidade_ensino)),
    nm_entidade_ensino = forcats::fct_relevel(nm_entidade_ensino, "Other", after = 0)) |>  
  mutate(percentual = scales::percent(n/sum(n), accuracy = .1, trim = FALSE))

siglas <- c("PUC/PR", "UNISINOS", "UFG", "UFPR", "UFSM", "UFRGS", "UFSCAR", "UFC",
            "UERJ", "UFSC", "PUC/RJ", "UFPB", "PUC/SP", "UFMG", "PUC/RS",
            "UNICAMP", "UFRJ", "USP")

ifes  |>  
  filter(nm_entidade_ensino != "Other") |>  
  ggplot(aes(x = n, y = nm_entidade_ensino,
             fill = nm_entidade_ensino)) +
  geom_col() +
  geom_text(
    aes(label = percentual,
        hjust = -0.1, 
        family = "Roboto Bold",
        size = 35)) +
  scale_fill_manual(values=met.brewer("Nizami", 18)) +
  scale_x_continuous(limits = c(0, 1250), expand = c(0, 0)) +
  scale_y_discrete(labels = siglas) +
  labs(title = "Trabalhos na Pós-Graduação em Filosofia",
       subtitle = "As 18 (de 60) instituições com maior percentual de defesas entre 1987 a 2021",
       caption = "Elaboração: Os autores | Dados: Catálogo de Teses e Dissertações (CAPES)", 
       x = "",
       y = "") +
  theme_void() +
  theme(plot.title = element_markdown(face = "bold"), #letra do título
        plot.caption = element_markdown(margin = margin(t = 2)),
        legend.position = "none",
        axis.text.y = element_text(size = 25),
        text = element_text(size = 30))+
  geom_label(x = 1000, y = 3, 
             family = "Roboto",
             label = "Demais instituições = 27.3%",
             alpha = .0, size = 6) +
  coord_cartesian(clip = 'off')

# Gráficos - Gênero#### 
# Proporção homens/mulheres ao longo dos anos
dados |> 
  drop_na(g_discente) |>
  count(g_discente) |> 
  mutate(percentual = scales::percent(n/sum(n), accuracy = .1, trim = FALSE))

#Gráfico com percentuais
dados |> 
  drop_na(g_discente) |> 
  ggplot(aes(x = an_base, fill = g_discente)) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação em Filosofia",
       subtitle = "Proporção de trabalhos defendidos por  <span style= 'color:#1d4497; font-size:32pt; font-weight: bold;'>Homens</span> e <span style= 'color:#b83326;font-size:32pt;'>Mulheres</span> (1987-2021)",
       caption = "Elaboração: Os autores | Dados: Catálogo de Teses e Dissertações (CAPES)", 
       x = "",
       y = "") +
  theme_minimal() +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_y_continuous(labels=NULL) +
  scale_x_continuous(limits = c(1987, 2021), expand = c(0, 0)) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),#letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        text = element_text(size = 30))  +
  geom_richtext(aes(x = 2015, 
                    y = .6, 
                    label = "**Média: 71.1% de Homens**",
                    size = 25,
                    fill = NA), 
                stat = "unique")

# Gráfico com percentuais - VOID
dados |> 
  drop_na(g_discente) |> 
  ggplot(aes(x = an_base, fill = g_discente)) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação em Filosofia",
       subtitle = "Proporção de trabalhos defendidos por  <span style= 'color:#1d4497; font-size:14pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:14pt;'>**Mulheres**</span> (1987-2020)",
       caption = "Elaboração: Os autores | Dados: Catálogo de Teses e Dissertações (CAPES)", 
       x = "",
       y = "") +
  theme_void() +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_y_continuous(labels=scales::percent, position = "right") +
  scale_x_continuous(limits = c(1987, 2021), expand = c(0, 0)) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),#letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = -6), hjust = 0.975),
        legend.position = "none",
        text = element_text(size = 13))  +
  geom_richtext(aes(x = 2015, 
                    y = .6, 
                    label = "**Média: 71.1% de homens**",
                    size = 7,
                    fill = NA), 
                stat = "unique")

# Proporção de gênero (década) ####
# Tabela de proporção entre décadas
g_discente_decada <- dados |> mutate(
  decada = cut(an_base,
               c(0, 2000, 2010, Inf),
               c("1987-2000", "2001-2010", "2011-2020"))) |>
  count(decada, g_discente, sort = TRUE) |> 
  group_by(decada) |> 
  mutate(percentual = scales::percent(n/sum(n), accuracy = .1, trim = FALSE))

dados |> 
  tabyl(nm_regiao, g_discente, show_na = FALSE) |> 
  adorn_totals("row") |> 
  adorn_percentages("row") |> 
  adorn_pct_formatting() |> 
  adorn_ns()

# Gráfico - Gênero de Professores#### 
# Gráfico com proporção de trabalhos orientados por homem/mulher
# Proporção homens/mulheres ao longo dos anos
dados |> 
  drop_na(g_orientador) |>
  count(g_orientador) |> 
  mutate(percentual = scales::percent(n/sum(n), accuracy = .1, trim = FALSE))

dados |> 
  drop_na(g_orientador) |> 
  ggplot(aes(x = an_base, fill = g_orientador)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Pós-Graduação em Filosofia",
       subtitle = "Proporção de trabalhos orientados por <span style= 'color:#1d4497; font-size:32pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:32pt;'>**Mulheres**</span> (1991-2021)",
       caption = "Elaboração: Os autores | Dados: Catálogo de Teses e Dissertações (CAPES)", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_x_continuous(limits = c(1990, 2022), expand = c(0, 0)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 28)) +
  geom_richtext(aes(x = 2017, 
                    y = .6, 
                    label = "**Média:80.4%**",
                    size = 25,
                    fill = NA), 
                stat = "unique")

#Gráfico com percentuais
dados |> 
  drop_na(g_orientador) |> 
  ggplot(aes(x = an_base, fill = g_orientador)) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação em Filosofia",
       subtitle = "Proporção de trabalhos orientados por  <span style= 'color:#1d4497; font-size:14pt; font-weight: bold;'>Homens</span> e <span style= 'color:#b83326;font-size:14pt;'>Mulheres</span> (1991-2020)",
       caption = "Elaboração: Os autores | Dados: Catálogo de Teses e Dissertações (CAPES)", 
       x = "",
       y = "") +
  theme_minimal() +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_y_continuous(labels = NULL) + #labels=scales::percent, position = "right") +
  scale_x_continuous(limits = c(1991, 2022), expand = c(0, 0)) +
  theme(plot.title = element_markdown(face = "bold"),#letra do título
        plot.subtitle = element_markdown(face = "bold"),
        plot.caption = element_markdown(margin = margin(t = 3)),
        legend.position = "none",
        text = element_text(size = 14))

# Porcentagem####
dados |> 
  tabyl(an_base, g_oridis, show_na = FALSE) |> 
  adorn_totals("row") |> 
  adorn_percentages("row") |> 
  adorn_pct_formatting() |> 
  adorn_ns()

gorientador_decada <- dados |> 
  drop_na(galuno, gorientador) |> 
  mutate(
    decada = cut(ano,
                 c(0, 2000, 2010, Inf),
                 c("1987-2000", "2001-2010", "2011-2020"))) |>
  count(decada, gorientador, sort = TRUE) |> 
  group_by(decada) |> 
  mutate(percentual = scales::percent(n/sum(n), accuracy = .1, trim = FALSE))


#Desigualdade de gênero | PROFESSOR/ALUNO#### 
goga_decada <- dados |> 
  drop_na(g_orientador, g_orientador) |> 
  mutate(
    decada = cut(ano,
                 c(0, 2000, 2010, Inf),
                 c("1987-2000", "2001-2010", "2011-2020"))) |>
  count(decada, regiao, goga, sort = TRUE) |> 
  group_by(decada) |> 
  mutate(percentual = scales::percent(n/sum(n), accuracy = .1, trim = FALSE))

dados |> 
  drop_na(g_oridis) |> 
  filter(an_base > 1990) |> 
  ggplot(aes(x = an_base, fill = factor(g_oridis, levels = c("FF", "FM", "MF", "MM")))) +
  geom_bar(position = "fill") +
  labs(title = "Desigualdade de gênero na Pós-Graduação em Filosofia",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1991-2020)",
       caption = "Elaboração: Os autores | Dados: Catálogo de Teses e Dissertações (CAPES)", 
       x = "",
       y = "",
       fill = "Professor/Aluno") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  scale_x_continuous(limits = c(1989, 2022), expand = c(0, 0)) +
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

dados |> 
  drop_na(g_oridis) |> 
  ggplot(aes(x = g_oridis, 
             fill = g_oridis)) +
  geom_bar(aes(y = (after_stat(count))/sum(after_stat(count))),width = .99) +
  geom_label(aes(y = ((after_stat(count))/sum(after_stat(count))), 
                 label = scales::percent((after_stat(count))/sum(after_stat(count)))), 
             stat = "count", 
             vjust = 0.8,
             size=8,
             color = "white",
             family = "Roboto Bold") +
  labs(title = "Desigualdade de gênero na Pós-Graduação em Filosofia",
       subtitle = "Proporção de trabalhos defendidos conforme relação Professor/Aluno (1991-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Java", 4)) +
  theme(plot.title = element_markdown(face = "bold", hjust = .5), #letra do título
        plot.subtitle = element_markdown(size = 26, hjust = .5),
        plot.caption = element_markdown(margin = margin(t = 3)),
        legend.position = "none",
        plot.margin = margin(1,1,1.2,1.2, "cm"),
        text = element_text(size = 30),
        axis.text.y=element_blank())

#Gráfico desigualdade por região
dados |> 
  drop_na(gorientador) |> 
  ggplot(aes(x = regiao, fill = gorientador)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Filosofia | Região",
       subtitle = "Proporção de trabalhos *orientados* por <span style= 'color:#1d4497; font-size:35pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:35pt;'>**Mulheres**</span> (1987-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_y_continuous(labels = NULL) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        axis.text.y=element_blank(),
        text = element_text(size = 32)) 

#Gráfico PPGs
#NomeIES
nome_ies <- c("universidade de sao paulo", 
              "universidade federal do rio de janeiro",
              "universidade estadual de campinas",
              "pontificia universidade catolica do rio grande do sul",
              "universidade federal de minas gerais",
              "pontificia universidade catolica de sao paulo",
              "universidade federal da paraiba",
              "pontificia universidade catolica do rio de janeiro",
              "universidade federal de santa catarina",
              "universidade do estado do rio de janeiro",
              "universidade federal do ceara",
              "universidade federal de sao carlos",
              "universidade federal do rio grande do sul",
              "universidade federal de santa maria")

siglas_ies <-  c("PUC/SP", "PUC/RJ","PUC/RS", "USP","UERJ", "UNICAMP", "UFPB",
                 "UFMG", "UFSC", "UFSM", "UFSCAR", "UFC", "UFRJ", "UFRGS")

dados |> 
  filter(nomeies %in% nome_ies) |> 
  drop_na(galuno) |> 
  ggplot(aes(x = nomeies, fill = galuno)) +
  geom_bar(position = "fill") +
  theme_minimal() +
  labs(title = "Desigualdade de gênero na Filosofia | IES",
       subtitle = "Proporção de trabalhos *orientados* por <span style= 'color:#1d4497; font-size:25pt;'>**Homens**</span> e <span style= 'color:#b83326;font-size:25pt;'>**Mulheres**</span> (1987-2020)",
       caption = "Dados: CAPES | Elaboração: Dataphilo", 
       x = "",
       y = "") +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_discrete(labels = siglas_ies) +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        plot.caption = element_markdown(margin = margin(t = 3), hjust = 0.975),
        legend.position = "none",
        plot.margin = margin(1,1,1.5,1.2, "cm"),
        #axis.text.y=element_blank(),
        text = element_text(size = 30)) +
  coord_flip()


#Matriz 2x2 para cálculo de razão de prevalência
matriz <- dados |> 
  drop_na(galuno, gorientador) |> 
  tabyl(gorientador, galuno) |> 
  adorn_totals(c("row", "col"))
epi.2by2(dat = matriz, method = "cross.sectional",
         conf.level = 0.95, units = 100, outcome = "as.columns")

dados <- dados  |>  
  mutate_at(c("galuno", "gorientador"), factor) 
matriz <- table(dados$gorientador, dados$galuno, dnn = c("Orientador", "Aluno"))
goga_prop <- dados |> 
  drop_na(galuno, gorientador) |> 
  filter(ano > 2015) |> 
  count(ano, gorientador, galuno, sort = TRUE) |> 
  group_by(ano, galuno) |> 
  mutate(percentual = scales::percent(n/sum(n), accuracy = .1, trim = FALSE))
ggplot(goga_prop, aes(x = n, y = gorientador,
                      fill = galuno)) +
  geom_col(position = "dodge") +
  geom_text(aes(label = percentual)) +
  coord_flip()

dados |> 
  drop_na(galuno, gorientador) |> 
  ggplot(aes(x = galuno, fill = galuno)) +
  geom_bar(stat = "count") +
  facet_wrap(~ gorientador)

#tabela para gráfico
#https://github.com/IcaroBernardes/tidytuesday/blob/main/2022/week12/week12.R 
dados_uf <- dados |> 
  drop_na(galuno) |> 
  count(uf, galuno) |> 
  group_by(uf) |> 
  mutate(percentual = n/sum(n),
         rotulo = scales::percent(n/sum(n), accuracy = .1, trim = FALSE),
         uf = str_to_upper(uf)) 


ggplot(NULL) +
  ggforce::geom_arc_bar(aes(x0 = 0, y0 = 0, r0 = 1, r = 1.3, amount = percentual,
                            fill = galuno), stat = "pie", data = dados_uf) +
  scale_fill_manual(values = met.brewer("Nizami", 2)) +
  geom_text(aes(x= 0, y = 0, label = ifelse(galuno == "Female", rotulo, "")), family = "Roboto", size = 8, data = dados_uf) +
  coord_fixed() +
  facet_wrap(~uf) +
  theme_void() +
  theme(plot.caption = element_markdown(margin = margin(t = 3)),
        legend.position = "none",
        text = element_text(size = 16, face = "bold"),
        axis.text.y=element_blank())


