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


# Banco de dados
dados <- read.csv("dados/dados_pre-stm.csv")
#Set up do tema####
theme_set(theme_minimal(base_family = "Roboto"))

#Gráfico - Evolução total da produção#### 
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

#Gráfico - Evolução da produção | Tese-Dissertação#### 
dados |> 
  ggplot(aes(x = an_base, color = nm_subtipo_producao)) +
  geom_line(stat = "count", linewidth = 1.2) +
  scale_x_continuous(limits = c(1988, 2021), breaks = seq(1980, 2020, 5)) +
  scale_y_continuous(limits = c(0, 600), position = "right") +
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

#Gráfico - Evolução da produção | Região#### 
ggplot(dados, aes(x = an_base, colour = nm_regiao)) + 
  geom_line(stat = "count", size = 1.2) +
  scale_x_continuous(limits = c(1987, 2021), breaks = seq(1980, 2020, 5), expand = c(0, .2)) +
  scale_y_continuous(limits = c(0, 400), position = "right") +
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

# Mapa Brasil | Regiões ####
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
# Unificar bancos
regiao <- dplyr::left_join(regiao, 
                           dados_regiao, 
                           by = c("name_region" = "nm_regiao"))

# Gráfico
ggplot() +
geom_sf(data = regiao, aes(fill=trabalhos), color = NA, size=.15) +
  labs(title="Trabalhos defendidos na Pós-Graduação de Filosofia por região (1988-2021)", size=8) +
  scale_fill_distiller(palette = "Blues", direction = 1, name="Trabalhos", limits = c(50,7000)) +
  theme_minimal() 

# Baixar mapa de Estados
estados <- geobr::read_state(year = 2020)
# Sumarizar dados por região
dados_estado <- dados |> 
  group_by(sg_uf_ies) |> 
  summarize(trabalhos = n()) |> 
  mutate(sg_uf_ies = str_to_upper(sg_uf_ies))

# Unificar bancos
estados <- dplyr::left_join(estados, 
                           dados_estado, 
                           by = c("abbrev_state" = "sg_uf_ies"))

# Gráfico
ggplot(estados) +
  geom_sf(aes(fill=trabalhos), color = NA, size=.15) +
  labs(title="Trabalhos defendidos na Pós-Graduação de Filosofia por Estado (1988-2021)", size=8) +
  scale_fill_distiller(palette = "Blues", direction = 1, name="Trabalhos", limits = c(0,4000)) +
  geom_sf_text(aes(label = trabalhos)) +
  theme_minimal() 

