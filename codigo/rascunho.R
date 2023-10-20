
#FREX####
resumo <- labelTopics(topic_model, n = 5)
frexs <- as_tibble(resumo$frex) 

frex <- frexs |> 
  mutate(topic = as.factor(row_number()),
         frex = paste(V1, V2, V3, V4, V5, sep = ", ")) |> 
  select(topic, frex)

# Findthoughts (STM) #### 
findallthoughts_m80 <- tidygamma |> 
  mutate(document = as.integer(document)) |> 
  left_join(dados,
            by = c("document" = "doc_id")) |> # Unifica o banco dados com a matrix gamma
  group_by(document) |> # Agrupa os valores gamma de cada tópico
  slice_max(order_by = gamma, n = 1) |> # Escolhe o tópico com maior gamma de cada documento
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

# TABELA CATEGORIA####
# Tabela | Categoria-Tópicos####
gamma_words <- gamma_words |> left_join(categorias,
                                        by = "topic") 

# Tabelas ver stm_descricao####
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


# Gráfico | Categorias-Tópicos COM EFEITOS####
cat_ano |>
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
             y = estimate))

ggplot(cat_ano, 
       aes(x = covariate.value,
           y = estimate, 
           group = category)) +
  geom_line() +
  facet_wrap(~topic) +
  labs(title = "Gráfico de Linha para o Tópico: {facet_wrap_var}",
       x = "Covariate Value",
       y = "Estimate") +
  theme_minimal()

# Sumariza por categoria | COM EFEITOS####
cat_ano <- stm_ano |> 
  summarize(total = sum(estimate), 
            .by = c(category, covariate.value)) 

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
  height = 95,
  dpi = 900,
  plot = last_plot())


# TABELA TRENDS ANO#### 
sorted_df <- stm_ano %>%
  arrange(covariate.value, desc(estimate))

top_10_topics <- sorted_df %>%
  group_by(covariate.value) %>%
  slice_head(n = 10)



trends_ano <- stm_ano |> 
  select(topic, covariate.value, estimate) |> 
  pivot_wider(names_from = "covariate.value", 
              values_from  = "estimate") 

trends_ano1 <- trends_ano  |> 
  pivot_longer(cols = -topic, names_to = "year", values_to = "value")

# Converta o ano para numérico
trends_ano_melted$year <- as.numeric(gsub("`", "", trends_ano_melted$year))

# Classifique decrescentemente por valor para cada ano e selecione os 10 principais tópicos
top10_trends <- trends_ano_melted %>%
  group_by(year) %>%
  arrange(desc(value)) %>%
  slice(1:10) %>%
  ungroup()

tabela_trends <- top10_trends %>%
  select(-value) %>%  # Remova a coluna 'value'
  mutate(topic = as.numeric(topic)) |> 
  pivot_wider(names_from = year, values_from = topic)

colnames(tabela_trends) <- gsub("`", "", colnames(tabela_trends))


# MAPA CORRETO####
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
                            "sul" = "Sul")) |> 
  mutate(frequencia = round(trabalhos/sum(trabalhos)*100,2)) 
# Unificar bancos
regiao <- dplyr::left_join(regiao, 
                           dados_regiao, 
                           by = c("name_region" = "nm_regiao"))

# Gráfico - Regiao
col_palette <-  colorspace::desaturate(sequential_hcl(
  n = 7, h = c(0, -100), c = c(80, NA, 40), l = c(40, 75), power = c(1, 1), 
  register = "Red-Blue")) 

ggplot(regiao) +
  geom_sf(aes(fill = trabalhos), color = "NA") +
  labs(size = 30) +
  theme_void() +
  theme(plot.title = element_markdown(face = "bold"),
        legend.position = "none") +
  scale_fill_gradientn(colors = col_palette) +
  geom_sf_text(aes(label = stringr::str_glue('{trabalhos}\n({frequencia}%)')), 
               size = 9,
               color = "White",
               fontface = "bold") 

ggsave(
  "figs/fig2_brazilmap.png",
  bg = "white",
  width = 15,
  height = 10,
  dpi = 1200,
  plot = last_plot())