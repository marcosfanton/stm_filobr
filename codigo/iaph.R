# Gráfico 01 | Evolução do n. de Teses e Dissertações BR####
evol_total <-  dados |> 
  summarize(n = n(),
            .by = c(an_base, nm_grau_academico)) |> 
  mutate(nm_grau_academico = recode(nm_grau_academico,
                                    "Mestrado" = "Master",
                                    "Doutorado" = "PhD",
                                    "Mestrado Profissional" = "Professional Master")) 

evol_total |> 
  ggplot(aes(x = an_base, 
             y = n)) +
  geom_line(aes(color = nm_grau_academico), linewidth = 3)+
  stat_summary(aes(color = "Total"), fun = sum, geom ='line', linewidth = 1.5) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(position = "right") +
  theme_classic() +
  scale_color_d3() +
  labs(title = "Defenses of academic works in Brazilian Graduate Programs",
       subtitle = "Theses and Dissertations defended between 1991-2021 | N: **1.117.943**",
       x = "",
       y = "",
       color = "Academic Degree") +
  theme(plot.title = element_markdown(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "top",
        text = element_text(size = 30)) +
  coord_cartesian(clip = 'off')  # Permite dados além dos limites do gráfico (seta,p.ex.)
 
ggsave(
  "figs/iaph/graf1_evoltotal.png",
  bg = "white",
  width = 22,
  height = 17,
  dpi = 900,
  plot = last_plot())

# Tabela 1 | Grande área e Humanas ####
areas <- dados |> 
  group_by(nm_grande_area_conhecimento) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2)) 

# Cálculo por orientador
areas_go <- dados |> 
  group_by(nm_grande_area_conhecimento, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

areas_go <- areas_go |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o))

# Cálculo por discente
areas_gd <- dados |> 
  group_by(nm_grande_area_conhecimento, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2))

areas_gd <- areas_gd |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d))

# Cálculo por oridis
areas_god <- dados |> 
  group_by(nm_grande_area_conhecimento, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

areas_god <- areas_god |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Tabela 1 | Agrupamento Grande Área####
lista_grande_area <- list(areas, 
                          areas_go, 
                          areas_gd, 
                          areas_god)

tab_grande_area <- purrr::reduce(lista_grande_area, 
                                 left_join, 
                                 by = "nm_grande_area_conhecimento") |> 
  mutate(descritor = "Fields",
         nm_grande_area_conhecimento = recode(nm_grande_area_conhecimento,
                        "Ciências Agrárias" = "Agricultural Sciences",
                        "Ciências Biológicas" = "Biological Sciences",
                        "Ciências Da Saúde" = "Health Sciences",
                        "Ciências Exatas E Da Terra" = "Exact and Earth Sciences",
                        "Ciências Humanas" = "Humanities",
                        "Ciências Sociais Aplicadas" = "Applied Social Sciences",
                        "Engenharias" = "Engineering",
                        "Lingüística, Letras E Artes" = "Linguistics, Literature, and Art",
                        "Multidisciplinar" = "Multidisciplinary")) |> 
  rename("areas" = "nm_grande_area_conhecimento")

# Tabela 1 | Cálculo por Humanas ####
humanas <- dados |> 
  filter(nm_grande_area_conhecimento == "Ciências Humanas") |> 
  mutate(nm_area_avaliacao = droplevels(nm_area_avaliacao))  

# Cálculo Total
humanas_total <- humanas |> 
  group_by(nm_area_avaliacao) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Cálculo por orientador
humanas_go <- humanas |> 
  group_by(nm_area_avaliacao, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2))

humanas_go <- humanas_go |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o))

# Cálculo por discente
humanas_gd <- humanas |> 
  group_by(nm_area_avaliacao, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2))

humanas_gd <- humanas_gd |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d))

# Cálculo por Orientador-Discente
humanas_god <- humanas |> 
  group_by(nm_area_avaliacao, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

humanas_god <- humanas_god |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Tabela 1 | Agrupamento Humanas ####
lista_humanas <- list(humanas_total, 
                      humanas_go, 
                      humanas_gd, 
                      humanas_god)

tab_humanas <- purrr::reduce(lista_humanas, 
                             left_join, 
                             by = "nm_area_avaliacao") |> 
  mutate(descritor = "Humanities",
         nm_area_avaliacao = recode(nm_area_avaliacao,
                                    "Antropologia / Arqueologia" = "Anthropology/Archaeology",
                                    "Ciência Política E RI" = "Political Science and IR",
                                    "Educação" = "Education",
                                    "Filosofia" = "Philosophy",
                                    "Geografia" = "Geography",
                                    "História" = "History",
                                    "Psicologia" = "Psychology",
                                    "Sociologia" = "Sociology",
                                    "Teologia" = "Theology"))|> 
  rename("areas" = "nm_area_avaliacao")

# Tabela 1 | União tab_grande_area + tab_humanas####
tab <- bind_rows(tab_grande_area, tab_humanas)

# TABELA 01####                                                             
tab_humanas_total <- tab_humanas |> 
  gt(rowname_col = "areas") |>
  cols_hide("descritor") |> 
  cols_merge(
    columns = c(total, frequencia), # Total
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_d_Male, frequencia_d_Male), # Discentes Homens
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_d_Female, frequencia_d_Female), # Discentes Mulheres
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_o_Male, frequencia_o_Male), # Orientadores Homens
    pattern = "{1} ({2}%)") |> 
  cols_merge(
    columns = c(total_o_Female, frequencia_o_Female), # Orientadoras Mulheres
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
  tab_spanner(
    label = "Student",
    columns = c(total_d_Male, total_d_Female)) |> 
  tab_spanner(   # Títulos
    label = "Supervisor",  
    columns = c(total_o_Male, total_o_Female)) |>
  tab_spanner(
    label = "Supervisor/Student",
    columns = c(total_od_FF, total_od_FM, total_od_MF,total_od_MM)) |> 
  cols_label(
    total = "Works",
    total_o_Male = "Man",
    total_o_Female = "Woman",
    total_d_Female = "Woman",
    total_d_Male = "Man",
    total_od_FF = "Female/Female",
    total_od_FM = "Female/Male",
    total_od_MF = "Male/Female",
    total_od_MM = "Male/Male"
  ) |> 
  tab_header(
    title = "Table 1. Description of the gender distribution among supervisors and students in Humanities (Graduate Programs) in Brazil (1991-2021)"
  ) |> 
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".") |> 
  tab_style(
    style = list(
      cell_fill(color = "lightgray")
    ),
    locations = cells_body(
      columns = everything(),
      rows = 4)
    )
# Salvar as tabelas - 2 Tabelas distintas
gtsave(tab_humanas_total,
  "figs/iaph/table1_2totaldescription.png",
  vwidth = 1300, vheight = 3000)

# Gráfico 02 | Prevalência mulher Docente vs Discente | Áreas e Grande Área #### 
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

# União
dados_god <- left_join(dados_go, 
                       dados_gd, 
                       by = c("nm_grande_area_conhecimento", "nm_area_avaliacao")) 

lista_pt <- unique(as.character(dados_god$nm_area_avaliacao))
lista_en <- unique(translate(as.character(dados_god$nm_area_avaliacao), 
                      from = "auto", 
                      to = "en")) 

lista_en <- sub("Right", "Law", lista_en)
correspondencia <- setNames(lista_en, lista_pt)

dados_god <- dados_god |> 
  mutate(nm_area_avaliacao =  recode(nm_area_avaliacao, 
                                     !!!setNames(lista_en, lista_pt)),
         nm_grande_area_conhecimento = recode(nm_grande_area_conhecimento,
                                              "Ciências Agrárias" = "Agricultural Sciences",
                                              "Ciências Biológicas" = "Biological Sciences",
                                              "Ciências Da Saúde" = "Health Sciences",
                                              "Ciências Exatas E Da Terra" = "Exact and Earth Sciences",
                                              "Ciências Humanas" = "Humanities",
                                              "Ciências Sociais Aplicadas" = "Applied Social Sciences",
                                              "Engenharias" = "Engineering",
                                              "Lingüística, Letras E Artes" = "Linguistics, Literature, and Art",
                                              "Multidisciplinar" = "Multidisciplinary")) 


dados_god |> ggplot(aes(x = frequencia_o, 
                        y = frequencia_d)) +
  geom_point(aes(colour = nm_grande_area_conhecimento),
             shape = 20,
             size = 4.5) +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  ggrepel::geom_text_repel(aes(label = nm_area_avaliacao,
                               color = nm_grande_area_conhecimento),
                           show.legend = FALSE,
                           min.segment.length = .7,
                           box.padding = 0.3,
                           size = 5,
                           nudge_x = 0.1,
                           nudge_y = 1.6) +
  labs(title = "Prevalence of Female Supervisors and Female Students among Scientific Subfields (1991-2021)", 
       x = "Prevalence of Female Supervisors (%)",
       y = "Prevalence of Female Students (%)",
       color = "Scientific Fields") +
  scale_color_d3() +
  guides(colour = guide_legend(override.aes = list(size=10))) +
  theme_classic() +
  theme(plot.title = element_markdown(face = "bold"),
        legend.position = c(.85, .38),
        text = element_text(size = 18),
        legend.title.align = 0.25,
        legend.background = element_rect(color = "black", 
                                         linewidth = 0.5, 
                                         linetype = "solid")) 
ggsave(
  "figs/iaph/graf2_supervisorvsstudents.png",
  bg = "white",
  width = 17,
  height = 12,
  dpi = 1200,
  plot = last_plot())

# Tabela 2 | Os 10 piores####

# Cálculo por orientador
piores_go <- dados |> 
  group_by(nm_area_avaliacao, g_orientador) |> 
  summarize(total_o = n()) |> 
  mutate(frequencia_o = round(total_o/sum(total_o)*100,2)) |> 
  ungroup()

# Organização e extração dos 10 piores cursos
piores_go <- piores_go |> 
  pivot_wider(
    names_from = g_orientador,
    values_from = c(total_o, frequencia_o)) |> 
  slice_min(frequencia_o_Female, n = 10) |> 
  mutate(nm_area_avaliacao = droplevels(nm_area_avaliacao)) 

# Lista para filtrar os dados
lista_piores <- levels(piores_go$nm_area_avaliacao)

# Cálculo Total
piores_total <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2))

# Cálculo por discente
piores_gd <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, g_discente) |> 
  summarize(total_d = n()) |> 
  mutate(frequencia_d = round(total_d/sum(total_d)*100,2))

piores_gd <- piores_gd |> 
  pivot_wider(
    names_from = g_discente,
    values_from = c(total_d, frequencia_d)) 

# Cálculo por Orientador-Discente
piores_god <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, g_oridis) |> 
  summarize(total_od = n()) |> 
  mutate(frequencia_od = round(total_od/sum(total_od)*100,2))

piores_god <- piores_god |> 
  pivot_wider(
    names_from = g_oridis,
    values_from = c(total_od, frequencia_od))

# Tabela 2 | Agrupamento em um dataframe ####
lista_piores_df <- list(piores_total, 
                        piores_go, 
                        piores_gd, 
                        piores_god)

tab_piores <- purrr::reduce(lista_piores_df, 
                            left_join, 
                            by = "nm_area_avaliacao") |> 
  arrange(frequencia_d_Female) |>  
  mutate(nm_area_avaliacao = recode(nm_area_avaliacao, 
                                    "Engenharias IV" = "Engineering IV",
                                    "Astronomia / Física" = "Astronomy/Physics",
                                    "Engenharias III" = "Engineering III",
                                    "Matemática / Probabilidade E Estatística" = "Mathematics/Statistics",
                                    "Filosofia" = "Philosophy",
                                    "Economia" = "Economics",
                                    "Teologia" = "Theology",
                                    "Geociências" = "Geoscience",
                                    "Engenharias I" = "Engineering I",
                                    "Ciências Agrárias I" = "Agricultural Sciences I")) |> 
  rename("Subfield" = "nm_area_avaliacao") 

# TABELA 2 ####
tabela_piores <- tab_piores |> 
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
    label = "Supervisor",  
    columns = c(total_o_Male, total_o_Female)) |>
  tab_spanner(
    label = "Student",
    columns = c(total_d_Male, total_d_Female)) |> 
  tab_spanner(
    label = "Supervisor/Student",
    columns = c(total_od_FF, total_od_FM, total_od_MF,total_od_MM)) |> 
  cols_label(
    total = "Works",
    total_o_Male = "Man",
    total_o_Female = "Woman",
    total_d_Female = "Woman",
    total_d_Male = "Man",
    total_od_FF = "Female/Female",
    total_od_FM = "Female/Male",
    total_od_MF = "Male/Female",
    total_od_MM = "Male/Male",
  ) |> 
  tab_header(
    title = "Table 2. Description of the Gender Distribution among the 10 Scientific SubFields with the Lowest Prevalence of works defended by Female Students in Brazil (1991-2021)") |> 
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".") 

# Salvar as tabelas - 2 Tabelas distintas
gtsave(tabela_piores,
       "figs/iaph/table2_10piores.png",
       vwidth = 1000, vheight = 3500)

# Gráfico 03 | Tendência Prop. Docente | 10 piores####
evol_piores_go <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, an_base, g_orientador) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2),
         nm_area_avaliacao = recode(nm_area_avaliacao, 
                                           "Engenharias IV" = "Engineering IV",
                                           "Astronomia / Física" = "Astronomy/Physics",
                                           "Engenharias III" = "Engineering III",
                                           "Matemática / Probabilidade E Estatística" = "Mathematics/Statistics",
                                           "Filosofia" = "Philosophy",
                                           "Economia" = "Economics",
                                           "Teologia" = "Theology",
                                           "Geociências" = "Geoscience",
                                           "Engenharias I" = "Engineering I",
                                           "Ciências Agrárias I" = "Agricultural Sciences I")) |> 
           rename("Subfield" = "nm_area_avaliacao")


# Avaliação aumento-diminuição em dados percentuais
avaliacao_go <- evol_piores_go  |> 
  filter(g_orientador == "Female")  |> 
  group_by(Subfield)  |> 
  arrange(an_base)  |> 
  summarise(variacao = round(((last(frequencia) - first(frequencia))/first(frequencia)) * 100,2))

# Junção no dataframe
evol_piores_go <- evol_piores_go %>%
  left_join(avaliacao_go, by = "Subfield")

# Gráfico 03.1
evol_piores_go |> 
  filter(g_orientador == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = Subfield)) +
  geom_line(linewidth = 2, alpha = 0.1) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              se = FALSE,
              linewidth = 2.5) +
  geom_label_repel(aes(label = paste0(variacao, "%")),
             data = filter(evol_piores_go, an_base == 2021 & g_orientador == "Female"),
             show.legend = FALSE,
             hjust = 0,
             size = 4,
             nudge_x = 0.2) +
  scale_x_continuous(limits = c(1991, 2022), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(limits = c(0, 30), position = "right") +
  labs(title = "Trend of Graduate Works Supervised by Women (with percentage variation between 1991 and 2021)",
       subtitle = "The 10 Scientific SubFields with the Lowest Prevalence of Works defended by Women Students in Brazil (1991-2021)",
       x = "",
       y = "%",
       color = "Subfield") +
  scale_color_d3() +
  theme_classic()+
  theme(text = element_text(size = 15),
        legend.position = "top",
        plot.title = element_markdown(face = "bold")) + 
  coord_cartesian(clip = 'off') 
 
ggsave(
  "figs/iaph/graf3_1tendencygo.png",
  bg = "white",
  width = 12,
  height = 8,
  dpi = 900,
  plot = last_plot())

# Gráfico 03.2 | Tendência Prop. Discente | 10 piores####
evol_piores_gd <- dados |> 
  filter(nm_area_avaliacao %in% lista_piores) |> 
  group_by(nm_area_avaliacao, an_base, g_discente) |> 
  summarize(total = n()) |> 
  mutate(frequencia = round(total/sum(total)*100,2),
         nm_area_avaliacao = recode(nm_area_avaliacao, 
                                    "Engenharias IV" = "Engineering IV",
                                    "Astronomia / Física" = "Astronomy/Physics",
                                    "Engenharias III" = "Engineering III",
                                    "Matemática / Probabilidade E Estatística" = "Mathematics/Statistics",
                                    "Filosofia" = "Philosophy",
                                    "Economia" = "Economics",
                                    "Teologia" = "Theology",
                                    "Geociências" = "Geoscience",
                                    "Engenharias I" = "Engineering I",
                                    "Ciências Agrárias I" = "Agricultural Sciences I")) |> 
  rename("Subfield" = "nm_area_avaliacao")


# Avaliação aumento-diminuição em dados percentuais
avaliacao_gd <- evol_piores_gd  |> 
  filter(g_discente == "Female")  |> 
  group_by(Subfield)  |> 
  arrange(an_base)  |> 
  summarise(variacao = round(((last(frequencia) - first(frequencia))/first(frequencia)) * 100,2))

# Junção no dataframe
evol_piores_gd <- evol_piores_gd  |> 
  left_join(avaliacao_gd, by = "Subfield")

evol_piores_gd |> 
  filter(g_discente == "Female") |> 
  ggplot(aes(x = an_base, 
             y = frequencia,
             color = Subfield)) +
  geom_line(linewidth = 2, alpha = 0.1) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              se = FALSE,
              linewidth = 2.5) +
  geom_label_repel(aes(label = paste0(variacao, "%")),
                   data = filter(evol_piores_gd, an_base == 2021 & g_discente == "Female"),
                   show.legend = FALSE,
                   hjust = 0,
                   size = 4,
                   nudge_x = 0.1) +
  scale_x_continuous(limits = c(1991, 2022), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(limits = c(0, 60), position = "right") +
  labs(title = "Trend of Graduate Works Defended by Women (with percentage variation between 1991 and 2021)",
       subtitle = "The 10 Scientific SubFields with the Lowest Prevalence of Works defended by Women Students in Brazil (1991-2021)",
       x = "",
       y = "%",
       color = "Subfield") +
  scale_color_d3() +
  theme_classic()+
  theme(text = element_text(size = 15),
        legend.position = "top",
        plot.title = element_markdown(face = "bold")) +  
  coord_cartesian(clip = 'off') 

ggsave(
  "figs/iaph/graf3_2tendencygd.png",
  bg = "white",
  width = 12,
  height = 8,
  dpi = 1200,
  plot = last_plot())

# ******FILOSOFIA******#### 
# Uso do catálogo específico da Filosofia | n: 12525
dadosfi <- readr::read_csv("dados/catalogo.csv") |> 
  filter(an_base >= 1991) |> # Exclusão - 172 observações (n: 12353)
  drop_na(g_oridis) # Exclusão - 408 (n: 11945)

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

# Gráfico 04 |  Filosofia Evolução | Grau Acadêmico | Linha ####
evol_fi_total <-  dadosfi |> 
  summarize(n = n(),
            .by = c(an_base, nm_grau_academico)) |> 
  mutate(nm_grau_academico = recode(nm_grau_academico,
                                    "mestrado" = "Master",
                                    "doutorado" = "PhD",
                                    "mestrado profissional" = "Professional Master"))

evol_fi_total |> 
  ggplot(aes(x = an_base, 
             y = n)) +
  geom_line(aes(color = nm_grau_academico), linewidth = 3)+
  stat_summary(aes(color = "Total"), fun = sum, geom ='line', linewidth = 1.5) +
  scale_x_continuous(limits = c(1991, 2021), breaks = seq(1990, 2021, 5)) +
  scale_y_continuous(position = "right") +
  theme_classic() +
  scale_color_d3() +
  labs(title = "Defenses of academic works in Graduate Programs in Philosophy in Brazil",
       subtitle = "Masters and PhD Dissertations defended between 1991-2021 | n: **11.945**",
       x = "",
       y = "",
       color = "Academic Degree") +
  theme(plot.title = element_markdown(face = "bold"),
        plot.subtitle = element_markdown(),
        legend.position = "top",
        text = element_text(size = 30)) +
  coord_cartesian(clip = 'off')  

ggsave(
  "figs/iaph/graf3_evolfitotal.png",
  bg = "white",
  width = 22,
  height = 17,
  dpi = 900,
  plot = last_plot())

# Gráfico 04.A | Filosofia | Descrição Professoras ao longo do tempo####
dadosfi |> 
  ggplot(aes(x = an_base, 
             fill = g_orientador)) +
  geom_bar(position = "fill") +
  theme_void() +
  labs(title = "Gender Inequality in Philosophy Graduate Studies in Brazil",
       subtitle = "Prevalence of dissertations *supervised* by <span style= 'color:#FF7F0EFF; font-size:32pt;'>**Men**</span> and <span style= 'color:#1F77B4FF;font-size:32pt;'>**Women**</span> (1991-2021)",
       x = "",
       y = "") +
  scale_fill_d3() +
  scale_x_continuous(limits = c(1990, 2021), expand = c(0, 0)) +
  scale_y_continuous(position = "right") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        legend.position = "none",
        axis.text.y=element_blank(),
        text = element_text(size = 25)) +
  geom_richtext(aes(x = 2017, 
                    y = 0.4, 
                    label ="&#187;Mean:80.23%&#171;"),
                stat = "unique",
                size = 8,
                fill = "#FF7F0EFF",
                color = "black")
ggsave(
  "figs/iaph/genero_graf04A_filtimego.png",
  bg = "white",
  width = 12,
  height = 8,
  dpi = 1500,
  plot = last_plot())

# Gráfico 04.B | Filosofia | Descrição Estudantes ao longo do tempo####

dadosfi |> 
  ggplot(aes(x = an_base, 
             fill = g_discente)) +
  geom_bar(position = "fill") +
  theme_void() +
  labs(title = "Gender Inequality in Philosophy Graduate Studies in Brazil",
       subtitle = "Prevalence of dissertations *defended* by <span style= 'color:#FF7F0EFF; font-size:32pt;'>**Men**</span> and <span style= 'color:#1F77B4FF;font-size:32pt;'>**Women**</span> (1991-2021)",
       x = "",
       y = "") +
  scale_fill_d3() +
  scale_x_continuous(limits = c(1990, 2021), expand = c(0, 0)) +
  scale_y_continuous(position = "right") +
  theme(plot.title = element_markdown(face = "bold", hjust = 0.5),  #letra do título
        plot.subtitle = element_markdown(hjust = 0.5),
        legend.position = "none",
        axis.text.y=element_blank(),
        text = element_text(size = 25)) +
  geom_richtext(aes(x = 2017, 
                    y = 0.4, 
                    label ="&#187;Mean:70.74%&#171;"),
                stat = "unique",
                size = 8,
                color = "black")

ggsave(
  "figs/iaph/genero_graf04B_filtimegd.png",
  bg = "white",
  width = 15,
  height = 8,
  dpi = 1500,
  plot = last_plot())

# Gráfico 05 | Filosofia | Relação Professor vs Discente | Barra ####
dadosfi |> 
  ggplot(aes(x = an_base, 
             fill = factor(g_oridis))) +
  geom_bar(position = "fill") +
  labs(title = "Gender Inequality in Graduate Programs in Philosophy in Brazil",
       subtitle = "Description according to the Relationship Supervisor/Candidate (1991-2021)",
       x = "",
       y = "",
       fill = "Supervisor/Candidate") +
  theme_classic() +
  scale_fill_d3() +
  scale_x_continuous(limits = c(1990, 2021), breaks = seq(1990, 2021, 5), expand = c(0, 0)) +
  scale_y_continuous(labels = scales::percent, position = "right") +
  theme(plot.title = element_markdown(face = "bold"),
        legend.position = "top",
        text = element_text(size = 30),
        legend.text = element_text(size = 30)) 

ggsave(
  "figs/iaph/graf5_relation.png",
  bg = "white",
  width = 25,
  height = 18,
  dpi = 900,
  plot = last_plot())

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
  rename("University" = "nm_entidade_ensino") 

# TABELA 3 ####
tabela_ies <- tab_ies |> 
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
    label = "Supervisor",  
    columns = c(total_o_Male, total_o_Female)) |>
  tab_spanner(
    label = "Candidate",
    columns = c(total_d_Male, total_d_Female)) |> 
  tab_spanner(
    label = "Supervisor/Candidate",
    columns = c(total_od_FF, total_od_FM, total_od_MF,total_od_MM)) |> 
  cols_label(
    total = "Theses",
    total_o_Male = "Man",
    total_o_Female = "Woman",
    total_d_Female = "Woman",
    total_d_Male = "Man",
    total_od_FF = "Woman/Woman",
    total_od_FM = "Woman/Man",
    total_od_MF = "Man/Woman",
    total_od_MM = "Man/Man") |> 
  tab_header(
    title = "Description of the Gender Distribution among the 10 most Productive University Institutions in Brazil between 1991-2021") |> 
  cols_align(
    align = "center") |> 
  fmt_number(
    drop_trailing_zeros = TRUE,
    decimals = 2,
    sep_mark = ".") 

# Salvar as tabelas - 2 Tabelas distintas
gtsave(tabela_ies,
       "figs/iaph/table3_10ies.png",
       vwidth = 2000, vheight = 3000)

