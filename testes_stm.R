library(gganimate)
library(transformr)
library(geomtextpath)

# GIF####
tidystm_ano <- tidystm_ano |> 
mutate(label = str_replace_all(label, "\\(Covariate Level: 1988\\)", ""),
       topic = factor(topic)) 
  
graf1 <-  
  ggplot(tidystm_ano, aes(x = covariate.value,
             y = estimate,
             color = topic,
             group = topic)) +
  geom_labelsmooth(aes(label = label),
                       method = "lm",
                   linewidth = 2,
                   fontface = "bold",
                       formula = y ~ x,
                       se = FALSE) +
  theme_classic() +
  scale_color_manual(values = met.brewer("Cross", 80)) +
  transition_states(states = topic,
                    transition_length = 5,
                    state_length = 1,
                    wrap = FALSE) +
  labs(title = "Trends of Topics in Dissertations",
       subtitle = "Brazilian Philosophy Graduate Programs (1988-2021)",
       x = "Year",
       y = "Point Estimation"
  ) +
  theme(plot.title = element_markdown(face = "bold"),
        text = element_text(size = 15),
        legend.position = "none")

graf1 <- graf1 +  
  enter_fade() + 
  shadow_mark(color = "gray", 
              alpha = 0.5) 

anim_save("figs/trends_year.gif", 
          graf1,
          duration = 80)

# CATEGORY LABELING####
gammawide <- tidygamma  |>  
  select(document, category, gamma) |> 
  summarise(gamma = mean(gamma), .by = c(category, document))  |> 
  pivot_wider(id_cols = document,
              names_from = category, 
              values_from = gamma) 
gamma_matrix <- as.matrix.data.frame(gammawide[,2:9], 
                                     rownames.force = TRUE)
rownames(gamma_matrix) <- unique(gammawide$document)
gamma_matrix[1:5, 1:2]

me_gamma.pca <- prcomp(gamma_matrix)
me_gamma_umap <- umap(me_gamma.pca$x, 
                      n_neighbors = 4,
                      n_epochs = 500,
                      spread = 6,
                      min_dist = .7,
                      seed = 1876
)
me_gamma_umap.pred <- predict(me_gamma_umap, me_gamma.pca$x)
me_gamma_umap.pred |>  head(5)


cat_labels <- tidygamma  |> 
  group_by(document) |> 
  slice_max(order_by = gamma) |> 
  ungroup() |> 
  distinct(document, .keep_all = TRUE)


stm_ano  |>  
  ggplot(
    aes(
      covariate.value, estimate, 
      color = topic, 
      fill = topic
    )
  ) +
  geom_stream()


teste <- tidygamma |> 
  summarize(gamma_ano = sum(gamma), .by = c(category, an_base)) |> 
  filter(category != "Excluídos")

teste |>
  ggplot(aes(x = an_base,
             y = gamma_ano,
             color = category)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", 
              formula = y ~ poly(x, 3), 
              se = FALSE,
              linewidth = .9) +
  scale_x_continuous(limits = c(1988, 2021)) +
  theme_classic() +
  labs(x = "Year",
       y = "Gamma",
       color = "Category",
       title = "Trends of Categories in Dissertations",
       subtitle = "Brazilian Philosophy Graduate Programs (1988-2021)") +
  scale_color_manual(values = met.brewer("Cross", 7)) +
  theme(legend.position = "top",
        plot.title = element_markdown(face = "bold"),
        text = element_text(size = 15))



theme(plot.title = element_markdown(face = "bold"),
      plot.subtitle = element_markdown(),
      legend.position = "top",
      text = element_text(size = 30)) +
  coord_cartesian(clip = 'off') 

##











# Rotulação de categorias dos documentos
cat_labels <- tidygamma |> 
  group_by(document, category) |> 
  summarise(gamma = mean(gamma)) |> 
  slice_max(gamma, n = 1, with_ties = FALSE)  

gamma_wide <- tidygamma  |> 
  group_by(category, document) |> 
  summarize(gamma = mean(gamma)) |> 
  pivot_wider(id_cols = document,
              names_from = category, 
              values_from = gamma) 


# Rotulação de tópicos dos documentos
cat_topic <- tidygamma |> 
  group_by(document, topic) |> 
  summarise(gamma_topic = mean(gamma)) |> 
  slice_max(gamma_topic, n = 1, with_ties = FALSE)

# Junção do banco
cat_labels <- cat_labels |> 
  left_join(cat_topic, by = "document")

umap_recipe <- recipe(~., data = gamma_wide)  |> 
  update_role(document, new_role = "id") |> 
  step_normalize(all_predictors())  |> 
  step_umap(all_predictors())

umap <- prep(umap_recipe)

# UMAP ####
# Preparação do banco
gammawide <- tidygamma  |>  
  group_by(category, document) |> 
  summarise(cat_gamma = mean(gamma), .groups = 'drop')  |> 
  pivot_wider(id_cols = document, ,
              names_from = category, 
              values_from = cat_gamma) 

# UMAP via recipe
umap_recipe <- recipe(~., data = gammawide)  |> 
  update_role(document, new_role = "id") |> 
  step_normalize(all_predictors())  |> 
  step_umap(all_predictors())
umap_model <- prep(umap_recipe)

# Plot do UMAP
juice(umap_model)  |> 
  ggplot(aes(UMAP1, UMAP2, label = topic)) +
  geom_point(aes(fill = category), alpha = .8, size = 4, shape = 21) +
  geom_text(check_overlap = TRUE, size = 3, color = "white") +
  #  scale_fill_manual(values = met.brewer("Cross", 8)) +
  theme_minimal() 


#











category_labels |> 
  ggplot(aes(x = an_base, color = category)) +
  geom_freqpoly(binwidth = 1, linewidth = 1.2) +
  scale_x_continuous(limits = c(1987, 2021)) +
  scale_y_continuous(limits = c(0, 200), position = "right") +
  scale_color_manual(values = met.brewer("Cross", 8))  



prop_by_category <- category_labels |> 
  drop_na() |> 
  group_by(category, g_orientador) |> 
  summarise(count = n()) |> 
  ungroup() |> 
  mutate(prop = count / sum(count))
ggplot(prop_by_category, aes(x = category, y = prop, fill = g_orientador)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Category", y = "Proportion", fill = "Gender") +
  theme_minimal() +
  scale_fill_manual(values = met.brewer("Austria", 2))  

category_labels  |> 
  ggplot(aes(x = an_base, color = category, group = category)) +
  geom_line(stat = "bin", binwidth = 1, size = 1.2, alpha = 0.5) +
  geom_point(aes(y = ..count..), stat = "bin", binwidth = 1, size = 1.5, alpha = 0.7) +
  scale_x_continuous(limits = c(1987, 2021)) +
  scale_y_continuous(limits = c(0, 200), position = "right") +
  scale_color_manual(values = met.brewer("Austria", 8)) +
  theme_minimal() +
  labs(x = "Ano", 
       color = "Categoria",
       title = "Número de trabalhos de filosofia defendidos por categoria",
       subtitle = "Elaboração: Os autores | Dados: Catálogo de Teses e Dissertações (CAPES)")

# UMAP####
#Transformação de matriz theta com informações
# Transformação de matriz thetha com informações
# Gamma matrix sem categorias
cat_labels <- tidygamma |> 
  group_by(document, category) |> 
  summarise(gamma = mean(gamma)) |> 
  slice_max(gamma, n = 1, with_ties = FALSE) 


group_by(document, category) %>% 
  summarise(gamma = mean(gamma)) %>%
  slice_max(gamma, n = 1, with_ties = FALSE)
pivot_wider(names_from = topic, 
            values_from = gamma)


teste <- tidygamma  |> 
  group_by(document) |> 
  slice_max(order_by = gamma) |> 
  ungroup() |> 
  distinct(document, .keep_all = TRUE)
# Rotulação de tópicos com base na medida gamma média
theta <- tidygamma |> 
  left_join(cat_labels  |>  
              select(document, topic, category), by = "document")

#Gamma matrix com categorias 
gammawise <- tidygamma |>
  select(document, category, topic, gamma) |> 
  group_by(category, document) |> 
  summarise(gamma = mean(gamma), .groups = 'drop') |> 
  pivot_wider(id_cols = document, 
              names_from = category, 
              values_from = gamma)

gamma_matrix1 <- as.matrix.data.frame(gamma_matrix[,2:8], rownames.force = TRUE)
rownames(gamma_matrix1) <- unique(gamma_matrix$document)

me_gamma.pca <- prcomp(gamma_matrix1)
me_gamma_umap <- umap(me_gamma.pca$x, 
                      n_neighbors = 4,
                      n_epochs = 500,
                      spread = 6,
                      min_dist = .7,
                      seed = 1876
)
me_gamma_umap.pred <- predict(me_gamma_umap, me_gamma.pca$x)
me_gamma_umap.pred  |>  head(5)

# Outra análise ####
theta_matrix <- tidygamma |> 
  select(document, topic)



umap_rec <- recipe(~., data = tidygamma)  |> 
  update_role(document, topic, category, an_base, g_orientador, new_role = "id") |> 
  step_normalize(all_predictors())  |> 
  step_umap(all_predictors())
umap_prep <- prep(umap_rec)

#Gráfico
#Extrair docs exemplares
docs_labels <- category_labels  |> 
  group_by(document)  |> 
  slice_max(order_by = gamma) |> 
  ungroup() |> 
  pull(document)

# Plot do UMAP
juice(umap_prep)  |> 
  ggplot(aes(UMAP1, UMAP2, fill = category, label = topic)) +
  geom_point(aes(color = category), alpha = 0.5, size = 6) +
  geom_text(check_overlap = TRUE, size = 5) +
  scale_color_manual(values = met.brewer("Austria", 8)) +
  theme_minimal() 

juice(umap) |> 
  ggplot(aes(UMAP1, UMAP2))+
  geom_point()

juice(umap_prep)  |> 
  ggplot(aes(UMAP1, UMAP2, label = topic)) +
  geom_point(aes(color = topic), shape = 19, alpha = 0.2, size = 14) +
  geom_text(check_overlap = TRUE) + 
  scale_color_manual(values = met.brewer("Cross", 80)) +
  theme_minimal() +
  theme(legend.position = "none")

#Salvar gráfico
ggsave(
  "figs/umap_m80_tema1.png",
  bg = "white",
  width = 20,
  height = 12,
  dpi = 300,
  plot = last_plot())

#Transformação de matriz thetha com informações
stm_gamma <- tidy(topic_model,
                  matrix = "gamma",
                  document_names = rownames(filosparse))  |>  
  group_by(topic) |>  
  arrange(topic, desc(gamma)) |>  
  mutate(topic = as_factor(topic))

gamma_matrix <- stm_gamma |> 
  pivot_wider(names_from = topic, 
              values_from = gamma)

topic_labels <- stm_gamma  |> 
  group_by(document) |> 
  slice_max(order_by = gamma) |> 
  ungroup()

theta_matrix <- gamma_matrix |> 
  left_join(topic_labels  |>  
              select(document, topic), by = "document")


umap_rec <- recipe(~., data = theta_matrix) %>%
  update_role(document, topic, new_role = "id") %>%
  step_normalize(all_predictors()) %>%
  step_umap(all_predictors())
umap_prep <- prep(umap_rec)

#Gráfico
#Extrair docs exemplares
docs_labels <- topic_labels %>%
  group_by(document) %>%
  slice_max(order_by = gamma) %>%
  ungroup() |> 
  pull(document)

# Plot do UMAP
juice(umap_prep)  |> 
  ggplot(aes(UMAP1, UMAP2, label = topic)) +
  geom_point(aes(color = topic), shape = 19, alpha = 0.2, size = 14) +
  geom_text(check_overlap = TRUE) + 
  scale_color_manual(values = met.brewer("Cross", 77)) +
  theme_minimal() +
  theme(legend.position = "none")




#Modelos Múltiplos (código de Julia Silge)####
#Modelo com múltiplos K####
plan(multisession)
many_models <- tidyr::tibble(K = c(75, 77, 78, 79, 80, 85)) |> #Teste de modelos com 40 a 80 Tópicos
  dplyr::mutate(topic_model = furrr::future_map(K, ~ stm::stm(filosparse, 
                                                              K = .,
                                                              prevalence = ~g_orientador + s(an_base),
                                                              seed = 1987,
                                                              data = covars,
                                                              init.type = "Spectral"),
                                                .options = furrr_options(seed = TRUE)))
#Gráficos de diagnóstico dos modelos
heldout <- stm::make.heldout(filosparse) 
k_result <- many_models |> # Cria banco com resultados de cada modelo
  dplyr::mutate(exclusivity = purrr::map(topic_model, exclusivity),
                semantic_coherence = purrr::map(topic_model, semanticCoherence, filosparse),
                eval_heldout = purrr::map(topic_model, eval.heldout, heldout$missing),
                residual = purrr::map(topic_model, checkResiduals, filosparse),
                bound =  purrr::map_dbl(topic_model, function(x) max(x$convergence$bound)),
                lfact = purrr::map_dbl(topic_model, function(x) lfactorial(x$settings$dim$K)),
                lbound = bound + lfact,
                iterations = purrr::map_dbl(topic_model, function(x) length(x$convergence$bound)))

#Critérios de avaliação de modelos | Held-out Likelihood, Coerência Semântica, Resíduos e Lower Bound ####
theme_set(theme_minimal(base_family = "Roboto"))

k_result |>  
  transmute(K,
            `Limite Inferior` = lbound,
            Resíduos = map_dbl(residual, "dispersion"),
            `Coerência Semântica` = map_dbl(semantic_coherence, mean),
            `Verossimilhanca retida (held-out likelihood)` = map_dbl(eval_heldout, "expected.heldout")) %>%
  gather(Metric, Value, -K) |> 
  ggplot(aes(K, Value, color = Metric)) +
  geom_line(linewidth = 1.5, alpha = 0.9, show.legend = FALSE) +
  facet_wrap(~Metric, scales = "free_y") +
  labs(x = "K (número de Tópicos)",
       y = NULL,
       title = "Diagnóstico do número de tópicos (K) para o modelo",
       subtitle = "O intervalo entre 70 e 80 tópicos parece ser o mais apropriado | Elaboração: Os autores")

#Gráfico Exclusividade x Coerência Semântica por tópicos
k_result |>  
  select(K, exclusivity, semantic_coherence)  |> 
  filter(K %in% c(30, 40, 50, 60, 70, 75, 77, 78, 79, 80, 90, 100))  |> 
  unnest(cols = c(exclusivity, semantic_coherence))  |> 
  mutate(K = as.factor(K)) |> 
  ggplot(aes(semantic_coherence, exclusivity, color = K)) +
  geom_point(size = 2.5, alpha = 0.9) +
  theme_minimal() +
  scale_color_manual(values = met.brewer("Renoir", 12)) +
  labs(x = "Coerência Semântica",
       y = "Exclusividade",
       title = "Comparação entre Coerência Semântica e Exclusividade",
       subtitle = "O intervalo entre 65 e 80 tópicos parece ser o mais apropriado | Elaboração: Os autores")

#Escolha do modelo com número adequado de tópicos
topic_model <- k_result  |>  
  filter(K == 85) |> 
  pull(topic_model)  %>%   
  .[[1]]


# Categorias Carolina
categorias <- dplyr::tibble(
  topic = as_factor(unlist(list(
    c(2,3,12,18,20,25,30,32,33,39,45,46,47,48,49,53,68,69,77), # Política
    c(5,8,14,16,17,22,26,31,35,44,56,63,64,65,66,67,71,74,76,80), # Lógica, Linguagem e Ciência
    c(6,7,15,19,34,40,42,43,52,57,60,61,79), # Moral
    c(11,21,23,54,70,73,75,78), # Metafísica
    c(4,10,24,36,37,50,51,55,62), # Estética 
    c(9,13,27,28,29,41,58,59,72), # Psicologia e Filosofia da Mente
    c(1,38) # Excluir
  ))),
  category = as_factor(c(
    rep("Filosofia Social e Política", 19),
    rep("Lógica, Linguagem e Ciência", 20),
    rep("Filosofia Moral", 13),
    rep("Metafísica", 8),
    rep("Estética", 9),
    rep("Psicologia e Filosofia da Mente", 9),
    rep("Excluídos", 2)
  )
  ))
