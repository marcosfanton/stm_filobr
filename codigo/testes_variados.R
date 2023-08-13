filowords1 <- filowords %>%
  group_by(doc_id) %>%
  mutate(ds_resumo = paste(word, collapse = " ")) %>%
  ungroup() %>%
  distinct(doc_id, ds_resumo, .keep_all = TRUE) %>%
  select(-word)

processed <- stm::textProcessor(documents = filowords1$ds_resumo, 
                             metadata = filowords1,
                             stem = FALSE)

out <- prepDocuments(processed$documents, 
                     processed$vocab, 
                     processed$meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta
plotRemoved(processed$documents, lower.thresh=seq(1,200, by=100))

search_results <- searchK(out$documents, 
                          out$vocab,
                          K = c(30, 40, 50, 60, 70, 80, 90),
                          N = 5000,
                          heldout.seed = 1987,
                          prevalence =~g_orientador + s(an_base),
                          init.type = "Spectral",
                          data = meta)

plot(search_results)
