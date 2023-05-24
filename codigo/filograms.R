####Pacotes####
library(tidyverse)
library(here)
library(tidytext) # Manipulação de texto
library(textcat) # Detecção de resumos em outros idiomas
library(stopwords) # Dicionário de stopwords

# Importação do banco limpo em "limpeza_catalogo.R"
dados <- read.csv("dados/catalogo.csv")

#Diagnóstico e customização de bi- e trigrams da filosofia####
#Método de frequência
#Bigrams
lixopt <- get_stopwords("pt")
bidados <- dados |> 
  unnest_tokens(bigram, resumo, token = "ngrams", n = 2) #bigrams com todas palavras
bidados_sep <- bidados  |>   
  separate(bigram, into = c("word1", "word2"), sep = " ") #banco com bgrams separadas
bidados_uni <- bidados_sep |> 
  filter(!word1 %in% lixopt$word,
         !word2 %in% lixopt$word) |> #remoção de bigrams com stopwords
  unite("bigram", c(word1, word2), sep = " ") |> #Banco unificado com bigrams
  count(bigram, sort = TRUE) |> #maior frequência de bigrams
  filter(n >= 5) |> 
  arrange(desc(n))
View(bidados_uni)
#Trigrams
tridados <- dados |> 
  unnest_tokens(trigram, resumo, token = "ngrams", n = 3) #bigrams
tridados_sep <- tridados  |>   
  separate(trigram, into = c("word1", "word2", "word3"), sep = " ")
tridados_uni <- tridados_sep |> 
  filter(!word1 %in% lixopt$word,
         !word2 %in% lixopt$word,
         !word3 %in% lixopt$word) |> 
  unite("trigram", c(word1, word2, word3), sep = " ") |> 
  count(trigram, sort = TRUE) |> 
  filter(n >= 5) |> 
  arrange(desc(n))
View(tridados_uni)

write.csv(bidados_uni, "23-05_filo_bidados.csv")
write.csv(tridados_uni, "23-05_filo_tridados.csv")

#Formação de bi- e trigrams####
filo_ngrams <- c(
  #######################Trigrams com até 10 ocorrências | Ver File 23-04_filotrigrams.csv##
  "tractatus logico philosophicus", 
  "humano demasiado humano", 
  "animais nao humanos",
  "assim falou zaratustra",
  "problema mente corpo",
  "manuscritos economico filosoficos", 
  "ortega y gasset",
  "relacao mente corpo",
  "tratado teologico politico", 
  "sociedade bem ordenada", 
  "sein und zeit", 
  "assim falava zaratustra", 
  "theory of justice", 
  "logicas nao classica",
  "sine qua non",
  #"prmuxis", 
  #######################Bigrams | Ver File filobigrams##
  "ser humano ",
  "natureza humana",
  "filosofia politica",
  "seculo xx",
  "ensino medio",
  "razao pura",
  "filosofia moral",
  "seculo xix",
  "direitos humanos",
  "pensamento politico",
  "teoria critica",
  "razao pratica",
  "seculo xviii",
  "condicao humana",
  "seres humanos",
  "sociedade civil",
  "vida humana",
  "filosofia pratica",
  "direito natural",
  "livre arbitrio",
  "teoria politica",
  "contrato social",
  "ser ai",
  "acao humana",
  "existencia humana",
  "senso comum",
  "eterno retorno",
  "lei moral",
  "esfera publica",
  "conhecimento cientifico",
  "imperativo categorico",
  "acao politica",
  "economia politica",
  "identidade pessoal",
  "santo agostinho",
  "ciencias humanas",
  "ontologia fundamental",
  "lei natural",
  "teoria moral",
  "vontade geral",
  "hermeneutica filosofica",
  "valores morais",
  "investigacoes filosoficas",
  "pensamento ocidental",
  "seculo xvii",
  "experiencia estetica",
  "analitica existencial",
  "filosofia contemporanea",
  "espaco publico",
  "acoes humanas",
  "bem estar",
  "teorias cientificas",
  "industria cultural",
  "sumo bem",
  "ciencia cognitiva",
  "bem comum",
  "liberdade humana",
  "ciencias naturais",
  "poder politico",
  "dignidade humana",
  "filosofia moderna",
  "filosofia transcendental",
  "idealismo transcendental",
  "estado civil",
  "livro i",
  "filosofia critica",
  "vida politica",
  "auto organizacao",
  "paz perpetua",
  "ciencia moderna",
  "mundo contemporaneo",
  "tecnica moderna",
  "estados mentais",
  "vida social",
  "realidade humana",
  "acao moral",
  "corpo politico",
  "liberalismo politico",
  "ma fe",
  "deducao transcendental",
  "filosofia analitica",
  "pessoa humana",
  "poder soberano",
  "agir comunicativo",
  "filosofia primeira",
  "mundo sensivel",
  "sociedade moderna",
  "liberdade politica",
  "vontade livre",
  "acao comunicativa",
  "antropologia filosofica",
  "metodo fenomenologico",
  "sociedade contemporanea",
  "consciencia moral",
  "homem moderno",
  "idealismo alemao",
  "liberdade individual",
  "metodo dialetico",
  "especie humana",
  "moral kantiana",
  "posicao original",
  "vida feliz",
  "espirito livre",
  "materialismo historico",
  "teoria freudiana",
  "dialetica negativa",
  "responsabilidade moral",
  "juizos morais",
  "pos modernidade",
  "seculo xvi",
  "processo historico",
  "propriedade privada",
  "santo tomas",
  "justica social",
  "razao suficiente",
  "ser livre",
  "tradicao metafisica",
  "pensamento nietzschiano",
  "teoria estetica",
  "estado democratico",
  "filosofia hegeliana",
  "gaia ciencia",
  "nomes proprios",
  "ordem social",
  "razao instrumental",
  "sabedoria pratica",
  "seculo xxi",
  "agir humano",
  "ciencias sociais",
  "cultura ocidental",
  "teoria psicanalitica",
  "comunidade politica",
  "estado moderno",
  "experiencia humana",
  "positivismo juridico",
  "realidade social",
  "inteligencia artificial",
  "logica classica",
  "meio ambiente",
  "metodo cientifico",
  "mundo exterior",
  "vida nua",
  "virtude moral",
  "emancipacao humana",
  "etica nicomaqueia",
  "pensamento heideggeriano",
  "sociedade capitalista",
  "behaviorismo radical",
  "educacao basica",
  "entendimento humano",
  "racionalidade cientifica",
  "realidade objetiva",
  "realismo cientifico",
  "selecao natural",
  "aparelho psiquico",
  "comportamento humano",
  "cultura moderna",
  "desenvolvimento historico",
  "dialetica hegeliana",
  "metafisica tradicional",
  "pensamento moderno",
  "revolucao francesa",
  "atividade humana",
  "filosofia natural",
  "idade media",
  "momento historico",
  "razao humana",
  "sentimentos morais",
  "direitos fundamentais",
  "grecia antiga",
  "mundo moderno",
  "filosofia cartesiana",
  "mal radical",
  "mundo externo",
  "mundos possiveis",
  "nivel medio",
  "pos moderna",
  "razao publica",
  "seres vivos",
  "vida pratica",
  "a priori",
  #Bigrams para EXCLUSÃO POSTERIOR
  "deste trabalho",
  "nesse sentido",
  "neste sentido",
  "diz respeito",
  "outro lado",
  "santa maria")

#Substituição dos ngrams##
filograms <- str_replace_all(filo_ngrams, " ", "")
names(filograms) <- c(filo_ngrams)