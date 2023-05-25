####Pacotes####
library(tidyverse)
library(here)
library(tidytext) # Manipulação de texto
library(textcat) # Detecção de resumos em outros idiomas

# Importação do banco limpo em "limpeza_catalogo.R"
dados <- read.csv("dados/catalogo.csv")

# Diagnóstico e customização de bi- e trigrams da filosofia####
# Método de frequência
# Bigrams
stop_pt <- tidytext::get_stopwords("pt")
bidados <- dados |> 
  tidytext::unnest_tokens(bigram, DS_RESUMO, token = "ngrams", n = 2) # Formação da variável bigram com todas palavras do resumo
bidados_sep <- bidados  |>   
  tidyr::separate(bigram, into = c("word1", "word2"), sep = " ") # Separação dos bigrams para remoção de stopwords
filobigrams <- bidados_sep |> 
  dplyr::filter(!word1 %in% stop_pt$word, # Remoção de stopwords em bigrams
         !word2 %in% stop_pt$word) |> # Remoção de stopwords em bigrams
  tidyr::unite("bigram", c(word1, word2), sep = " ") |> # Unificação dos bigrams novamente
  dplyr::count(bigram, sort = TRUE) |> # Contagem da frequência absoluta de cada bigram
  dplyr::filter(n >= 5)

# Salvar tabela com bigrams da filosofia
filobigrams |>
  readr::write_csv("dados/filobigrams.csv")

# Trigrams
tridados <- dados |> 
  tidytext::unnest_tokens(trigram, DS_RESUMO, token = "ngrams", n = 3) # Formação da variável trigram com todas palavras do resumo
tridados_sep <- tridados  |>   
  tidyr::separate(trigram, into = c("word1", "word2", "word3"), sep = " ") # Separação dos trigrams para remoção de stopwords
filotrigrams <- tridados_sep |> 
  dplyr::filter(!word1 %in% stop_pt$word, 
                !word2 %in% stop_pt$word,
                !word3 %in% stop_pt$word) |> # Remoção de stopwords em trigrams
  tidyr::unite("trigram", c(word1, word2, word3), sep = " ") |> # Unificação dos trigrams novamente
  dplyr::count(trigram, sort = TRUE) |> # Contagem da frequência absoluta de cada trigram
  dplyr::filter(n >= 5)

# Salvar tabela com trigrams da filosofia
filotrigrams |>
  readr::write_csv("dados/filotrigrams.csv")

#Incorporação de bi- e trigrams relevantes na variável DS_RESUMO####
filongrams <- c(
  #####Trigrams relevantes com até 05 ocorrências | Arquivo: dados/filotrigrams.csv##
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
  #####Bigrams relevantes com até 10 ocorrências | Arquivo: dados/filobigrams.csv##
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
  #Bigrams para exclusão (reconhecidas após alguns modelos STM)
  "diz respeito",
  "outro lado",
  "santa maria")

#Substituição dos ngrams na variável DS_RESUMO##
filograms <- str_replace_all(filongrams, " ", "")
names(filograms) <- c(filongrams)
