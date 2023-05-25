# Diagnóstico e customização de bi- e trigrams da filosofia####
# Método de frequência
# Bigrams
stop_pt <- tidytext::get_stopwords("pt")
bidados <- dados |> 
  tidytext::unnest_tokens(bigram, ds_resumo, token = "ngrams", n = 2) # Formação da variável bigram com todas palavras do resumo
bidados_sep <- bidados  |>   
  tidyr::separate(bigram, into = c("word1", "word2"), sep = " ") # Separação dos bigrams para remoção de stopwords
filobigrams <- bidados_sep |> 
  dplyr::filter(!word1 %in% stop_pt$word, # Remoção de stopwords em bigrams
         !word2 %in% stop_pt$word) |> # Remoção de stopwords em bigrams
  tidyr::unite("bigram", c(word1, word2), sep = " ") |> # Unificação dos bigrams novamente
  dplyr::count(bigram, sort = TRUE) |> # Contagem da frequência absoluta de cada bigram
  dplyr::filter(n >= 29) # Filtragem de bigrams com 29 ou mais ocorrências (n = 1019)

# Salvar tabela com bigrams da filosofia
filobigrams |>
  readr::write_csv("dados/filobigrams.csv")

# Trigrams
tridados <- dados |> 
  tidytext::unnest_tokens(trigram, ds_resumo, token = "ngrams", n = 3) # Formação da variável trigram com todas palavras do resumo
tridados_sep <- tridados  |>   
  tidyr::separate(trigram, into = c("word1", "word2", "word3"), sep = " ") # Separação dos trigrams para remoção de stopwords
filotrigrams <- tridados_sep |> 
  dplyr::filter(!word1 %in% stop_pt$word, 
                !word2 %in% stop_pt$word,
                !word3 %in% stop_pt$word) |> # Remoção de stopwords em trigrams
  tidyr::unite("trigram", c(word1, word2, word3), sep = " ") |> # Unificação dos trigrams novamente
  dplyr::count(trigram, sort = TRUE) |> # Contagem da frequência absoluta de cada trigram
  dplyr::filter(n >= 5) # Filtragem de bigrams com 10 ou mais ocorrências (n = 909)

# Salvar tabela com trigrams da filosofia
filotrigrams |>
  readr::write_csv("dados/filotrigrams.csv")

# NGRAMS ####
filongrams <- c(
  #####Trigrams relevantes com até 05 ocorrências | Arquivo: dados/filotrigrams.csv##
  "humano demasiado humano", 
  "assim falou zaratustra",
  "animais nao humanos",
  "sine qua non",
  "todos contra todos",
  "letre et le neant",
  "sein und zeit",
  "assim falava zaratustra", 
  "theory of justice", 
  #####Bigrams relevantes com até 10 ocorrências | Arquivo: dados/filobigrams.csv##
  "ser humano ",
  "natureza humana",
  "filosofia politica",
  "seculo xx",
  "razao pura",
  "filosofia moral",
  "seculo xix",
  "pensamento politico",
  "direitos humanos",
  "teoria critica",
  "razao pratica",
  "seculo xviii",
  "si mesma",
  "seres humanos",
  "condicao humana",
  "sociedade civil",
  "vida humana",
  "filosofia pratica",
  "direito natural",
  "teoria politica",
  "contrato social",
  "acao humana",
  "eterno retorno",
  "existencia humana",
  "lei moral",
  "esfera publica",
  "acao politica",
  "imperativo categorico",
  "economia politica",
  "identidade pessoal",
  "conhecimento cientifico",
  "santo agostinho",
  "lei natural",
  "ontologia fundamental",
  "teoria moral",
  "ciencias humanas",
  "pensamento ocidental",
  "hermeneutica filosofica",
  "investigacoes filosoficas",
  "seculo xvii",
  "analitica existencial",
  "acoes humanas",
  "espaco publico",
  "ciencia cognitiva",
  "tractatus logicophilosophicus",
  "bem comum",
  "ciencias naturais",
  "estado civil",
  "filosofia moderna",
  "filosofia transcendental",
  "idealismo transcendental",
  "livro i",
  "poder politico",
  "sumo bem",
  "dignidade humana",
  "paz perpetua",
  "filosofia critica",
  "industria cultural",
  "vida politica",
  "ciencia moderna",
  "estados mentais",
  "acao moral",
  "corpo politico",
  "liberalismo politico",
  "tecnica moderna",
  "filosofia analitica",
  "mundo sensivel",
  "sociedade moderna",
  "pessoa humana",
  "vontade livre",
  "antropologia filosofica",
  "deducao transcendental",
  "liberdade politica",
  "poder soberano",
  "filosofia primeira",
  "especie humana",
  "liberdade individual",
  "idealismo alemao",
  "metodo dialetico",
  "responsabilidade moral",
  "vida feliz",
  "espirito livre",
  "homem moderno",
  "juizos morais",
  "moral kantiana",
  "nomes proprios",
  "santo tomas",
  "teoria freudiana",
  "agir comunicativo",
  "consciencia moral",
  "posicao original",
  "seculo xvi",
  "acao comunicativa",
  "dialetica negativa",
  "gaia ciencia",
  "justica social",
  "propriedade privada",
  "razao suficiente",
  "agir humano",
  "metodo fenomenologico",
  "sociedade contemporanea",
  "sabedoria pratica",
  "cultura ocidental",
  "mundo exterior",
  "ordem social",
  "positivismo juridico",
  "teoria psicanalitica",
  "arte contemporanea",
  "estado moderno",
  "inteligencia artificial",
  "razao instrumental",
  "vida nua",
  "arte moderna",
  "ciencias sociais",
  "comunidade politica",
  "experiencia humana",
  "pensamento metafisico",
  "seculo xxi",
  "virtude moral",
  "arte tragica",
  "livre arbitrio",
  "processo historico",
  "aparelho psiquico",
  "behaviorismo radical",
  "dialetica hegeliana",
  "emancipacao humana",
  "entendimento humano",
  "essencia humana",
  "logica classica",
  "meio ambiente",
  "metafisica tradicional",
  "metodo cientifico",
  "racionalidade cientifica",
  "realidade social",
  "realismo cientifico",
  "selecao natural",
  "materialismo historico",
  "si mesmos",
  "comportamento humano",
  "cultura moderna",
  "direitos fundamentais",
  "primeiros principios",
  "razao humana",
  "sociedade capitalista",
  "atividade humana",
  "filosofia cartesiana",
  "filosofia natural",
  "idade media",
  "mundo externo",
  "mundos possiveis",
  "pensamento etico",
  "pensamento moderno",
  "principios metafisicos",
  "sentimentos morais",
  "seres vivos",
  "realidade objetiva",
  "revolucao francesa",
  "valor moral",
  "concepcao politica",
  "filosofia platonica",
  "grecia antiga",  
  "ontologia fenomenologica",
  "razao publica",
  "religiao crista",
  "si mesmas",
  "teoria etica",
  "ecce homo",
  "educacao estetica",
  "filosofia teorica",
  "genero humano",
  "mal radical",
  "mundo natural",
  "revolucoes cientificas",
  "boa vontade",
  "etica aristotelica",
  "filosofia ocidental",
  "forma logica",
  "investigacoes logicas",
  "atitude critica",  
  "conduta humana",
  "direito positivo",
  "estatuto ontologico",
  "historia natural",
  "leis naturais",
  "mundo moderno",
  "pacto social",
  "sistema juridico",
  "tragedia grega",
  "vida boa",
  "vontade humana",
  "animais naohumanos",
  "dialetica transcendental",
  "estados unidos",
  "livro x",
  "principio moral",
  "principio supremo",
  "psicanalise freudiana",
  "sociedade democratica",
  "conhecimento objetivo",
  "desobediencia civil",
  "fenomenologia husserliana",
  "filosofia antiga",
  "filosofia grega",
  "mecanica quantica",
  "ordem politica",
  "si proprio",
  "sociedade industrial",
  "teoria social",
  "vida cotidiana",
  "bem supremo",
  "drama barroco",
  "historia universal",
  "problema mentecorpo",
  "representacoes mentais",
  "sistema capitalista",
  "soberania popular",
  "amor fati",
  "discurso moral",
  "ilusao transcendental",
  "logica modal",
  "mundo fisico",
  "organizacao social",
  "relacoes internacionais",
  "transformacao social",
  "ciencias cognitivas",
  "consequencia logica",
  "critica imanente",
  "estado social",
  "filosofia aristotelica",
  "instituicoes politicas",
  "instituicoes sociais",
  "pesquisa cientifica",
  "primeiro principio",
  "principios fundamentais",
  "projeto politico",
  "representacao politica",
  "sociedades democraticas",
  "agir moral",
  "antiguidade classica",
  "ideologia alema",
  "justificacao epistemica",
  "revolucao cientifica",
  "sexto empirico",
  "sistema hegeliano",
  "teoria kantiana",
  "terceira critica",
  #Bigrams para exclusão (reconhecidos após rodar alguns modelos STM)
  "diz respeito",
  "outro lado",
  "santa maria")
# Salvar vetor para uso em stm_filosofia.R
readr::write_lines(filongrams, "dados/filongrams")

# Incorporação de ngrams relevantes na variável DS_RESUMO####
filograms <- str_replace_all(filongrams, " ", "")
names(filograms) <- c(filongrams)
dados <- dados |> 
  mutate(ds_resumo = str_replace_all(ds_resumo, pattern = filograms)) #substitui expressões compostas

#Diagnóstico de palavras mais frequentes#### 
filo_freqwords <- dados |> 
  tidytext::unnest_tokens(output = word, input = ds_resumo) |> # Separação de palavras dos resumos
  anti_join(get_stopwords("pt")) |> # Eliminação de stopwords (serão eliminadas posteriormente)
  anti_join(get_stopwords("en")) |> 
  filter(!str_length(word) <= 2) |> # Eliminação de palavras com 2 ou menos caracteres
  count(word, sort = TRUE) |> # Contagem
  filter(n >= 100)

# Salvar banco com palavras mais frequências de resumos de filosofia
filo_freqwords |> 
    readr::write_csv("dados/filofreqwords.csv")

# Listagem de stopwords personalizadas da filosofia#### 
filolixo <- "nao
ser
sobre
filosofia
partir
trabalho
pensamento
relacao
conceito
obra
teoria
sao
presente
objetivo
dissertacao
critica
modo
analise
forma
assim
segundo
tambem
pode
problema
questao
tese
tal
filosofo
conceitos
sentido
pesquisa
concepcao
fim
nocao
parte
capitulo
estudo
atraves
primeiro
tanto
possivel
desta
compreensao
enquanto
ideia
autor
dois
possibilidade
mostrar
dessa
busca
obras
apresenta
quais
tema
investigacao
acerca
saber
perspectiva
compreender
meio
proposta
filosofica
ponto
duas
tres
filosofico
primeira
alem
desse
pois
interpretacao
relacoes
ainda
propria
principais
momento
analisar
sera
quanto
trata
sendo
proprio
alguns
pensar
elementos
vez
desenvolvimento
contexto
questoes
leitura
aspectos
apenas
vista
pretende
reflexao
base
maneira
faz
portanto
projeto
estrutura
texto
papel
ate
cada
geral
deste
apresentar
deve
ideias
tais
neste
lugar
nova
outros
investigar
desde
sob
medida
textos
tendo
entao
todo
problemas
teorias
algumas
onde
fundamental
principal
abordagem
discussao
formas
importancia
toda
central
segunda
hipotese
consiste
procura
nesta
aqui
todos
diferentes
mesma
condicao
demonstrar
propoe
autores
condicoes
fazer
termos
debate
porque
ambito
escritos
podem
nesse
nocoes
longo
outras
uso
terceiro
campo
fundamentais
periodo
argumentos
disso
novo
tipo
sempre
seguida
sobretudo
dentro
capaz
criticas
principalmente
coisas
constitui
contra
melhor
qualquer
ultimo
necessario
livro
caminho
conceitual
visa
mostra
teorico
posicao
estabelecer
diante
torna
encontra
grande
definicao
limites
algo
temas
filosofos
somente
permite
todas
caso
especial
ter
entender
dimensao
diferenca
lado
doutrina
final
estao
destetrabalho
fundamentos
distincao
tentativa
resposta
teses
tarefa
capitulos
embora
momentos
maior
luz
filosoficas
nessesentido
nessa
percurso
podemos
apos
conteudo
outra
interior
entanto
caracteristicas
importante
dizer
partindo
resultado
termo
exposicao
problematica
aspecto
analisa
antes
solucao
literatura
parece
porem
especialmente
possui
pressupostos
teorica
defesa
chave
pontos
partes
torno
apresentada
presentes
procuramos
ultima
epoca
especificamente
inicio
possibilidades
realizacao
ambos
intuito
critico
reflexoes
anos
cuja
modos
defende
dar
contudo
desses
consequencias
possiveis
desenvolve
importantes
proposito
serao
estabelece
diversas
dizrespeito
influencia
tratado
dessas
conta
apresentamos
partida
estudos
filosoficos
finalmente
consideracoes
passa
tudo
apesar
inicialmente
via
realizar
afirma
existe
exemplo
menos
diversos
elaboracao
pensador
primeiros
formulacao
atual
buscamos
explicar
mediante
pergunta
implicacoes
novas
aborda
bases
dado
pensadores
conforme
pretendemos
centrais
possa
conclusao
necessaria
discutir
interpretacoes
desenvolvida
encontrar
considera
criterio
outrolado
investiga
feita
leva
consideracao
ensaio
identificar
apresentacao
primeiramente
tao
presenca
passagem
considerando
desenvolver
esclarecer
objetiva
responder
frente
mestrado
explicacao
sido
articulacao
surge
estatuto
examinar
tratar
poderia
escrita
cujo
aplicacao
reconstrucao
seguinte
significa
constituem
explicitar
tipos
plano
demais
resultados
tornar
implica
analisamos
quatro
dentre
entretanto
perspectivas
realiza
refere
visto
compreende
devem
apresentado
alternativa
surgimento
terceira
vai
certa
revela
buscando
estar
tematica
ocorre
propriamente
introducao
fio
teoricos
novos
programa
relevancia
abordar
analises
objetivos
pressuposto
grandes
oposicao
aproximacao
afirmar
aponta
decada
destes
entendida
sim
defender
expor
proposto
considerar
condutor
ver
considerado
contribuicao
resumo
mostrando
ora
coloca
criterios
gerais
entende
posicoes
unica
discussoes
caracteriza
comentadores
conhecer
durante
especifico
fase
propostas
quer
simples
considerada
unico
posteriormente
preciso
representa
universidade
justamente
alcancar
dada
tomando
VERDADEIRO
chamada
compreendida
dai
possibilita
serem
discute
metodologia
apontar
nestesentido
traducao
apresentadas
refletir
buscar
expressa
foco
caracterizacao
diferencas
baseada
diretamente
inicial
distintas
elucidar
hoje
ambas
casos
seculo
dificuldades
varias
discursos
favor
fonte
assume
contribuicoes
seguintes
alguma
muitos
conceituais
seguir
trabalhos
fundo
teoricas
precisamente
determinar
especifica
proprias
vem
livros
investigacoes
sustenta
nivel
avaliar
estrategia
vezes
estabelecimento
evidenciar
preocupacao
proprios
atualidade
fundamenta
iii
avaliacao
propor
varios
linhas
distintos
oferecer
todavia
respostas
segue
encontro
enfase
estudar
fundamentar
conclui
consequentemente
desenvolvido
ensaios
adequada
certo
mesmos
quadro
devido
essencialmente
forte
muitas
relevantes
realizada
caracteristica
exige
feito
veremos
destas
construir
olhar
encontram
superar
mostramos
chamado
expoe
verificar
leituras
dimensoes
envolve
fez
oferece
traz
diferente
fontes
logo
pretensao
apresentam
serie
alcance
escopo
chegar
postura
precisa
breve
deixa
filosofias
objecoes
apresentados
trajetoria
algum
conduz
buscou
determinada
entendido
passo
abordagens
conexao
perceber
visando
demonstra
falar
particularmente
passando
primeiras
certos
estabelecida
fazendo
conseguinte
toma
versao
concebe
encontramos
nela
ocupa
propomos
justificar
pensa
possuem
determinado
pesquisas
procurando
diz
inicia
defendida
sustentar
daquilo
destaca
destaque
fazem
tomada
apresentando
definir
investigamos
desdobramentos
divisao
pouco
campos
leitor
reconhecer
igualmente
tentar
bastante
conduta
destacando
ultimos
conhecido
anterior
atuais
depende
descrever
destacar
fornecer
pensada
examina
assunto
comparacao
compoem
reconstruir
relacionados
utiliza
certas
compreendido
questionamento
chama
identifica
nucleo
tracos
vinculo
amplo
relaciona
tenta
tornou
contribuir
explorar
respectivamente
influencias
abre
conceber
etc
existir
mantem
procurou
anteriores
articula
seguindo
permitem
corresponde
essenciais
consigo
itinerario
quarto
criar
considerados
buscaremos
levando
artigo
dando
las
los
correntes
definicoes
linha
determina
inclusive
analisaremos
elaborada
exigencia
consideramos
levar
revisao
situa
situacoes
curso
existem
metodologico
agora
nele
opera
posto
realizado
relevante
analisando
escrito
junto
indica
manutencao
mesmas
serve
ampla
constituir
descoberta
estaria
existentes
referencial
relacionadas
define
partimos
criticos
dito
vistas
enfoque
hipoteses
pressupoe
quase
configura
feitas
desafio
resolver
volta
dificuldade
fundamentalmente
seculos
claro
concebida
confronto
descreve
referencias
resulta
desdobramento
passagens
dias
elabora
recusa
trazer
analisada
debates
especificos
alternativas
bibliografica
secao
sugere
tornando
tratados
evidente
formulacoes
ademais
completa
interpretes
mostraremos
profunda
estrategias
haver
interpretar
utilizado
vir
atingir
garantir
indicar
tempos
colocar
abordamos
caracterizar
enfim
insere
simplesmente
acima
apresentaremos
cerne
entendemos
areas
concerne
conteudos
estilo
otica
promover
versa
baseado
denominada
deu
divide
manter
parana
privilegiado
situar
total
acreditamos
chega
disciplina
estadual
explicita
expressoes
tentamos
clara
constatacao
contato
etapas
constroi
decorrer
estabelecido
defendemos
marcada
obstante
orientacao
analisados
conhecida
discutimos
dita
metodologica
procuraremos
oeste
relacionar
sequencia
solucoes
toledo
conclusoes
desenvolvidas
nunca
referido
ressaltar
talvez
cabe
pensado
titulo
basicos
consideradas
exemplos
referida
utilizada
comeca
doutrinas
fundada
peculiar
possam
produz
reconhece
reflexivo
aparentemente
denominado
inerente
mera
notadamente
panorama
contem
devemos
emerge
pano
caracterizada
deixar
acaba
dividida
observar
utilizando
der
empreendimento
etapa
tange
elaborar
fruto
metade
podera
produzir
tomar
querer
tecnicas
cinco
envolvidos
exposto
tratamos
daquele
efetivamente
exposta
fornece
fundamentada
seguranca
assumir
consegue
constituido
decadas
permanece
posteriores
problematizacao
analisado
apontando
argumenta
centralidade
utilizados
completamente
consequente
dao
fica
percebe
remete
tentativas
absolutamente
aparecem
consistencia
desenvolveu
envolvem
exclusivamente
publicacao
tornam
utilizacao
acontece
analisadas
contextos
formulada
intento
levou
reflete
concluimos
deveria
area
basicas
concluir
denomina
determinadas
surgem
acompanhar
comentario
dividido
levam
necessarios
poe
resgate
marco
perda
puramente
simultaneamente
totalmente
apresento
cunho
daquela
explica
iremos
passou
perpassa
ultimas
concentra
cria
exigencias
parecem
propostos
correta
marcado
meramente
pelotas
reside
abordado
assuntos
cartas
determinados
basica
carta
doutorado
especificidade
relacionada
requer
resolucao
ali
aproxima
atribui
chamamos
concebido
demonstrando
desenvolvidos
estuda
levaram
padroes
realmente
criticamente
funda
impoe
limitacoes
maiores
relativas
revelar
explicitacao
federal
passos
reformulacao
tratam
aplicada
estritamente
ligada
questionamentos
tentaremos
conduzir
detrimento
examinamos
observacoes
verdadeiras
elucidacao
faremos
poderiam
questionar
argumentamos
artigos
definida
exatamente
fazemos
formular
percorrer
apreender
articular
consistente
construido
dedicado
distinguir
estudiosos
finais
juntamente
representar
senao
vias
comeco
construida
elaborado
especificas
significativa
tracar
abordada
cursos
definido
diferentemente
ganha
mestre
norte
publicado
adequacao
chamar
plenamente
servir
topicos
transforma
abordados
ampliacao
apontam
dedica
distincoes
edicao
exerce
imprescindivel
inclui
intitulado
procurar
usos
tica
tico
eio
santamaria
oes
rio
vel
amplo
indicando
etc
cent
lise
apresente
difere
explora
sertacao
pertencera
humanidade
humano"

# Salvar vetor para uso em stm_filosofia.R####
readr::write_lines(filolixo, "dados/filolixo")
# Transformação em tibble para uso no anti_join####
filolixo <- tibble(word = unlist(str_split(filolixo, "\n")))
