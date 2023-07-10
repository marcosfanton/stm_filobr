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
  "ortega y gasset",
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
  #Bigrams para exclusão (reconhecidos após rodar alguns modelos STM)
  "diz respeito",
  "outro lado",
  "santa maria")
# Salvar vetor para uso em stm_filosofia.R
readr::write_lines(filongrams, "dados/filongrams")
#Diagnóstico de palavras mais frequentes#### 
filo_freqwords <- dados |> 
  tidytext::unnest_tokens(output = word, input = ds_resumo) |> # Separação de palavras dos resumos
  anti_join(get_stopwords("pt")) |> # Eliminação de stopwords (serão eliminadas posteriormente)
  anti_join(get_stopwords("en")) |> 
  filter(!str_length(word) <= 2) |> # Eliminação de palavras com 2 ou menos caracteres
  count(word, sort = TRUE) |> # Contagem de palavras
  filter(n >= 100)

# Salvar banco com palavras mais frequências de resumos de filosofia
filo_freqwords |> 
    readr::write_csv("dados/filofreqwords.csv")

# Listagem de stopwords personalizadas da filosofia#### 
filolixo <- 
"nao
ser
sobre
filosofia
trabalho
partir
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
homem
analise
assim
segundo
tambem
questao
problema
tese
tal
pode
filosofo
conceitos
pesquisa
concepcao
fim
nocao
capitulo
estudo
parte
atraves
primeiro
tanto
possivel
enquanto
desta
autor
possibilidade
dois
deste
mostrar
dessa
investigacao
obras
quais
tema
acerca
perspectiva
meio
proposta
saber
compreender
duas
ponto
tres
primeira
filosofica
desse
busca
pois
interpretacao
alem
ainda
momento
relacoes
propria
principais
apresenta
analisar
sera
quanto
neste
desenvolvimento
filosofico
sendo
vez
alguns
elementos
pensar
nesse
contexto
proprio
questoes
aspectos
apenas
leitura
vista
geral
base
maneira
portanto
papel
cada
ate
texto
projeto
ideias
nova
tais
lugar
apresentar
teorias
faz
sob
outros
medida
investigar
desde
deve
textos
entao
problemas
tendo
onde
todo
fundamental
algumas
abordagem
principal
discussao
pretende
importancia
formas
central
segunda
consiste
toda
hipotese
nesta
aqui
diferentes
demonstrar
todos
autores
ambito
termos
debate
porque
fazer
podem
escritos
longo
outras
formacao
nocoes
terceiro
campo
disso
periodo
sempre
novo
tipo
fundamentais
sobretudo
propoe
seguida
dentro
procura
capaz
principalmente
criticas
mesma
trata
qualquer
melhor
caminho
visa
tratase
diante
estabelecer
definicao
grande
livro
contra
constitui
temas
todas
algo
filosofos
somente
especial
entender
lado
permite
diferenca
estao
ter
doutrina
distincao
concepcoes
tentativa
tarefa
capitulos
momentos
resposta
teses
luz
apos
embora
maior
percurso
podemos
nessa
caracteristicas
conteudo
outra
acordo
entanto
resultado
problematica
antes
importante
mostra
filosoficas
aspecto
referencia
solucao
termo
especialmente
defesa
dizer
possui
literatura
partindo
porem
pressupostos
torno
presentes
pontos
parece
apresentada
ultima
inicio
especificamente
cuja
procuramos
anos
epoca
intuito
encontra
ambos
reflexoes
modos
realizacao
tratado
desses
consequencias
proposito
contudo
teorica
serao
importantes
diversas
influencia
dar
dizrespeito
conta
estudos
dessas
estabelece
partida
apresentamos
consideracoes
apesar
tudo
finalidade
inicialmente
elaboracao
passa
menos
via
exemplo
existe
exame
diversos
mediante
filosoficos
pensador
realizar
finalmente
atual
implicacoes
presenca
desenvolve
analisa
buscamos
afirma
dado
explicar
pergunta
torna
john
novas
centrais
desenvolvida
intelectual
outrolado
conforme
discutir
possa
pretendemos
necessaria
pensadores
defende
interpretacoes
conclusao
feita
palavras
aparece
apresentacao
consideracao
ensaio
primeiramente
primeiros
sido
identificar
encontrar
responder
surge
esclarecer
frente
reconstrucao
mestrado
desenvolver
examinar
tao
cujo
poderia
significa
leva
explicitar
tipos
seguinte
quatro
aborda
demais
implica
constituem
resultados
visto
analisamos
plano
descricao
perspectivas
dentre
devem
entretanto
tratar
apresentado
considerando
surgimento
tematica
certa
investiga
vai
programa
ocorre
atencao
introducao
relevancia
abordar
buscando
propriamente
teoricos
analises
fio
novos
objetivos
aproximacao
grandes
decada
pretendese
destes
contribuicao
proposto
considera
sim
expor
entendida
considerado
criterios
serem
afirmar
ora
terceira
defender
condutor
considerar
estar
fase
considerada
realiza
durante
discussoes
especifico
mostrando
simples
aponta
propostas
comentadores
conhecer
quer
representa
ver
alcancar
justamente
preciso
refletir
compreendida
dai
viver
apontar
chamada
posteriormente
metodologia
possibilita
diferencas
foco
revela
diretamente
distintas
baseada
apresentadas
caracterizacao
inicial
dificuldades
hoje
seguintes
ambas
elucidar
varias
casos
expressa
favor
compreende
coloca
contribuicoes
procedimento
nivel
teoricas
vem
alguma
fundo
refere
trabalhos
muitos
seguir
especifica
fala
precisamente
conceituais
tomando
vezes
apresentase
assume
estabelecimento
evidenciar
varios
avaliar
buscase
linhas
tornase
avaliacao
estrategia
proprias
buscar
determinar
livros
face
atualidade
proprios
distintos
propor
certo
consequentemente
estudar
respostas
caracteristica
enfase
todavia
desenvolvido
muitas
oferecer
realizada
veremos
ensaios
entende
adequada
devido
chave
caracteriza
fundamentar
relevantes
essencialmente
olhar
destas
diagnostico
feito
leituras
chamado
construir
superar
mostramos
serie
verificar
pretensao
dimensoes
envolve
tornar
traz
chegar
diferente
exige
trajetoria
cenario
controle
escopo
oferece
alcance
fontes
postura
discute
necessariamente
determinada
filosofias
passo
precisa
breve
identificacao
deixa
logo
perceber
apresentados
falar
fundamenta
objecoes
particularmente
algum
dados
meios
primeiras
propomos
conexao
sustenta
abordagens
estabelecida
passando
acesso
conseguinte
nela
certos
conduz
entendido
versao
visando
possuem
tomada
defendida
mesmos
diz
pesquisas
desdobramentos
destaque
encontramos
justificar
pouco
daquilo
determinado
investigamos
pensada
pensamentos
procurando
encontrase
expoe
sustentar
concebe
anterior
atuais
centro
fazem
igualmente
leitor
reconstruir
bastante
podese
reconhecer
destacar
duracao
retomada
ultimos
assunto
compoem
depende
definir
descrever
relacionados
tracos
conhecido
contribuir
questionamento
vinculo
amplo
certas
compreendido
existir
fazendo
segue
comparacao
tentar
fez
fornecer
explorar
conceber
anteriores
chama
influencias
procurase
consigo
respectivamente
ocupa
toma
itinerario
permitem
corresponde
mantem
transformacoes
resumo
criar
etc
pensa
quarto
considerados
definicoes
demonstra
destino
capazes
seguindo
inclusive
apresentando
determina
elaborada
analisaremos
apresentam
existem
linha
nele
relevante
revisao
artigo
buscaremos
consideramos
curso
exigencia
agora
estaria
levar
descoberta
hipoteses
opera
realizado
tendencia
ampla
feitas
junto
metodologico
tenta
destacando
encontram
fundamentalmente
indica
partimos
posto
concebida
enfoque
vistas
pressupoe
resolver
resulta
serve
define
dias
dificuldade
relacionadas
secao
separacao
abre
desafio
ira
referencias
bibliografica
ademais
claro
dito
nenhuma
passagens
analisada
especificos
haver
simplesmente
tratados
trazer
debates
marca
mostraremos
atingir
garantir
indicar
mostrase
analisando
articula
inicia
profunda
utiliza
alternativas
caracterizar
dando
emergencia
examina
interpretar
utilizado
elabora
fazse
identifica
acreditamos
apresentaremos
baseado
completa
enfim
tempos
total
acima
concerne
concreta
cerne
denominada
interpretativa
nunca
promover
abordamos
constatacao
decorrer
estilo
etapas
areas
clara
colocar
contato
conteudos
entendemos
estabelecido
situar
tentamos
estadual
grupos
individuais
solucoes
configuracao
defendemos
obstante
parana
relaciona
desenvolvidas
talvez
analisados
conclui
conclusoes
conhecida
marcada
constituir
dupla
procuraremos
sequencia
basicos
constroi
discutimos
dita
duplo
metodologica
oeste
peculiar
propoese
referido
sugere
doutrinas
inerente
mera
possam
produz
denominado
panorama
pensado
ressaltar
titulo
toledo
aparentemente
continua
devemos
podera
produto
referida
comeca
configura
consideradas
contem
manter
notadamente
utilizada
cabe
etapa
explicita
exposta
levando
metade
pano
caracterizada
dividida
descreve
deu
intelectuais
mesmas
observar
efetivamente
produzir
acaba
aparecem
cinco
consegue
constituido
daquele
elaborar
empreendimento
envolvidos
problematizacao
der
exemplos
permanece
posteriores
tange
aberto
fruto
reconhece
dao
fornece
fundamentada
tratamos
versa
volta
insere
maxima
publicacao
querer
tomar
tornarse
utilizacao
apontando
posterior
utilizados
adequado
completamente
intento
quase
superior
absolutamente
analisase
centralidade
exclusivamente
formulada
acontece
analisado
area
assumir
concluimos
contextos
cunho
plena
suporte
analisadas
apresento
corrente
deixar
deveria
inspiracao
perda
posicionamento
surgem
tentativas
atuacao
denomina
envolvem
especificidade
relacionar
comentario
fica
acompanhar
dividido
perpassa
resgate
simultaneamente
concebido
determinadas
exigencias
marco
reside
buscouse
iremos
levam
marcado
meramente
reflete
remete
ultimas
cria
doutorado
parecem
abordado
desenvolveu
destaca
determinante
relacionada
totalmente
aberta
ali
concluir
propostos
saida
assuntos
complexa
daquela
lidar
poe
realmente
requer
criticamente
passos
passou
reformulacao
concluise
demonstrando
desenvolvidos
estritamente
ligada
permanente
vir
aplicada
argumentamos
buscou
criadora
determinados
levou
meta
pelotas
chamamos
faremos
limitacoes
maiores
poderiam
questionamentos
situa
atribui
definida
exatamente
explica
fazemos
federal
garantia
juntamente
levaram
tentaremos
vias
artigos
consistente
dedicado
estudiosos
senao
tracar
construida
cursos
especificas
lida
observacoes
percorrer
primazia
questionar
representar
abordada
comeco
construido
elaborado
formular
imprescindivel
chega
examinamos
plenamente
revelar
apreender
articular
comentarios
diferentemente
distinguir
mestre
publicado
abordados
basicamente
constituise
constitutivos
definido
ganha
intitulada
procurou
tornouse
aceitacao
ampliacao
chamar
distincoes
evitar
filosofar
tratam
usos
abordaremos
distancia
inclui
inves
proximo
temse
sertacao
difere
tido
inerentes
explicitando
indicando
notas
consolidacao
pesquisar
eio
pertencera
apresente
santamaria
dizrespeito
outrolado
tica
mira
sinalizar
trecho
dedicase
decorrem
analiso
trato
abordo
trabalha
realizou
aotureza
presentouse
isa
destacase
oes
inge
nuo
dedicou
estender
publicada
funda
trabalhar
otica
interprete
recurso
modo
camada
geralmente
compreendermos
voltada
chegamos
posgraduacao
aula
nco
significar
oficial
monografia
compara
pesquisadores
recentemente
desempenha
incluindo
acompanha
esforco
aes
disserta
explora
tornara
histaria
solu
sco
crotica
perptua
cioncia
rela
recorrendo
questaes
concep
sculo
reflexco
iii
rio
ncia
investigaremos
pensamos
referidos"


# Salvar vetor para uso em stm_filosofia.R####
readr::write_lines(filolixo, "dados/filolixo")
# Transformação em tibble para uso no anti_join####
filolixo <- tibble(word = unlist(str_split(filolixo, "\n")))
