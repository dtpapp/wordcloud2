#++++++++++++++++++++++++++++++++++++++++++++++++
# Pacotes 
#++++++++++++++++++++++++++++++++++++++++++++++++

#Instalacao  (necessario rodar apenas uma vez) :
install.packages("stringr")      #Pacote para manipulação de strings
install.packages("dplyr")        #Pacote para manipulação de dados
install.packages("tm")           #Pacote de para text mining
install.packages("wordcloud")    #Pacote para nuvem de palavras
install.packages("readxl")       #Pacote para leitura de dados excel
install.packages("readr")        #Pacote para leitura de dados
install.packages("tidytext")     #Manipulação de textos
install.packages("reshape2")     #Manipulação de dados
install.packages("lexiconPT")    #Importar palavras de sentimentos
install.packages("SnowballC")    #Para identificar os radicais
install.packages("RColorBrewer") #Palleta de cores
install.packages("rJava")        #Conectar com Weka
install.packages("RWeka")        #Pra executar as N-grams (n palavras que aparecem juntas com frequencia)
install.packages("ggplot2")      #Pra criar figuras

#Carregando os pacotes:
suppressMessages(library(stringr))         #Pacote para manipulação de strings
suppressMessages(library(dplyr))           #Pacote para manipulação de dados
suppressMessages(require(tm))              #Pacote de para text mining
suppressMessages(require(wordcloud))       #Pacote para nuvem de palavras
suppressMessages(require(readxl))          #Pacote para leitura de dados excel
suppressMessages(library(tidytext))        #Manipulação de textos
suppressMessages(library(reshape2))        #Manipulação de dados
suppressMessages(library(lexiconPT))       #Importar palavras de sentimentos
suppressMessages(library(SnowballC))       #Para identificar os radicais
suppressMessages(library(RColorBrewer))    #Palleta de cores
suppressMessages(library(rJava))           #Conectar com Weka
suppressMessages(library(RWeka))           #Pra executar as N-grams (n palavras que aparecem juntas com frequencia)
suppressMessages(library(ggplot2))         #Pra criar figuras
suppressMessages(library(readr))           #Pacote para leitura dos dados

#++++++++++++++++++++++++++++++++++++++++++++++++
# Funcoes 
#++++++++++++++++++++++++++++++++++++++++++++++++

#Primeiramente selecione o diretorio de trabalho (note que o caminho deve ser expecificado com a barra "/"):
setwd("~/Projeto Estagio/Linhas de codigo passo a passo")

#Obs: com arquivos com essas funcoes precisam estar na mesmo diretorio de trabalho deste arquivo
source("catch_error.R")                           #Captacao de erros
source("cleanTweets.R")                           #Limpeza de caracteres especiais de redes sociais (como "@nome", "https://(...)", "...")
source("cleanTweetsAndRemoveNAs.R")               #Remove linhas em branco
rm_accent <- function(str,pattern="all") {
  # Rotinas e funções úteis V 1.0
  # rm.accent - REMOVE ACENTOS DE PALAVRAS
  # Função que tira todos os acentos e pontuações de um vetor de strings.
  # Parâmetros:
  # str - vetor de strings que terão seus acentos retirados.
  # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
  #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
  #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
  #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
  if(!is.character(str))
    str <- as.character(str)
  
  pattern <- unique(pattern)
  
  if(any(pattern=="Ç"))
    pattern[pattern=="Ç"] <- "ç"
  
  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
  )
  
  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )
  
  accentTypes <- c("´","`","^","~","¨","ç")
  
  if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
    return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
  
  for(i in which(accentTypes%in%pattern))
    str <- chartr(symbols[i],nudeSymbols[i], str)
  
  return(str)
}    #Remover acentos
source("wordcloud_sentiment.R")                   #Funcao para gerar nuvem de palavras, explicacao logo abaixo
source("frequencia.R")                            #Funcao para tabela de frequencias 


#++++++++++++++++++++++++++++++++++++++++++++++++
# Leitura e breve tratamento da base de dados
#++++++++++++++++++++++++++++++++++++++++++++++++

#Leitura dos dados:
dados <- read_delim("social-searcher(23) meu inss.csv",          #Nome do arquivo que sera lido
                    ";", escape_double = FALSE, trim_ws = TRUE)  #Para importar outros arquivos, utilize o caminho: "File>Import Dataset>From (...)"

#removendo as linhas em branco:
dados=dados%>%
  filter(!is.na(text))

#Criando a variavel "x" que contem a coluna com os textos que deseja-se avaliar:
x=dados$text

#Removendo linhas duplicadas:
x=unique(x)

#++++++++++++++++++++++++++++++++++++++++++++++++
# Criando nuvens de palavra
#++++++++++++++++++++++++++++++++++++++++++++++++

# Como utilizar a funcao wordcloud_sentiment():

#----------------.-------------------------------------------------------------------------------------------------.----------------------
#      Argumento | Definicao                                                                                       | Default
#----------------|------------------------------------------------------------------------------------------------------------------------
#              x |  Coluna com textos (cada linha representa um comentario)                                        |
#----------------|------------------------------------------------------------------------------------------------------------------------
#            lang| Lingua utilizada na nuvem (para remover as stopwords em portugues)                              |(Default = "portuguese")
#----------------|------------------------------------------------------------------------------------------------------------------------
#   excludeWords | Vetor de stopwords adicionais para serem removidas                                              |(Default=NULL) ou seja, sem palavras adicionais por padrao
#----------------|------------------------------------------------------------------------------------------------------------------------
#       min.freq | Palavras com frequência abaixo de min.freq não serão plotadas                                   |
#----------------|------------------------------------------------------------------------------------------------------------------------
#      max.words | Número máximo de palavras a serem plotadas. Menos termos freqüentes caíram                      |
#----------------|------------------------------------------------------------------------------------------------------------------------
#      rm.accent | Controla de os acentos serao removidos:                                                         |(Default = TRUE)
#                |- TRUE para remover acentos,                                                                     |     
#                |- FALSE caso nao queira remover acentos (obs: de preferencia para remover acentos)               |
#----------------|------------------------------------------------------------------------------------------------------------------------
#          print | Controla de a nuvem de palavras sera exibida:                                                   |(Default = TRUE)
#                |- TRUE para plotar a nuvem de palavras,                                                          |
#                |- FALSE para obter apenas a matriz de termos e a tabela de frequencias                           |
#----------------|------------------------------------------------------------------------------------------------------------------------
#         ngrams | Controla a frequencia sequencias de n palavras :                                                |(Default = 0)
#                |- 2: sequencias de duas palavras                                                                 |
#                |- 3: sequencias de tres palavras                                                                 |
#                |- n: sequencias de n palavras                                                                    |
#----------------|------------------------------------------------------------------------------------------------------------------------
#   colorPalette | Este argumento define duas opcoes de como as palavras serao coloridas:                          |(Default = "Dark2") 
#                |- "sentiment" : cada palavra é pintada de azul (positivo), vermelho (negativo) ou cinza (neutro) |
#                |- "Dark2"     : exemplo de palleta de cores, para mais opcoes consultar rodar o codigo abaixo:   |
#                |                display.brewer.all()  (copiar e colar no console)                                |
#----------------|------------------------------------------------------------------------------------------------------------------------
# Funcao retorna | além da nuvem de palavras, retorna uma lista com dois objetos:                                  |    
#                |- $tdm = matriz de termos e                                                                      |
#                |- $freqTable = tabela de frequencia em ordem decrescente                                         |
#----------------'-------------------------------------------------------------------------------------------------'----------------------


# Entendendo a funcao -----------------------------------------------------

#Considere a segunte nuvem de palavras:
nuvem_exemplo=wordcloud_sentiment(x)  #note que nenhuma opcao da funcao foi selecionada, pois a finalidade eh entender os objetos retornados

#Alem da nuvem de palavras gerada na area de plot, note também é gerada a tabela dos termos mais frequentes, veja:
nuvem_exemplo$freqTable
#Com mais detalhes (a tabela com todos os termos sera aberta em uma nova guia):
View(nuvem_exemplo$freqTable)


#++++++++++++++++++++++++++++++++++++++++++++++++
# Exemplos pratico
#++++++++++++++++++++++++++++++++++++++++++++++++

#Determine as Stopwords adicionais (Palavras que deseja remover da analise):
stopwords_adicionais=c("pro","nao", "pra", "ano", "anos", "vai", "vamos","faz","sit","bom dia","boa tarde", "boa noite", "por favor", "favor", "por","jeito", "fazer", "faz", "fiz", "aparece", "apareceu", "fica", "nada", "ver", "ter", "pois", "diz", "vou", "assim", "ainda", "opcao", "pede", "ficar", "fica", "dando", "sendo", "toda", "todas", "vezes", "todo", "todos", "faco", "faço", "hora", "outra", "outras", "outro", "outros","podem", "pode", "coisa", "dar", "varias", "tudo", "mudar", "opcoes", "porque", "por que", "voce", "vc", "vcs", "deveria", "ser", "tal", "ficam", "mim", "porem")


# 1 -----------------------------------------------------------------------
#Nuvem de frequencia de palavras removendo as stopwords acima, colorindo as palavras de acordo com o sentimento, de palavras que apareceram no minimo 1 vez, permitindo um total de 70 palavras na nuvem

wordcloud1=wordcloud_sentiment(x,                                    #variavel "x" que contem a coluna com os textos
                               excludeWords=stopwords_adicionais,    #stopwords adicionais definidas acima
                               colorPalette="sentiment",             #as cores corresponderao ao sentimento de cada palavra
                               min.freq=1,                           #palavras que aparecem pelo menos 1 vez
                               max.words=70                          #maximo de 70 palavras na nuvem
)

View(wordcloud1$freqTable)

#Para tabela dos 20 termos mais frequentes:
frequencia(wordcloud1$freqTable,n=20) #Eh ossivel alterar o numero de palavras


# 2 -----------------------------------------------------------------------

#Nuvem de frequencia de sequencias de 2 palavras removendo as stopwords acima, colorindo as palavras de acordo com o sentimento, de palavras que apareceram no minimo 1 vez, permitindo um total de 70 palavras na nuvem

wordcloud2=wordcloud_sentiment(x,                                    #variavel "x" que contem a coluna com os textos
                               excludeWords=stopwords_adicionais,    #stopwords adicionais definidas acima
#                              colorPalette="sentiment",             #neste caso o sentimentos das palavras nao sera identificado
                               min.freq=1,                           #palavras que aparecem pelo menos 1 vez
                               max.words=70,                         #maximo de 70 palavras na nuvem
                               ngrams = 2                            #sequencias de duas palavras
)

View(wordcloud2$freqTable)

# Para tabela dos 20 termos mais frequentes:
frequencia(wordcloud2$freqTable,n=20)

# 3 -----------------------------------------------------------------------

#Nuvem de frequencia de sequencias de 2 palavras removendo as stopwords acima, colorindo as palavras de acordo com o sentimento, de palavras que apareceram no minimo 1 vez, permitindo um total de 70 palavras na nuvem

wordcloud3=wordcloud_sentiment(x,                                    #variavel "x" que contem a coluna com os textos
                               excludeWords=stopwords_adicionais,    #stopwords adicionais definidas acima
#                              colorPalette="sentiment",             #neste caso o sentimentos das palavras nao sera identificado
                               min.freq=1,                           #palavras que aparecem pelo menos 1 vez
                               max.words=70,                         #maximo de 70 palavras na nuvem
                               ngrams = 3                            #sequencias de duas palavras
)

View(wordcloud3$freqTable)

# Para tabela dos 20 termos mais frequentes:
frequencia(wordcloud3$freqTable,n=20)