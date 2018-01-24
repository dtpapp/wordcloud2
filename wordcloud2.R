#Carregando os pacotes:
suppressMessages(library(stringr))         #Pacote para manipulação de strings
suppressMessages(library(dplyr))           #Pacote para manipulação de dados
suppressMessages(require(tm))              #Pacote de para text mining
suppressMessages(require(wordcloud))       #Pacote para nuvem de palavras
suppressMessages(require(wordcloud2))       #Pacote para nuvem de palavras
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
dados <- read_csv("base.csv")

#removendo as linhas em branco:
dados=dados%>%
  filter(!is.na(text))

#Criando a variavel "x" que contem a coluna com os textos que deseja-se avaliar:
x=dados[,2]

#Removendo linhas duplicadas:
x=unique(x)

x=base[,2]

#Temporario:
text <- x

#limpeza
text=cleanTweets(text)

#Remover acentos

text=rm_accent(text)


# Carrega o arquivo de texto para um corpus:
docs <- Corpus(DataframeSource(as.data.frame(text)))

# Convert the text to lower case
docs <- tm_map(docs, content_transformer(tolower))

# Remove numbers
docs <- tm_map(docs, removeNumbers)

lang="portuguese"
# Remove stopwords for the language 
docs <- tm_map(docs, removeWords, stopwords(lang))

# Remove punctuations
docs <- tm_map(docs, removePunctuation)

# Eliminate extra white spaces
docs <- tm_map(docs, stripWhitespace)

# # Remove your own stopwords
# if(!is.null(excludeWords)) 
#   docs <- tm_map(docs, removeWords, excludeWords) 

# Create term-document matrix
tdm <- TermDocumentMatrix(docs)

m <- as.matrix(tdm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(words = names(v),freq=v)


nuvem_exemplo$freqTable
data=data$freqTable

#++++++++++++++++++++++++++++++++++
#Nuvem com o formato do logo
#+++++++++++++++++++++++++++++++++++

figPath = system.file("examples/t.png",package = "wordcloud2")
wordcloud2(data, figPath = figPath, size =1)
