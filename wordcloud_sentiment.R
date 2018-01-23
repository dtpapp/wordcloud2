#++++++++++++++++++++++++++++++++++
# rquery.wordcloud() versao inicial retirada:
# - http://www.sthda.com
#+++++++++++++++++++++++++++++++++++

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
#                |                display.brewer.all()                                                             |
#----------------|------------------------------------------------------------------------------------------------------------------------
# Funcao retorna | além da nuvem de palavras, retorna uma lista com dois objetos:                                  |    
#                |- $tdm = matriz de termos e                                                                      |
#                |- $freqTable = tabela de frequencia em ordem decrescente                                         |
#----------------'-------------------------------------------------------------------------------------------------'----------------------

#Funcao:
wordcloud_sentiment <- function(x, 
                             lang="portuguese", excludeWords=NULL,  colorPalette="Dark2",
                             min.freq=3, max.words=200,rm.accent=T,print=T,ngrams=0)
{ 
  #Temporario:
  text <- x
    
  #limpeza
  text=cleanTweets(text)
  
  #Remover acentos
  if(rm.accent==T){
    text=rm_accent(text)
  }
  
  # Carrega o arquivo de texto para um corpus:
  docs <- Corpus(DataframeSource(as.data.frame(text)))
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  
  # Remove stopwords for the language 
  docs <- tm_map(docs, removeWords, stopwords(lang))
  
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  # Remove your own stopwords
  if(!is.null(excludeWords)) 
    docs <- tm_map(docs, removeWords, excludeWords) 
  
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  
  #Se Ngram=True:
  if(ngrams!=0){
    Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngrams, max = ngrams))
    tdm = TermDocumentMatrix(docs,control = list(tokenize = Tokenizer))
  }
  
  #Criando matriz para retornar
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(words = names(v),freq=v)
  
  if(print==T){
    # check the color palette name 
    if(colorPalette!="sentiment"){
      if(!colorPalette %in% rownames(brewer.pal.info)){ 
        colors = colorPalette
        # Plot the word cloud
        set.seed(1234)
        wordcloud(d$words,d$freq, min.freq=min.freq, max.words=max.words,
                  random.order=FALSE, rot.per=0.35, 
                  use.r.layout=FALSE, colors=colors)
      }else{
        colors = brewer.pal(8, colorPalette)
        # Plot the word cloud
        set.seed(1234)
        wordcloud(d$words,d$freq, min.freq=min.freq, max.words=max.words,
                  random.order=FALSE, rot.per=0.35, 
                  use.r.layout=FALSE, colors=colors)
      }
      return(list(tdm=tdm, freqTable = d)) 
    }
    
    if(colorPalette=="sentiment"){
      sentiLex_lem_PT02 <- lexiconPT::sentiLex_lem_PT02
      
      #Selecionando as palavras (seus radicais) e sua polaridade
      dicionary=data.frame(cbind(sentiLex_lem_PT02$term,sentiLex_lem_PT02$polarity))
      matriz=d
      #Arrumando nome das bases de dados2: (Colocar nomes iguais para words)
      names(dicionary)=c("words", "sentiment")
      names(matriz)=c("words", "freq")
      
      #Transformando palavras em character:
      dicionary$words=as.character(dicionary$words)
      matriz$words=as.character(matriz$words)
      
      
      dicionary=dicionary[ dicionary$sentiment==1 | dicionary$sentiment==0 | dicionary$sentiment==-1, ]
      table(dicionary$sentiment)
      dicionary$sentiment=as.factor(dicionary$sentiment)
      #Alterando o nome dos sentimentos:
      levels(dicionary$sentiment)[levels(dicionary$sentiment)==-1]=c("Negativo")
      levels(dicionary$sentiment)[levels(dicionary$sentiment)==0]=c("Neutro")
      levels(dicionary$sentiment)[levels(dicionary$sentiment)==1]=c("Positivo")
      
      #Join das palavras do documento com o dicionario ntivo do R
      sentimentos=data.frame(matriz) %>%
        left_join(data.frame(dicionary),by="words") %>%
        select(words,sentiment,freq)%>%
        distinct(words,.keep_all = T)
      
      rownames(d)=d$words
      #Neutro para palavras fora do dicionario
      sentimentos$sentiment[is.na(sentimentos$sentiment)]="Neutro"
      
      #Criando coluna de cores para cada sentimento
      sentimentos$col=c(ifelse(sentimentos$sentiment=="Neutro","gray80",ifelse(sentimentos$sentiment=="Positivo","blue","red")))
      ##########################################
      # Plot the word cloud
      set.seed(1234)
      wordcloud(sentimentos$words,freq = sentimentos$freq, min.freq=min.freq, max.words=max.words,
                random.order=FALSE, rot.per=0.35, 
                use.r.layout=FALSE, colors = sentimentos$col,ordered.colors = T)
      return(list(tdm=tdm, freqTable = sentimentos))
    }
  }else{
    return(list(tdm=tdm, freqTable = d))}
  
}
