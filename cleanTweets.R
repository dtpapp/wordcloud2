#++++++++++++++++++++++++++++++++++
# Limpeza de caracteres especiais
cleanTweets<- function(tweet){
  
  # Clean the tweet for sentiment analysis
  
  # remove html links
  
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
  
  # Remove retweet entities
  
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
  
  # Remove all "#Hashtag"
  
  tweet = gsub("#\\w+", " ", tweet)
  
  # Remove all "@people"
  
  tweet = gsub("@\\w+", " ", tweet)
  
  # Remove all the punctuation
  
  tweet = gsub("[[:punct:]]", " ", tweet)
  
  # Remove numbers, we need only text for analytics
  
  tweet = gsub("[[:digit:]]", " ", tweet)
  
  # Remove unnecessary spaces (white spaces, tabs etc)
  tweet = gsub("[ \t]{2,}", " ", tweet)
  tweet = gsub("^\\s+|\\s+$", "", tweet)
  
  tweet = gsub('https://','',tweet) # removes https://
  tweet = gsub('http://','',tweet) # removes http://
  tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
  #like emoticons 
  tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
  tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
  tweet = gsub('\\d+', '', tweet) # removes numbers
  tweet=str_replace_all(tweet,"[^[:graph:]]", " ")
  #tweet=SnowballC::wordStem(tweet,language = "portuguese")
  
  
  #Convert all text to lowercase
  tweet = catch.error(tweet)
  
  return(tweet)
}
#++++++++++++++++++++++++++++++++++