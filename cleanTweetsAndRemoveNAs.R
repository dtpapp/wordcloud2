#++++++++++++++++++++++++++++++++++
# Remover NAs
cleanTweetsAndRemoveNAs<- function(Tweets) {
  
  TweetsCleaned = sapply(Tweets, cleanTweets)
  
  # Remove the "NA" tweets from this tweet list
  TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
  
  names(TweetsCleaned) = NULL
  # Remove the repetitive tweets from this tweet list
  
  TweetsCleaned = unique(TweetsCleaned)
  
  TweetsCleaned
}
#++++++++++++++++++++++++++++++++++