frequencia=function(x,n){
  head(x,n=n)%>%
    ggplot(aes(reorder(words,freq), freq)) +
    geom_bar(stat = "identity", color = "black", fill = "#87CEFA") +
    scale_y_continuous(breaks=seq(0,max(x$freq),round((max(x$freq)-min(x$freq))/10,0)))+
    coord_flip() +
    labs(title = "Palavras mais mencionadas",  x = "Palavras", y = "Número de usos")+theme_minimal()
}

  