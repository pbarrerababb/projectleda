filetext <- ("/users/patriciobarrera/desktop/birdman_2014.txt")

text <- readLines(filetext)

library(tm)
library(wordcloud)
library(RColorBrewer)

charquery <- function(charname){

  
      charquest <- paste("^",charname, sep = "")
  
      Riggtxt <- grepl(charquest, text)

      finaltxt<- tolower( text[Riggtxt])
  
      corporo <- Corpus(VectorSource(finaltxt))
  
      corporo <- tm_map(corporo,content_transformer(
        function(x) gsub(tolower(charname),"",x)
      ))

      corporo <- tm_map(corporo,content_transformer(
        function(x) gsub("\\(.+\\)","",x)
        ))

      daamn <- TermDocumentMatrix(corporo,
                                  control = list(removePunctuation = TRUE, 
                                                stopwords = c(stopwords(), charname),
                                                removeNumbers = TRUE ))

      monono =  as.matrix(daamn)

      word_freqs = sort(rowSums(monono), decreasing = T)

      dm = data.frame(word=names(word_freqs), freq = word_freqs)

      wordcloud(dm$word,dm$freq,c(5,.3),10,500,FALSE,.1)

}
