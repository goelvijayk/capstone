

##########################
#INITIALIZE

library(tm)
library(RWeka)
library(R.utils)
require(slam)

set.seed(2180)

initialize <- function() {
  setwd("~/Documents/Training/Capstone_Swiftkey/final/en_US")

  b<- readLines("en_US.blogs.txt")[rbinom(countLines("en_US.blogs.txt"),1,0.01)>0] #raw small sample to get started with
  t<- readLines("en_US.twitter.txt")[rbinom(countLines("en_US.twitter.txt"),1,0.01)>0] #raw small sample to get started with
  n<- readLines("en_US.news.txt")[rbinom(countLines("en_US.news.txt"),1,0.01)>0] #raw small sample to get started with


  symbolsRemove <<- c("$", "€", "£", ":", ",", "!","-","'", "‘", "’", "-", ".", "?", ")", "(", "&", "/","|", "<", ">", "_", "=", "+", "*", "%", "@", "!", "#") # symbols to be removed

  messages<<- "Few interesting facts -"

  gc()

  t2<- " "
  l <- as.integer(length(t)/100)
  for (j in 0:99) {
    for (i in 1:l) {
      t2[j+1]<- paste(t2[j+1], t[j*l+i])
    }
  }

  b2<- " "
  l <- as.integer(length(b)/100)
  for (j in 0:99) {
    for (i in 1:l) {
      b2[j+1]<- paste(b2[j+1], b[j*l+i])
    }
  }

  n2<- " "
  l <- as.integer(length(n)/100)
  for (j in 0:99) {
    for (i in 1:l) {
      n2[j+1]<- paste(n2[j+1], n[j*l+i])
    }
  }

  rm(b);
  rm(t);
  rm(n);
  gc()


  messages<<- rbind(messages, paste("Total lines in en_US.blogs.txt are - ",as.numeric(countLines("en_US.blogs.txt"))))
  messages<<- rbind(messages, paste("Total lines in en_US.news.txt are - ",as.numeric(countLines("en_US.news.txt"))))
  messages<<- rbind(messages, paste("Total lines in en_US.twitter.txt are - ",as.numeric(countLines("en_US.twitter.txt"))))
  messages<<- rbind(messages, paste("Length of longest line in en_US.blogs.txt is - ",max(nchar(readLines("en_US.blogs.txt")))))
  messages<<- rbind(messages, paste("Length of longest line in en_US.news.txt is - ",max(nchar(readLines("en_US.news.txt")))))
  messages<<- rbind(messages, paste("Length of longest line in en_US.twitter.txt is - ",max(nchar(readLines("en_US.twitter.txt")))))

  bc<<- Corpus(VectorSource(c(b2))) #corpus
  tc<<- Corpus(VectorSource(c(t2))) #corpus
  nc<<- Corpus(VectorSource(c(n2))) #corpus
  tdm <<- TermDocumentMatrix(tc)
  messages<<- rbind(messages, paste("Total words in en_US.twitter.txt sample are - ", sum(tdm)))
  messages<<- rbind(messages, paste("Total unique words in en_US.twitter.txt sample are - ", dim(tdm)[1]))
  tdm <<- TermDocumentMatrix(bc)
  messages<<- rbind(messages, paste("Total words in en_US.blogs.txt sample are - ", sum(tdm)))
  messages<<- rbind(messages, paste("Total unique words in en_US.blogs.txt sample are - ", dim(tdm)[1]))
  tdm <<- TermDocumentMatrix(nc)
  messages<<- rbind(messages, paste("Total words in en_US.news.txt sample are - ", sum(tdm)))
  messages<<- rbind(messages, paste("Total unique words in en_US.news.txt sample are - ", dim(tdm)[1]))

  gc()

  #make lists of profane words
  #http://www.frontgatemedia.com/a-list-of-723-bad-words-to-blacklist-and-how-to-use-facebooks-moderation-tool/
  setwd("..")
  setwd("..")
  profaneWords2 <- read.csv("Terms-to-Block.csv")
  profaneWords2<<- profaneWords2[2]
  p<- as.character(profaneWords2[,1])
  profaneWords2<<- gsub(",", "", p)

  clean<<- Corpus(VectorSource(c(t2,b2,n2))) #corpus

}



#count of words removed during data cleaning: for reporting
wordsRemoved <- function(x) {
  tdm <- TermDocumentMatrix(x)
  r<- runningCount
  runningCount <<- dim(tdm)[1]
  lost <- r-runningCount
  return(lost)
}

#http://www.unt.edu/rss/class/Jon/R_SC/Module12/BasicTextMining.R
cleanTokenizeCorpus <- function(clean) {

  totalEntriesStart <<- length(clean) #reference point for # of entries in beginning

  tdm <<- TermDocumentMatrix(clean)
  runningCount <<- dim(tdm)[1] #running count of words
  messages<<- rbind(messages,paste("Total unique words - ", runningCount))

  clean <- tm_map(clean, tolower)
  messages<<- rbind(messages,paste("Unique words removed by changing to lower case - ", wordsRemoved(clean)))

  x <- mapply(FUN= function(...) {
    clean <<- gsub(...,x=clean, fixed=TRUE)},
    pattern=symbolsRemove, replacement="")
  rm(x) # discard x; it's empty
  clean<- Corpus(VectorSource(clean))
  messages<<- rbind(messages,paste("Unique words removed due to symbols - ",wordsRemoved(clean)))
  #clean <- tm_map(clean, stemDocument)

  clean <- tm_map(clean, removeWords, profaneWords2)
  messages<<- rbind(messages,paste("Unique words removed due to profanity - ", wordsRemoved(clean)))

  clean <- tm_map(clean, removePunctuation)
  messages<<- rbind(messages,paste("Unique words removed by removing punctuation - ",wordsRemoved(clean)))

  clean <- tm_map(clean, removeNumbers)
  messages<<- rbind(messages,paste("Unique words removed by removing numbers - ", wordsRemoved(clean)))

  clean <- tm_map(clean, removeWords, c(stopwords("english")))
  messages<<- rbind(messages,paste("Unique stopwords removed - ",wordsRemoved(clean)))

  # Remove entries with non-ASCII characters - entire blog entry from corpus is filtered out, if any non-ASCII letter is found in entire document. This is the proxy used for foreign language.
  # find indices of words with non-ASCII characters
  #dat2<- clean
  #dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
  # subset original vector of words to exclude words with non-ASCII char
  #clean <- dat2[-dat3]
  #messages<<- rbind(messages,paste("Entries removed due to special characters - ", length(dat3)))
  #rm(dat2)
  #rm(dat3)
  gc()
  messages<<- rbind(messages,paste("Unique words removed due to special character entries - ",wordsRemoved(clean)))

  clean <- tm_map(clean, stripWhitespace)

  gc()
  tdm <<- TermDocumentMatrix(clean)

  UnigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 1, max = 1))}
  x<- UnigramTokenizer(clean)
  allWords1<<- sort(table(x), descending=TRUE)

  BigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 2, max = 2))}
  x<- BigramTokenizer(clean)
  allWords2<<- sort(table(x), descending=TRUE)

  TrigramTokenizer <- function(x) {RWeka::NGramTokenizer(x, RWeka::Weka_control(min = 3, max = 3))}
  x<- TrigramTokenizer(clean)
  allWords3<<- sort(table(x), descending=TRUE)

  allWords<<- list(allWords1, allWords2, allWords3)
  return(clean)

}

#maxWords = # of words to show in word cloud. nrgam = 1,2,3
makeCloud<- function(maxWords, ngram) {
  library(wordcloud)
  require(RColorBrewer)
  pal <- brewer.pal(9,"RdYlGn")
  pal <- pal[-(4:6)]
  x<- sort(allWords[[ngram]], decreasing = TRUE)[1:maxWords]
  print(
    wordcloud(
      names(x),
      as.numeric(x),
      #scale = c(4,0.5),
      min.freq=1,
      max.words=maxWords,
      random.order = FALSE,
      colors=pal
    )
  )
}


