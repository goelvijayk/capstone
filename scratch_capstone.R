# Capstone project scratchpad

##########################
#INITIALIZE

library(tm)
library(RWeka)
library(RWekajars)
library(R.utils)

initialize <- function() {
  setwd("~/Documents/Training/Capstone_Swiftkey/final/en_US")

  con <- file("en_US.blogs.txt", "r")
  b<- readLines(con, 1000) #raw small sample to get started with
  close(con)

  con <- file("en_US.twitter.txt", "r")
  t<- readLines(con, 1000) #raw small sample to get started with
  close(con)

  con <- file("en_US.news.txt", "r")
  n<- readLines(con, 1000) #raw small sample to get started with
  close(con)

  symbolsRemove <<- c("$", "€", "£", ":", ",", "!","-","'", "‘", "’", "-", ".", "?", ")", "(", "&", "/","|", "<", ">", "_", "=", "+", "*", "%", "@", "!", "#") # symbols to be removed

  messages<<- "For display"
  messages<<- rbind(messages, paste("Total lines in en_US.blogs.txt are - ",as.numeric(countLines("en_US.blogs.txt"))))
  messages<<- rbind(messages, paste("Total lines in en_US.news.txt are - ",as.numeric(countLines("en_US.news.txt"))))
  messages<<- rbind(messages, paste("Total lines in en_US.twitter.txt are - ",as.numeric(countLines("en_US.twitter.txt"))))

  bc<<- Corpus(VectorSource(c(b))) #corpus
  tc<<- Corpus(VectorSource(c(t))) #corpus
  nc<<- Corpus(VectorSource(c(n))) #corpus
  tdm <<- TermDocumentMatrix(tc)
  messages<<- rbind(messages, paste("Total words in en_US.twitter.txt sample are - ", sum(tdm)))
  messages<<- rbind(messages, paste("Total unique words in en_US.twitter.txt sample are - ", dim(tdm)[1]))
  tdm <<- TermDocumentMatrix(bc)
  messages<<- rbind(messages, paste("Total words in en_US.blogs.txt sample are - ", sum(tdm)))
  messages<<- rbind(messages, paste("Total unique words in en_US.blogs.txt sample are - ", dim(tdm)[1]))
  tdm <<- TermDocumentMatrix(nc)
  messages<<- rbind(messages, paste("Total words in en_US.news.txt sample are - ", sum(tdm)))
  messages<<- rbind(messages, paste("Total unique words in en_US.news.txt sample are - ", dim(tdm)[1]))

  rm(bc); rm(tc); rm(nc); rm(tdm);
  gc()

  clean<<- Corpus(VectorSource(c(t,b,n))) #corpus
  rm(b); rm(t); rm(n);
  gc()

  #make lists of profane words
  #http://www.bannedwordlist.com/lists/swearWords.csv
  #http://www.frontgatemedia.com/a-list-of-723-bad-words-to-blacklist-and-how-to-use-facebooks-moderation-tool/
  setwd("..")
  setwd("..")
  profaneWords1 <- read.csv("swearWords.csv")
  profaneWords1<<- names(profaneWords1)
  profaneWords2 <- read.csv("Terms-to-Block.csv")
  profaneWords2<<- profaneWords2[2]
  p<- as.character(profaneWords2[,1])
  profaneWords2<<- gsub(",", "", p)

}

#count of words removed during data cleaning: for reporting
wordsRemoved <- function(x) {
  tdm <- TermDocumentMatrix(x)
  r<- runningCount
  runningCount <<- dim(tdm)[1]
  lost <- r-runningCount
  return(lost)
}

#function to replace symbols inside words
gsub2<- function(myPattern, myCorpus){
  for (i in 1:length(myCorpus)){
    for (j in 1:length(myPattern)){
      myCorpus[[i]]<- gsub(myPattern[j],"", myCorpus[[i]], fixed=TRUE)
    }
  }
  return(myCorpus)
}


#rm(c)
#http://www.unt.edu/rss/class/Jon/R_SC/Module12/BasicTextMining.R
cleanTokenizeCorpus <- function(clean) {

  totalEntriesStart <<- length(clean) #reference point for # of entries in beginning

  tdm <<- TermDocumentMatrix(clean)
  runningCount <<- dim(tdm)[1] #running count of words
  messages<<- rbind(messages,paste("Total unique words - ", runningCount))

  clean <- tm_map(clean, tolower)
  messages<<- rbind(messages,paste("Unique words removed by changing to lower case - ", wordsRemoved(clean)))
  #inspect(tdm[1:10,1])

  clean <- tm_map(clean, removeWords, profaneWords1)
  clean <- tm_map(clean, removeWords, profaneWords2)
  messages<<- rbind(messages,paste("Unique words removed due to profanity - ", wordsRemoved(clean)))

  clean <- tm_map(clean, removePunctuation)
  #clean[[20]]
  messages<<- rbind(messages,paste("Unique words removed by removing punctuation - ",wordsRemoved(clean)))
  #inspect(tdm[1:10,1])

  clean <- tm_map(clean, removeNumbers)
  messages<<- rbind(messages,paste("Unique words removed by removing numbers - ", wordsRemoved(clean)))
  #inspect(tdm[1:10,1])

  clean <<- tm_map(clean, removeWords, c(stopwords("english")))
  messages<<- rbind(messages,paste("Unique stopwords removed - ",wordsRemoved(clean)))
  #inspect(tdm[1:20,1])

  x <- mapply(FUN= function(...) {
    clean <<- gsub(...,x=clean, fixed=TRUE)},
    pattern=symbolsRemove, replacement="")
  rm(x) # discard x; it's empty
  clean<- Corpus(VectorSource(clean))
  messages<<- rbind(messages,paste("Unique words removed due to symbols - ",wordsRemoved(clean)))
  #clean <- tm_map(clean, stemDocument)

  # Remove entries with non-ASCII characters - entire blog entry from corpus is filtered out, if any non-ASCII letter is found in entire document. This is the proxy used for foreign language.
  # find indices of words with non-ASCII characters
  dat2<- clean
  dat3 <- grep("dat2", iconv(dat2, "latin1", "ASCII", sub="dat2"))
  # subset original vector of words to exclude words with non-ASCII char
  clean <- dat2[-dat3]
  messages<<- rbind(messages,paste("Entries removed foreign language or special characters - ", length(dat3)))
  rm(dat2)
  rm(dat3)
  gc()
  messages<<- rbind(messages,paste("Unique words removed due to these entries - ",wordsRemoved(clean)))

  clean <- tm_map(clean, stripWhitespace)

  #tdm <- TermDocumentMatrix(clean)
  #findFreqTerms(x = tdm, lowfreq = 1, highfreq = 1)[1:20]

  #inspect(tdm[1:5,1])
  return(clean)
}




#inspect(tdm[,1:2])
#findFreqTerms(x = tdm, lowfreq = 8, highfreq = Inf)
#findAssocs(x = tdm, term = "away", corlimit = 0.2)
#tdm.common.60 <- removeSparseTerms(x = tdm, sparse = 0.60) #higher sparse = more terms retained
#rm(tdm.common.60)

