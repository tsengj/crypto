## ----author info, include=F----------------------------------------------
## Author:  Yanchang Zhao
## Email:   yanchang@RDataMining.com
## Website: http://www.RDataMining.com
## Date:    26 May 2017

## ----load libraries, include=F, echo=F-----------------------------------
## load required packages
library(magrittr) ## for pipe operations
library(twitteR)
library(tm)
library(ggplot2)
library(graph)
library(Rgraphviz)
library(RColorBrewer)
library(wordcloud)
#library(fpc)
library(topicmodels)
library(data.table) # month(), asIDate()
library(sentiment)

## ----term weighting, eval=F, tidy=F--------------------------------------
# library(magrittr)
# library(tm) ## package for text mining
# a <- c("I like R", "I like Python")
# ## build corpus
# b <- a %>% VectorSource() %>% Corpus()
# ## build term document matrix
# m <- b %>% TermDocumentMatrix(control=list(wordLengths=c(1, Inf)))
# m %>% inspect()
# ## various term weighting schemes
# m %>% weightBin() %>% inspect() ## binary weighting
# m %>% weightTf() %>% inspect() ## term frequency
# m %>% weightTfIdf(normalize=F) %>% inspect() ## TF-IDF
# m %>% weightTfIdf(normalize=T) %>% inspect() ## normalized TF-IDF

## ----retrieve-tweets, eval=F---------------------------------------------
## ## Option 1: retrieve tweets from Twitter
## library(twitteR)
## library(ROAuth)
## ## Twitter authentication
## setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
## ## 3200 is the maximum to retrieve
## tweets <- "RDataMining" %>% userTimeline(n=3200)

## ----download-tweets, eval=F---------------------------------------------
## ## Option 2: download @RDataMining tweets from RDataMining.com
## library(twitteR)
## url <- "http://www.rdatamining.com/data/RDataMining-Tweets-20160212.rds"
## download.file(url, destfile="./data/RDataMining-Tweets-20160212.rds")
## ## load tweets into R
## tweets <- readRDS("./data/RDataMining-Tweets-20160212.rds")

## ----load-tweets, echo=F-------------------------------------------------
library(twitteR)
## load tweets into R
library(twitteR)

#authenticate using your login token
consumerKey <- "x"
consumerSecret <- "x"
accessToken <- "x"
accessTokenSecret <- "x"
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

##Retrieve tweets from Twitter
searchstring <- hashtags <- c("#ICX",'#cryptocurrency') #
searchstring <- paste(hashtags, collapse = " AND ")
tweets <- searchTwitter(searchstring, n = 3200) #since="2017-03-01", until="2017-04-07" , resultType = "popular"
rm(hashtags,searchstring)

## ----print-tweets, tidy=F------------------------------------------------
(n.tweet <- tweets %>% length())
# convert tweets to a data frame
tweets.df <- tweets %>% twListToDF()
# tweet #1
tweets.df[1, c("id", "created", "screenName", "replyToSN",
               "favoriteCount", "retweetCount", "longitude", "latitude", "text")]
# print tweet #1 and make text fit for slide width
tweets.df$text[1] %>% strwrap(60) %>% writeLines()

## ----text cleaning functions, tidy=F-------------------------------------
#remove all non graphical characters
tweets.df$text=str_replace_all(tweets.df$text,"[^[:graph:]]", " ")

# function for removing URLs, i.e.,
#  "http" followed by any non-space letters
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
# function for removing anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
# customize stop words
#myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
#                 "use", "see", "used", "via", "amp")
myStopwords <- c(stopwords('english'),"rt",'crypto','cryptocurrency','cryptocurrencurrency')

## ----prepare-text, tidy=F------------------------------------------------
library(tm)

# build a corpus and specify the source to be character vectors
corpus.raw <- tweets.df$text %>% VectorSource() %>% Corpus()

# text cleaning
corpus.cleaned <- corpus.raw %>% 
  # convert to lower case
  tm_map(content_transformer(tolower)) %>% 
  # remove URLs
  tm_map(content_transformer(removeURL)) %>% 
  # remove numbers and punctuations
  tm_map(content_transformer(removeNumPunct)) %>% 
  # remove stopwords
  tm_map(removeWords, myStopwords) %>% 
  # remove extra whitespace
  tm_map(stripWhitespace) 

## ----stemming and stem-completion, tidy=F--------------------------------
## stem words
corpus.stemmed <- corpus.cleaned %>% tm_map(stemDocument)

## stem completion
stemCompletion2 <- function(x, dictionary) {
  x <- unlist(strsplit(as.character(x), " "))
  x <- x[x != ""]
  x <- stemCompletion(x, dictionary=dictionary)
  x <- paste(x, sep="", collapse=" ")
  stripWhitespace(x)
}

corpus.completed <- corpus.stemmed %>% 
  lapply(stemCompletion2, dictionary=corpus.cleaned) %>% 
  VectorSource() %>% Corpus()

## ----before/after text cleaning, tidy=F----------------------------------
# original text
corpus.raw[[1]]$content %>% strwrap(60) %>% writeLines()
# after basic cleaning
corpus.cleaned[[1]]$content %>% strwrap(60) %>% writeLines()
# stemmed text
corpus.stemmed[[1]]$content %>% strwrap(60) %>% writeLines()
# after stem completion
corpus.completed[[1]]$content %>% strwrap(60) %>% writeLines()

## ----fix-mining, tidy=F--------------------------------------------------
# count word frequence
wordFreq <- function(corpus, word) {
  results <- lapply(corpus,
                    function(x) grep(as.character(x), pattern=paste0("\\<",word)) )
  sum(unlist(results))
}
#n.miner <- corpus.cleaned %>% wordFreq("miner")
#n.mining <- corpus.cleaned %>% wordFreq("mining")
#cat(n.miner, n.mining)
# replace old word with new word
replaceWord <- function(corpus, oldword, newword) {
  tm_map(corpus, content_transformer(gsub),
         pattern=oldword, replacement=newword)
}
corpus.completed <- corpus.completed %>% 
  replaceWord("crypto", "cryptocurren") %>%
  replaceWord("btc", "bitcoin") %>% 
  replaceWord("cryptocurrencurren", "cryptocurren") %>%
  replaceWord("icon", "icx") 

#  replaceWord("universidad", "university") %>% 
#  replaceWord("scienc", "science")

## ----term-doc-matrix, tidy=F---------------------------------------------
tdm <- corpus.completed %>% 
  TermDocumentMatrix(control = list(wordLengths = c(1, Inf)))  %>% 
  print
#idx <- which(dimnames(tdm)$Terms %in% c("r", "data", "mining"))
#tdm[idx, 21:30] %>% as.matrix()

## ----frequent-terms, out.truncate=70-------------------------------------
# inspect frequent words
freq.terms <- tdm %>% findFreqTerms(lowfreq=20) %>% print
term.freq <- tdm %>% as.matrix() %>% rowSums()
term.freq <- term.freq %>% subset(term.freq>=20)
df <- data.frame(term=names(term.freq), freq=term.freq)

## ----plot-frequent-terms, tidy=F, fig.align="center", fig.width=4, fig.height=3, out.height=".8\\textheight"----
library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat="identity") +
  xlab("Terms") + ylab("Count") + coord_flip() +
  theme(axis.text=element_text(size=7))

## ----wordcloud1----------------------------------------------------------
m <- tdm %>% as.matrix
# calculate the frequency of words and sort it by frequency
word.freq <- m %>% rowSums() %>% sort(decreasing=T)

# colors
library(RColorBrewer)
pal <- brewer.pal(9, "BuGn")[-(1:4)]

## ----eval=F--------------------------------------------------------------
## # plot word cloud
## library(wordcloud)
## wordcloud(words=names(word.freq), freq=word.freq, min.freq=3, random.order=F, colors=pal)

## ----wordcloud2, fig.width=8, out.width="0.9\\textwidth", crop=T, echo=F----
wordcloud(words=names(word.freq), freq=word.freq, min.freq=3, random.order=F, colors=pal)

## ----association---------------------------------------------------------
# which words are associated with "r"?
tdm %>% findAssocs('icx', 0.15)
# which words are associated with "data"?
tdm %>% findAssocs('data', 0.2)

## ----network, fig.width=12, out.width="1.05\\textwidth", out.height="0.6\\textwidth", crop=T----
library(graph)
library(Rgraphviz)
plot(tdm, term = freq.terms, corThreshold = 0.13, weighting = T,
     attrs=list(node=list(label="foo", 
                          fillcolor="lightgreen",
                          fontsize=100,
                          height=1.8,
                          width=1.8),
                edge=list(color="pink",width="0.4")))

#Export image to disk
png(filename="./plot/icx_assoc.png",
    height=1080, 
    width=1920,
    units="px"
)
plot(tdm, term = freq.terms, corThreshold = 0.15, weighting = T,
     attrs=list(node=list(label="foo", 
                          fillcolor="lightgreen",
                          fontsize=50,
                          height=1.8,
                          width=1.8),
                edge=list(color="pink",width="0.4")))
dev.off()
## ----clustering----------------------------------------------------------
# remove sparse terms
m2 <- tdm %>% removeSparseTerms(sparse=0.95) %>% as.matrix()

# calculate distance matrix
dist.matrix <- m2 %>% scale() %>% dist()

# hierarchical clustering
fit <- dist.matrix %>% hclust(method="ward.D2")

## ----save-data,echo=F----------------------------------------------------
# save m2 for social network analysis later
term.doc.matrix <- m2
term.doc.matrix %>% save(file="./export/termDocMatrix.rdata")

## ----plot-cluster, fig.width=8, fig.height=6, out.height='.9\\textwidth'----
plot(fit)
fit %>% rect.hclust(k=9) # cut tree into 6 clusters
groups <- fit %>% cutree(k=9)

## ----kmeans--------------------------------------------------------------
m3 <- m2 %>% t() # transpose the matrix to cluster documents (tweets)
set.seed(122) # set a fixed random seed to make the result reproducible
k <- 10 # number of clusters
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers, digits=3) # cluster centers

## ----print-clusters------------------------------------------------------
for (i in 1:k) {
  cat(paste("cluster ", i, ":  ", sep=""))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:9], "\n")
  # print the tweets of every cluster
  # print(tweets[which(kmeansResult$cluster==i)])
}

## ----echo=F--------------------------------------------------------------
set.seed(523)

## ----topic modelling-----------------------------------------------------
dtm <- tdm %>% as.DocumentTermMatrix()

#exclude empty rows
rowTotals <- apply(dtm , 1, sum) #Find the sum of words in each Document
dtm.new   <- dtm[rowTotals> 0, ]           #remove all docs without words

#identify where it is to remove it from df
empty_rows <- as.integer(dtm[rowTotals == 0, ]$dimnames[1][[1]])

removeRows <- function(rowNum, data) {
  newData <- data[-rowNum, , drop = FALSE]
  rownames(newData) <- NULL
  newData
}

tweets.df_no_na <- removeRows(empty_rows, tweets.df)

library(topicmodels)
lda <- LDA(dtm.new, k=8) # find 8 topics
term <- terms(lda, 10) # first 7 terms of every topic
term <- apply(term, MARGIN=2, paste, collapse=", ") %>% print

## ----density-plot, tidy=F, fig.width=10, out.width="\\textwidth", out.height="0.5\\textwidth", fig.align='center', crop=T----
rdm.topics <- topics(lda) # 1st topic identified for every document (tweet)
rdm.topics <- data.frame(date=as.IDate(tweets.df_no_na$created), 
                         topic=rdm.topics)
ggplot(rdm.topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack")

png(filename="./plot/time_topics.png",
    height=720, 
    width=1080,
    units="px"
)
ggplot(rdm.topics, aes(date, fill = term[topic])) +
  geom_density(position = "stack")
dev.off()

## ----eval=F--------------------------------------------------------------
## # install package sentiment140
## require(devtools)
## install_github('sentiment140', 'okugami79')

## ----sentiment-----------------------------------------------------------
# sentiment analysis
library(sentiment)
sentiments <- sentiment(tweets.df$text)
table(sentiments$polarity)
# sentiment plot
sentiments$score <- 0
sentiments$score[sentiments$polarity == "positive"] <- 1
sentiments$score[sentiments$polarity == "negative"] <- -1
sentiments$date <- as.IDate(tweets.df$created)
result <- aggregate(score ~ date, data=sentiments, sum)

## ----sentiment plot------------------------------------------------------
plot(result, type="l")

png(filename="./plot/time_sentiments.png",
    height=720, 
    width=1080,
    units="px"
)
plot(result, type="l")
dev.off()
