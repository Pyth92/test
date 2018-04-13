###################################################################
############################# BITCOIN #############################
###################################################################
#install.packages("quantmod")
library(quantmod)

start <- as.Date("2015-01-01")
tickers <- c('BTC-EUR')
getSymbols(tickers, from=start, src="yahoo")
#column headings ("open","high","low","close","volume","adj.")

Adj_close_price <- do.call(merge, lapply(tickers, function(x) Ad(get(x))))
colnames(Adj_close_price) <- tickers
Adj_close_price <- data.frame(Date = time(Adj_close_price), coredata(Adj_close_price))
Adj_close_price <- Adj_close_price[nrow(Adj_close_price):1,]
Adj_close_price<-Adj_close_price[order(as.Date(Adj_close_price$Date, format="%d/%m/%Y")),]
plot(Adj_close_price, type="l", main="Bitcoin's Value", xlab="Date", ylab="BTC (€)", col="blue")
n<-length(Adj_close_price$Date)
return.daily <- ((Adj_close_price$BTC.EUR[2:n] - Adj_close_price$BTC.EUR[1:(n - 1)])/Adj_close_price$BTC.EUR[1:(n - 1)])
plot(return.daily, type="l", main="Bitcoin's Returns", xlab="Date", ylab="BTC (€)", col="blue")
return.daily
#vol.daily <- sd(return.daily)
#vol.annual <- vol.daily*sqrt(250)
return.daily_reduced <- return.daily[-c(1:1163)] #From the 10.03.18 to today
plot(return.daily_reduced, type="l", main="Bitcoin's Returns", xlab="Date", ylab="BTC (€)", col="blue")
return.daily_reduced
###################################################################
###################################################################




###################################################################
############################# Twitter #############################
###################################################################

############################# Packages #############################
# install.packages("twitteR") 
# install.packages(c("devtools", "rjson", "bit64", "httr"))
# install.packages("openssl")
# install.packages("openssl@1.1")
# install.packages("httr")
# install.packages("httpuv")
# install.packages("magrittr")
# install.packages("dplyr")
library(twitteR)
library(devtools)
library(openssl)
library(httr)
library(httpuv)
library(magrittr)
library(dplyr)
###################################################################
###################################################################


###################### Twitter Access #############################
install_github("twitteR", username="geoffjentry") #http://thinktostart.com/twitter-authentification-with-r/
api_key <- "La7CjZbHXvpRxefVAmawCd2GT"
api_secret <- "R54JqpjD3rt9ogN0Vpm8Lo7Jz9X29YL5LrIteaV2hQlCj2gnwE"
access_token <- "966318256410808320-dweDEsssC1noTxPYKVuG1XYYOTJ5xjm"
access_token_secret <- "2yQOLr3UrKQY0hckpfVs50TUgTNXs7lpwK98KmhIgVdjD"
setup_twitter_oauth(api_key,api_secret)
###################################################################
###################################################################


####### Twitter Search to retrieve the Tweets each day ############
tws<-searchTwitter("bitcoin -filter:retweets", n=100, lang="en", since='2018-04-12', until='2018-04-13', locale=NULL, geocode=NULL, sinceID=NULL, maxID=NULL,resultType="popular")
df<-do.call("rbind",lapply(tws,as.data.frame)) #convert tweets into data frame
write.csv(df,file="2018-04-12.csv") #produces a csv file of the data
###################################################################
###################################################################


################## Twitter Packages for qdap ######################
#Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre7') # for 64-bit version
if (Sys.info()['sysname'] == 'Darwin') {
  libjvm <- paste0(system2('/usr/libexec/java_home',stdout = TRUE)[1],'/jre/lib/server/libjvm.dylib')
  message (paste0('Load libjvm.dylib from: ',libjvm))
  dyn.load(libjvm)
}
library(rJava)
#install.packages("rJava",,"http://rforge.net", type='source')
#install.packages("qdap")
#install_github("qdapDictionaries", "trinker")
#install_github("qdapRegex", "trinker")
#install_github("qdapTools", "trinker")
#install_github("qdap", "trinker")
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(qdap)
library(readr)
###################################################################
###################################################################


########## Twitter Polarity without cleaning the datas ############
X2018_03_10 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-10.csv", col_types = cols(X1 = col_skip()))
#X2018_03_10<-sentSplit(X2018_03_10,"text" , rm.var = NULL, endmarks = c("?", ".", "!","|"), incomplete.sub = TRUE, rm.bracket = TRUE, stem.col = FALSE, text.place = "right", verbose = is.global(2)) #The function sentSplit does not change anything
#SentSplit does not change the polarity since the ponctuation does not have any impact on sentiment analysis
polX2018_03_10 <- polarity(X2018_03_10, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_10
AvgPolX2018_03_10<-select(polX2018_03_10[["group"]], ave.polarity)
# We need to clean the data because it impacts a lot our polarity result
###################################################################
###################################################################


################### Text cleaning & Wordcloud #####################
#install.packages("tm")  # for text mining
#install.packages("SnowballC") # for text stemming
#install.packages("wordcloud") # word-cloud generator 
#install.packages("RColorBrewer") # color palettes
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
#text<-readLines(file.choose())
X2018_03_10 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-10.csv", col_types = cols(X1 = col_skip()))
#Load the data as a corpus
X2018_03_10_cleaned <- Corpus(VectorSource(X2018_03_10$text))
inspect(X2018_03_10_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_10_cleaned <- tm_map(X2018_03_10_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_10_cleaned <- tm_map(X2018_03_10_cleaned, toSpace, "/")
X2018_03_10_cleaned <- tm_map(X2018_03_10_cleaned, toSpace, "@")
X2018_03_10_cleaned <- tm_map(X2018_03_10_cleaned, toSpace, "\\|")
# Convert the text to lower case
X2018_03_10_cleaned <- tm_map(X2018_03_10_cleaned, content_transformer(tolower))
# Remove numbers
# X2018_03_10_cleaned <- tm_map(X2018_03_10_cleaned, removeNumbers)
# Remove english common stopwords
X2018_03_10_cleaned <- tm_map(X2018_03_10_cleaned, removeWords, stopwords("english"))
# Remove your own stop word
# specify your stopwords as a character vector
X2018_03_10_cleaned <- tm_map(X2018_03_10_cleaned, removeWords, c("jpg")) 
# Remove punctuations
X2018_03_10_cleaned <- tm_map(X2018_03_10_cleaned, removePunctuation)
# Eliminate extra white spaces
X2018_03_10_cleaned <- tm_map(X2018_03_10_cleaned, stripWhitespace)
# Text stemming
# X2018_03_10_cleaned <- tm_map(X2018_03_10_cleaned, stemDocument)
dtm <- TermDocumentMatrix(X2018_03_10_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_10_cleaned)

X2018_03_11 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-11.csv", col_types = cols(X1 = col_skip()))
X2018_03_11_cleaned <- Corpus(VectorSource(X2018_03_11$text))
inspect(X2018_03_11_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_11_cleaned <- tm_map(X2018_03_11_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_11_cleaned <- tm_map(X2018_03_11_cleaned, toSpace, "/")
X2018_03_11_cleaned <- tm_map(X2018_03_11_cleaned, toSpace, "@")
X2018_03_11_cleaned <- tm_map(X2018_03_11_cleaned, toSpace, "\\|")
X2018_03_11_cleaned <- tm_map(X2018_03_11_cleaned, content_transformer(tolower))
X2018_03_11_cleaned <- tm_map(X2018_03_11_cleaned, removeWords, stopwords("english"))
X2018_03_11_cleaned <- tm_map(X2018_03_11_cleaned, removePunctuation)
X2018_03_11_cleaned <- tm_map(X2018_03_11_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_11_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_11_cleaned)

X2018_03_12 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-12.csv", col_types = cols(X1 = col_skip()))
X2018_03_12_cleaned <- X2018_03_12[-7,] #The 7th tweet was presenting some issues. I tried to remove them with the following codes but it could word so we will not use this tweet.
#X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned, removeWords, c("xedxa0xbdxedxb0x9dxedxa0xbcxedxbdxb5xedxa0xbcxedxbcx8a"))
#X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned, removeWords, c("\xed", "\xa0", "\xbd", "\xed", "\xb0", "\x9d", "\xed", "\xa0", "\xbc", "\xed", "\xbd", "\xb5", "\xed", "\xa0", "\xbc", "\xed", "\xbc", "\x8a"))
#X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned$`7`, removeWords, stopwords("Bitcoin"))
#X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned, removeWords, c("\xed\xa0\xbd\xed\xb0\x9d\xed\xa0\xbc\xed\xbd\xb5\xed\xa0\xbc\xed\xbc\x8a"))
#X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned, removeWords, c("������������������"))
X2018_03_12_cleaned <- Corpus(VectorSource(X2018_03_12_cleaned$text))
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned, removeURL)
#X2018_03_12_cleaned <- sapply(X2018_03_12_cleaned,function(row) iconv(row, "latin1", "ASCII", sub=""))
#X2018_03_12_cleaned <- iconv(X2018_03_12_cleaned, "latin1", "ASCII", sub="")
inspect(X2018_03_12_cleaned)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned, toSpace, "/")
X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned, toSpace, "@")
X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned, toSpace, "\\|")
X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned, content_transformer(tolower))
X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned, removeWords, stopwords("english"))
X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned, removePunctuation)
X2018_03_12_cleaned <- tm_map(X2018_03_12_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_12_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_12_cleaned)

X2018_03_13 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-13.csv", col_types = cols(X1 = col_skip()))
X2018_03_13_cleaned <- Corpus(VectorSource(X2018_03_13$text))
inspect(X2018_03_13_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_13_cleaned <- tm_map(X2018_03_13_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_13_cleaned <- tm_map(X2018_03_13_cleaned, toSpace, "/")
X2018_03_13_cleaned <- tm_map(X2018_03_13_cleaned, toSpace, "@")
X2018_03_13_cleaned <- tm_map(X2018_03_13_cleaned, toSpace, "\\|")
X2018_03_13_cleaned <- tm_map(X2018_03_13_cleaned, content_transformer(tolower))
X2018_03_13_cleaned <- tm_map(X2018_03_13_cleaned, removeWords, stopwords("english"))
X2018_03_13_cleaned <- tm_map(X2018_03_13_cleaned, removePunctuation)
X2018_03_13_cleaned <- tm_map(X2018_03_13_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_13_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_13_cleaned)

X2018_03_14 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-14.csv", col_types = cols(X1 = col_skip()))
X2018_03_14_cleaned <- Corpus(VectorSource(X2018_03_14$text))
inspect(X2018_03_14_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_14_cleaned <- tm_map(X2018_03_14_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_14_cleaned <- tm_map(X2018_03_14_cleaned, toSpace, "/")
X2018_03_14_cleaned <- tm_map(X2018_03_14_cleaned, toSpace, "@")
X2018_03_14_cleaned <- tm_map(X2018_03_14_cleaned, toSpace, "\\|")
X2018_03_14_cleaned <- tm_map(X2018_03_14_cleaned, content_transformer(tolower))
X2018_03_14_cleaned <- tm_map(X2018_03_14_cleaned, removeWords, stopwords("english"))
X2018_03_14_cleaned <- tm_map(X2018_03_14_cleaned, removePunctuation)
X2018_03_14_cleaned <- tm_map(X2018_03_14_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_14_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_14_cleaned)

X2018_03_15 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-15.csv", col_types = cols(X1 = col_skip()))
X2018_03_15_cleaned <- Corpus(VectorSource(X2018_03_15$text))
inspect(X2018_03_15_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_15_cleaned <- tm_map(X2018_03_15_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_15_cleaned <- tm_map(X2018_03_15_cleaned, toSpace, "/")
X2018_03_15_cleaned <- tm_map(X2018_03_15_cleaned, toSpace, "@")
X2018_03_15_cleaned <- tm_map(X2018_03_15_cleaned, toSpace, "\\|")
X2018_03_15_cleaned <- tm_map(X2018_03_15_cleaned, content_transformer(tolower))
X2018_03_15_cleaned <- tm_map(X2018_03_15_cleaned, removeWords, stopwords("english"))
X2018_03_15_cleaned <- tm_map(X2018_03_15_cleaned, removePunctuation)
X2018_03_15_cleaned <- tm_map(X2018_03_15_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_15_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_15_cleaned)

X2018_03_16 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-16.csv", col_types = cols(X1 = col_skip()))
X2018_03_16_cleaned <- Corpus(VectorSource(X2018_03_16$text))
inspect(X2018_03_16_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_16_cleaned <- tm_map(X2018_03_16_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_16_cleaned <- tm_map(X2018_03_16_cleaned, toSpace, "/")
X2018_03_16_cleaned <- tm_map(X2018_03_16_cleaned, toSpace, "@")
X2018_03_16_cleaned <- tm_map(X2018_03_16_cleaned, toSpace, "\\|")
X2018_03_16_cleaned <- tm_map(X2018_03_16_cleaned, content_transformer(tolower))
X2018_03_16_cleaned <- tm_map(X2018_03_16_cleaned, removeWords, stopwords("english"))
X2018_03_16_cleaned <- tm_map(X2018_03_16_cleaned, removePunctuation)
X2018_03_16_cleaned <- tm_map(X2018_03_16_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_16_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_16_cleaned)

X2018_03_17 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-17.csv", col_types = cols(X1 = col_skip()))
X2018_03_17_cleaned <- X2018_03_17[-28,]
X2018_03_17_cleaned <- Corpus(VectorSource(X2018_03_17_cleaned$text))
inspect(X2018_03_17_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_17_cleaned <- tm_map(X2018_03_17_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_17_cleaned <- tm_map(X2018_03_17_cleaned, toSpace, "/")
X2018_03_17_cleaned <- tm_map(X2018_03_17_cleaned, toSpace, "@")
X2018_03_17_cleaned <- tm_map(X2018_03_17_cleaned, toSpace, "\\|")
X2018_03_17_cleaned <- tm_map(X2018_03_17_cleaned, content_transformer(tolower))
X2018_03_17_cleaned <- tm_map(X2018_03_17_cleaned, removeWords, stopwords("english"))
X2018_03_17_cleaned <- tm_map(X2018_03_17_cleaned, removePunctuation)
X2018_03_17_cleaned <- tm_map(X2018_03_17_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_17_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_17_cleaned)

X2018_03_18 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-18.csv", col_types = cols(X1 = col_skip()))
X2018_03_18_cleaned <- X2018_03_18[-23,]
X2018_03_18_cleaned <- Corpus(VectorSource(X2018_03_18_cleaned$text))
inspect(X2018_03_18_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_18_cleaned <- tm_map(X2018_03_18_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_18_cleaned <- tm_map(X2018_03_18_cleaned, toSpace, "/")
X2018_03_18_cleaned <- tm_map(X2018_03_18_cleaned, toSpace, "@")
X2018_03_18_cleaned <- tm_map(X2018_03_18_cleaned, toSpace, "\\|")
X2018_03_18_cleaned <- tm_map(X2018_03_18_cleaned, content_transformer(tolower))
X2018_03_18_cleaned <- tm_map(X2018_03_18_cleaned, removeWords, stopwords("english"))
X2018_03_18_cleaned <- tm_map(X2018_03_18_cleaned, removePunctuation)
X2018_03_18_cleaned <- tm_map(X2018_03_18_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_18_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_18_cleaned)

X2018_03_19 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-19.csv", col_types = cols(X1 = col_skip()))
X2018_03_19_cleaned <- X2018_03_19[-6,]
X2018_03_19_cleaned <- Corpus(VectorSource(X2018_03_19_cleaned$text))
inspect(X2018_03_19_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_19_cleaned <- tm_map(X2018_03_19_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_19_cleaned <- tm_map(X2018_03_19_cleaned, toSpace, "/")
X2018_03_19_cleaned <- tm_map(X2018_03_19_cleaned, toSpace, "@")
X2018_03_19_cleaned <- tm_map(X2018_03_19_cleaned, toSpace, "\\|")
X2018_03_19_cleaned <- tm_map(X2018_03_19_cleaned, content_transformer(tolower))
X2018_03_19_cleaned <- tm_map(X2018_03_19_cleaned, removeWords, stopwords("english"))
X2018_03_19_cleaned <- tm_map(X2018_03_19_cleaned, removePunctuation)
X2018_03_19_cleaned <- tm_map(X2018_03_19_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_19_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_19_cleaned)

X2018_03_20 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-20.csv", col_types = cols(X1 = col_skip()))
X2018_03_20_cleaned <- Corpus(VectorSource(X2018_03_20$text))
inspect(X2018_03_20_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_20_cleaned <- tm_map(X2018_03_20_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_20_cleaned <- tm_map(X2018_03_20_cleaned, toSpace, "/")
X2018_03_20_cleaned <- tm_map(X2018_03_20_cleaned, toSpace, "@")
X2018_03_20_cleaned <- tm_map(X2018_03_20_cleaned, toSpace, "\\|")
X2018_03_20_cleaned <- tm_map(X2018_03_20_cleaned, content_transformer(tolower))
X2018_03_20_cleaned <- tm_map(X2018_03_20_cleaned, removeWords, stopwords("english"))
X2018_03_20_cleaned <- tm_map(X2018_03_20_cleaned, removePunctuation)
X2018_03_20_cleaned <- tm_map(X2018_03_20_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_20_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_20_cleaned)

X2018_03_21 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-21.csv", col_types = cols(X1 = col_skip()))
X2018_03_21_cleaned <- Corpus(VectorSource(X2018_03_21$text))
inspect(X2018_03_21_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_21_cleaned <- tm_map(X2018_03_21_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_21_cleaned <- tm_map(X2018_03_21_cleaned, toSpace, "/")
X2018_03_21_cleaned <- tm_map(X2018_03_21_cleaned, toSpace, "@")
X2018_03_21_cleaned <- tm_map(X2018_03_21_cleaned, toSpace, "\\|")
X2018_03_21_cleaned <- tm_map(X2018_03_21_cleaned, content_transformer(tolower))
X2018_03_21_cleaned <- tm_map(X2018_03_21_cleaned, removeWords, stopwords("english"))
X2018_03_21_cleaned <- tm_map(X2018_03_21_cleaned, removePunctuation)
X2018_03_21_cleaned <- tm_map(X2018_03_21_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_21_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_21_cleaned)

X2018_03_22 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-22.csv", col_types = cols(X1 = col_skip()))
X2018_03_22_cleaned <- X2018_03_22[-18,]
X2018_03_22_cleaned <- Corpus(VectorSource(X2018_03_22_cleaned$text))
inspect(X2018_03_22_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_22_cleaned <- tm_map(X2018_03_22_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_22_cleaned <- tm_map(X2018_03_22_cleaned, toSpace, "/")
X2018_03_22_cleaned <- tm_map(X2018_03_22_cleaned, toSpace, "@")
X2018_03_22_cleaned <- tm_map(X2018_03_22_cleaned, toSpace, "\\|")
X2018_03_22_cleaned <- tm_map(X2018_03_22_cleaned, content_transformer(tolower))
X2018_03_22_cleaned <- tm_map(X2018_03_22_cleaned, removeWords, stopwords("english"))
X2018_03_22_cleaned <- tm_map(X2018_03_22_cleaned, removePunctuation)
X2018_03_22_cleaned <- tm_map(X2018_03_22_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_22_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_22_cleaned)

X2018_03_23 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-23.csv", col_types = cols(X1 = col_skip()))
X2018_03_23_cleaned <- X2018_03_23[-c(3,8),]
X2018_03_23_cleaned <- Corpus(VectorSource(X2018_03_23_cleaned$text))
inspect(X2018_03_23_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_23_cleaned <- tm_map(X2018_03_23_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_23_cleaned <- tm_map(X2018_03_23_cleaned, toSpace, "/")
X2018_03_23_cleaned <- tm_map(X2018_03_23_cleaned, toSpace, "@")
X2018_03_23_cleaned <- tm_map(X2018_03_23_cleaned, toSpace, "\\|")
X2018_03_23_cleaned <- tm_map(X2018_03_23_cleaned, content_transformer(tolower))
X2018_03_23_cleaned <- tm_map(X2018_03_23_cleaned, removeWords, stopwords("english"))
X2018_03_23_cleaned <- tm_map(X2018_03_23_cleaned, removePunctuation)
X2018_03_23_cleaned <- tm_map(X2018_03_23_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_23_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_23_cleaned)

X2018_03_24 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-24.csv", col_types = cols(X1 = col_skip()))
X2018_03_24_cleaned <- Corpus(VectorSource(X2018_03_24$text))
inspect(X2018_03_24_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_24_cleaned <- tm_map(X2018_03_24_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_24_cleaned <- tm_map(X2018_03_24_cleaned, toSpace, "/")
X2018_03_24_cleaned <- tm_map(X2018_03_24_cleaned, toSpace, "@")
X2018_03_24_cleaned <- tm_map(X2018_03_24_cleaned, toSpace, "\\|")
X2018_03_24_cleaned <- tm_map(X2018_03_24_cleaned, content_transformer(tolower))
X2018_03_24_cleaned <- tm_map(X2018_03_24_cleaned, removeWords, stopwords("english"))
X2018_03_24_cleaned <- tm_map(X2018_03_24_cleaned, removePunctuation)
X2018_03_24_cleaned <- tm_map(X2018_03_24_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_24_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_24_cleaned)

X2018_03_25 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-25.csv", col_types = cols(X1 = col_skip()))
X2018_03_25_cleaned <- Corpus(VectorSource(X2018_03_25$text))
inspect(X2018_03_25_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_25_cleaned <- tm_map(X2018_03_25_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_25_cleaned <- tm_map(X2018_03_25_cleaned, toSpace, "/")
X2018_03_25_cleaned <- tm_map(X2018_03_25_cleaned, toSpace, "@")
X2018_03_25_cleaned <- tm_map(X2018_03_25_cleaned, toSpace, "\\|")
X2018_03_25_cleaned <- tm_map(X2018_03_25_cleaned, content_transformer(tolower))
X2018_03_25_cleaned <- tm_map(X2018_03_25_cleaned, removeWords, stopwords("english"))
X2018_03_25_cleaned <- tm_map(X2018_03_25_cleaned, removePunctuation)
X2018_03_25_cleaned <- tm_map(X2018_03_25_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_25_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_25_cleaned)

X2018_03_26 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-26.csv", col_types = cols(X1 = col_skip()))
X2018_03_26_cleaned <- X2018_03_26[-15,]
X2018_03_26_cleaned <- Corpus(VectorSource(X2018_03_26_cleaned$text))
inspect(X2018_03_26_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_26_cleaned <- tm_map(X2018_03_26_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_26_cleaned <- tm_map(X2018_03_26_cleaned, toSpace, "/")
X2018_03_26_cleaned <- tm_map(X2018_03_26_cleaned, toSpace, "@")
X2018_03_26_cleaned <- tm_map(X2018_03_26_cleaned, toSpace, "\\|")
X2018_03_26_cleaned <- tm_map(X2018_03_26_cleaned, content_transformer(tolower))
X2018_03_26_cleaned <- tm_map(X2018_03_26_cleaned, removeWords, stopwords("english"))
X2018_03_26_cleaned <- tm_map(X2018_03_26_cleaned, removePunctuation)
X2018_03_26_cleaned <- tm_map(X2018_03_26_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_26_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_26_cleaned)

X2018_03_27 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-27.csv", col_types = cols(X1 = col_skip()))
X2018_03_27_cleaned <- X2018_03_27[-7,]
X2018_03_27_cleaned <- Corpus(VectorSource(X2018_03_27_cleaned$text))
inspect(X2018_03_27_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_27_cleaned <- tm_map(X2018_03_27_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_27_cleaned <- tm_map(X2018_03_27_cleaned, toSpace, "/")
X2018_03_27_cleaned <- tm_map(X2018_03_27_cleaned, toSpace, "@")
X2018_03_27_cleaned <- tm_map(X2018_03_27_cleaned, toSpace, "\\|")
X2018_03_27_cleaned <- tm_map(X2018_03_27_cleaned, content_transformer(tolower))
X2018_03_27_cleaned <- tm_map(X2018_03_27_cleaned, removeWords, stopwords("english"))
X2018_03_27_cleaned <- tm_map(X2018_03_27_cleaned, removePunctuation)
X2018_03_27_cleaned <- tm_map(X2018_03_27_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_27_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_27_cleaned)

X2018_03_28 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-28.csv", col_types = cols(X1 = col_skip()))
X2018_03_28_cleaned <- Corpus(VectorSource(X2018_03_28$text))
inspect(X2018_03_28_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_28_cleaned <- tm_map(X2018_03_28_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_28_cleaned <- tm_map(X2018_03_28_cleaned, toSpace, "/")
X2018_03_28_cleaned <- tm_map(X2018_03_28_cleaned, toSpace, "@")
X2018_03_28_cleaned <- tm_map(X2018_03_28_cleaned, toSpace, "\\|")
X2018_03_28_cleaned <- tm_map(X2018_03_28_cleaned, content_transformer(tolower))
X2018_03_28_cleaned <- tm_map(X2018_03_28_cleaned, removeWords, stopwords("english"))
X2018_03_28_cleaned <- tm_map(X2018_03_28_cleaned, removePunctuation)
X2018_03_28_cleaned <- tm_map(X2018_03_28_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_28_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_28_cleaned)

X2018_03_29 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-29.csv", col_types = cols(X1 = col_skip()))
X2018_03_29_cleaned <- X2018_03_29[-34,]
X2018_03_29_cleaned <- Corpus(VectorSource(X2018_03_29_cleaned$text))
inspect(X2018_03_29_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_29_cleaned <- tm_map(X2018_03_29_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_29_cleaned <- tm_map(X2018_03_29_cleaned, toSpace, "/")
X2018_03_29_cleaned <- tm_map(X2018_03_29_cleaned, toSpace, "@")
X2018_03_29_cleaned <- tm_map(X2018_03_29_cleaned, toSpace, "\\|")
X2018_03_29_cleaned <- tm_map(X2018_03_29_cleaned, content_transformer(tolower))
X2018_03_29_cleaned <- tm_map(X2018_03_29_cleaned, removeWords, stopwords("english"))
X2018_03_29_cleaned <- tm_map(X2018_03_29_cleaned, removePunctuation)
X2018_03_29_cleaned <- tm_map(X2018_03_29_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_29_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_29_cleaned)

X2018_03_30 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-30.csv", col_types = cols(X1 = col_skip()))
X2018_03_30_cleaned <- Corpus(VectorSource(X2018_03_30$text))
inspect(X2018_03_30_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_30_cleaned <- tm_map(X2018_03_30_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_30_cleaned <- tm_map(X2018_03_30_cleaned, toSpace, "/")
X2018_03_30_cleaned <- tm_map(X2018_03_30_cleaned, toSpace, "@")
X2018_03_30_cleaned <- tm_map(X2018_03_30_cleaned, toSpace, "\\|")
X2018_03_30_cleaned <- tm_map(X2018_03_30_cleaned, content_transformer(tolower))
X2018_03_30_cleaned <- tm_map(X2018_03_30_cleaned, removeWords, stopwords("english"))
X2018_03_30_cleaned <- tm_map(X2018_03_30_cleaned, removePunctuation)
X2018_03_30_cleaned <- tm_map(X2018_03_30_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_30_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_30_cleaned)

X2018_03_31 <- read_csv("~/Desktop/Mémoire/Datas/2018-03-31.csv", col_types = cols(X1 = col_skip()))
X2018_03_31_cleaned <- X2018_03_31[-c(8,19),]
X2018_03_31_cleaned <- Corpus(VectorSource(X2018_03_31_cleaned$text))
inspect(X2018_03_31_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_03_31_cleaned <- tm_map(X2018_03_31_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_03_31_cleaned <- tm_map(X2018_03_31_cleaned, toSpace, "/")
X2018_03_31_cleaned <- tm_map(X2018_03_31_cleaned, toSpace, "@")
X2018_03_31_cleaned <- tm_map(X2018_03_31_cleaned, toSpace, "\\|")
X2018_03_31_cleaned <- tm_map(X2018_03_31_cleaned, content_transformer(tolower))
X2018_03_31_cleaned <- tm_map(X2018_03_31_cleaned, removeWords, stopwords("english"))
X2018_03_31_cleaned <- tm_map(X2018_03_31_cleaned, removePunctuation)
X2018_03_31_cleaned <- tm_map(X2018_03_31_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_03_31_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_03_31_cleaned)

X2018_04_01 <- read_csv("~/Desktop/Mémoire/Datas/2018-04-01.csv", col_types = cols(X1 = col_skip()))
X2018_04_01_cleaned <- X2018_04_01[-c(9,16),]
X2018_04_01_cleaned <- Corpus(VectorSource(X2018_04_01_cleaned$text))
inspect(X2018_04_01_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_04_01_cleaned <- tm_map(X2018_04_01_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_04_01_cleaned <- tm_map(X2018_04_01_cleaned, toSpace, "/")
X2018_04_01_cleaned <- tm_map(X2018_04_01_cleaned, toSpace, "@")
X2018_04_01_cleaned <- tm_map(X2018_04_01_cleaned, toSpace, "\\|")
X2018_04_01_cleaned <- tm_map(X2018_04_01_cleaned, content_transformer(tolower))
X2018_04_01_cleaned <- tm_map(X2018_04_01_cleaned, removeWords, stopwords("english"))
X2018_04_01_cleaned <- tm_map(X2018_04_01_cleaned, removePunctuation)
X2018_04_01_cleaned <- tm_map(X2018_04_01_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_04_01_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_04_01_cleaned)

X2018_04_02 <- read_csv("~/Desktop/Mémoire/Datas/2018-04-02.csv", col_types = cols(X1 = col_skip()))
X2018_04_02_cleaned <- X2018_04_02[-c(4,28),]
X2018_04_02_cleaned <- Corpus(VectorSource(X2018_04_02_cleaned$text))
inspect(X2018_04_02_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_04_02_cleaned <- tm_map(X2018_04_02_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_04_02_cleaned <- tm_map(X2018_04_02_cleaned, toSpace, "/")
X2018_04_02_cleaned <- tm_map(X2018_04_02_cleaned, toSpace, "@")
X2018_04_02_cleaned <- tm_map(X2018_04_02_cleaned, toSpace, "\\|")
X2018_04_02_cleaned <- tm_map(X2018_04_02_cleaned, content_transformer(tolower))
X2018_04_02_cleaned <- tm_map(X2018_04_02_cleaned, removeWords, stopwords("english"))
X2018_04_02_cleaned <- tm_map(X2018_04_02_cleaned, removePunctuation)
X2018_04_02_cleaned <- tm_map(X2018_04_02_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_04_02_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_04_02_cleaned)

X2018_04_03 <- read_csv("~/Desktop/Mémoire/Datas/2018-04-03.csv", col_types = cols(X1 = col_skip()))
X2018_04_03_cleaned <- Corpus(VectorSource(X2018_04_03$text))
inspect(X2018_04_03_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_04_03_cleaned <- tm_map(X2018_04_03_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_04_03_cleaned <- tm_map(X2018_04_03_cleaned, toSpace, "/")
X2018_04_03_cleaned <- tm_map(X2018_04_03_cleaned, toSpace, "@")
X2018_04_03_cleaned <- tm_map(X2018_04_03_cleaned, toSpace, "\\|")
X2018_04_03_cleaned <- tm_map(X2018_04_03_cleaned, content_transformer(tolower))
X2018_04_03_cleaned <- tm_map(X2018_04_03_cleaned, removeWords, stopwords("english"))
X2018_04_03_cleaned <- tm_map(X2018_04_03_cleaned, removePunctuation)
X2018_04_03_cleaned <- tm_map(X2018_04_03_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_04_03_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_04_03_cleaned)

X2018_04_04 <- read_csv("~/Desktop/Mémoire/Datas/2018-04-04.csv", col_types = cols(X1 = col_skip()))
X2018_04_04_cleaned <- X2018_04_04[-27,]
X2018_04_04_cleaned <- Corpus(VectorSource(X2018_04_04_cleaned$text))
inspect(X2018_04_04_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_04_04_cleaned <- tm_map(X2018_04_04_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_04_04_cleaned <- tm_map(X2018_04_04_cleaned, toSpace, "/")
X2018_04_04_cleaned <- tm_map(X2018_04_04_cleaned, toSpace, "@")
X2018_04_04_cleaned <- tm_map(X2018_04_04_cleaned, toSpace, "\\|")
X2018_04_04_cleaned <- tm_map(X2018_04_04_cleaned, content_transformer(tolower))
X2018_04_04_cleaned <- tm_map(X2018_04_04_cleaned, removeWords, stopwords("english"))
X2018_04_04_cleaned <- tm_map(X2018_04_04_cleaned, removePunctuation)
X2018_04_04_cleaned <- tm_map(X2018_04_04_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_04_04_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_04_04_cleaned)

X2018_04_05 <- read_csv("~/Desktop/Mémoire/Datas/2018-04-05.csv", col_types = cols(X1 = col_skip()))
X2018_04_05_cleaned <- X2018_04_05[-30,]
X2018_04_05_cleaned <- Corpus(VectorSource(X2018_04_05_cleaned$text))
inspect(X2018_04_05_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_04_05_cleaned <- tm_map(X2018_04_05_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_04_05_cleaned <- tm_map(X2018_04_05_cleaned, toSpace, "/")
X2018_04_05_cleaned <- tm_map(X2018_04_05_cleaned, toSpace, "@")
X2018_04_05_cleaned <- tm_map(X2018_04_05_cleaned, toSpace, "\\|")
X2018_04_05_cleaned <- tm_map(X2018_04_05_cleaned, content_transformer(tolower))
X2018_04_05_cleaned <- tm_map(X2018_04_05_cleaned, removeWords, stopwords("english"))
X2018_04_05_cleaned <- tm_map(X2018_04_05_cleaned, removePunctuation)
X2018_04_05_cleaned <- tm_map(X2018_04_05_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_04_05_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_04_05_cleaned)

X2018_04_06 <- read_csv("~/Desktop/Mémoire/Datas/2018-04-06.csv", col_types = cols(X1 = col_skip()))
X2018_04_06_cleaned <- X2018_04_06[-27,]
X2018_04_06_cleaned <- Corpus(VectorSource(X2018_04_06_cleaned$text))
inspect(X2018_04_06_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_04_06_cleaned <- tm_map(X2018_04_06_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_04_06_cleaned <- tm_map(X2018_04_06_cleaned, toSpace, "/")
X2018_04_06_cleaned <- tm_map(X2018_04_06_cleaned, toSpace, "@")
X2018_04_06_cleaned <- tm_map(X2018_04_06_cleaned, toSpace, "\\|")
X2018_04_06_cleaned <- tm_map(X2018_04_06_cleaned, content_transformer(tolower))
X2018_04_06_cleaned <- tm_map(X2018_04_06_cleaned, removeWords, stopwords("english"))
X2018_04_06_cleaned <- tm_map(X2018_04_06_cleaned, removePunctuation)
X2018_04_06_cleaned <- tm_map(X2018_04_06_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_04_06_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_04_06_cleaned)

X2018_04_07 <- read_csv("~/Desktop/Mémoire/Datas/2018-04-07.csv", col_types = cols(X1 = col_skip()))
X2018_04_07_cleaned <- Corpus(VectorSource(X2018_04_07$text))
inspect(X2018_04_07_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_04_07_cleaned <- tm_map(X2018_04_07_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_04_07_cleaned <- tm_map(X2018_04_07_cleaned, toSpace, "/")
X2018_04_07_cleaned <- tm_map(X2018_04_07_cleaned, toSpace, "@")
X2018_04_07_cleaned <- tm_map(X2018_04_07_cleaned, toSpace, "\\|")
X2018_04_07_cleaned <- tm_map(X2018_04_07_cleaned, content_transformer(tolower))
X2018_04_07_cleaned <- tm_map(X2018_04_07_cleaned, removeWords, stopwords("english"))
X2018_04_07_cleaned <- tm_map(X2018_04_07_cleaned, removePunctuation)
X2018_04_07_cleaned <- tm_map(X2018_04_07_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_04_07_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_04_07_cleaned)

X2018_04_08 <- read_csv("~/Desktop/Mémoire/Datas/2018-04-08.csv", col_types = cols(X1 = col_skip()))
X2018_04_08_cleaned <- X2018_04_08[-2,]
X2018_04_08_cleaned <- Corpus(VectorSource(X2018_04_08_cleaned$text))
inspect(X2018_04_08_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_04_08_cleaned <- tm_map(X2018_04_08_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_04_08_cleaned <- tm_map(X2018_04_08_cleaned, toSpace, "/")
X2018_04_08_cleaned <- tm_map(X2018_04_08_cleaned, toSpace, "@")
X2018_04_08_cleaned <- tm_map(X2018_04_08_cleaned, toSpace, "\\|")
X2018_04_08_cleaned <- tm_map(X2018_04_08_cleaned, content_transformer(tolower))
X2018_04_08_cleaned <- tm_map(X2018_04_08_cleaned, removeWords, stopwords("english"))
X2018_04_08_cleaned <- tm_map(X2018_04_08_cleaned, removePunctuation)
X2018_04_08_cleaned <- tm_map(X2018_04_08_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_04_08_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_04_08_cleaned)

X2018_04_09 <- read_csv("~/Desktop/Mémoire/Datas/2018-04-09.csv", col_types = cols(X1 = col_skip()))
X2018_04_09_cleaned <- X2018_04_09[-21,]
X2018_04_09_cleaned <- Corpus(VectorSource(X2018_04_09_cleaned$text))
inspect(X2018_04_09_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_04_09_cleaned <- tm_map(X2018_04_09_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_04_09_cleaned <- tm_map(X2018_04_09_cleaned, toSpace, "/")
X2018_04_09_cleaned <- tm_map(X2018_04_09_cleaned, toSpace, "@")
X2018_04_09_cleaned <- tm_map(X2018_04_09_cleaned, toSpace, "\\|")
X2018_04_09_cleaned <- tm_map(X2018_04_09_cleaned, content_transformer(tolower))
X2018_04_09_cleaned <- tm_map(X2018_04_09_cleaned, removeWords, stopwords("english"))
X2018_04_09_cleaned <- tm_map(X2018_04_09_cleaned, removePunctuation)
X2018_04_09_cleaned <- tm_map(X2018_04_09_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_04_09_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_04_09_cleaned)

X2018_04_10 <- read_csv("~/Desktop/Mémoire/Datas/2018-04-10.csv", col_types = cols(X1 = col_skip()))
X2018_04_10_cleaned <- X2018_04_10[-31,]
X2018_04_10_cleaned <- Corpus(VectorSource(X2018_04_10_cleaned$text))
inspect(X2018_04_10_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_04_10_cleaned <- tm_map(X2018_04_10_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_04_10_cleaned <- tm_map(X2018_04_10_cleaned, toSpace, "/")
X2018_04_10_cleaned <- tm_map(X2018_04_10_cleaned, toSpace, "@")
X2018_04_10_cleaned <- tm_map(X2018_04_10_cleaned, toSpace, "\\|")
X2018_04_10_cleaned <- tm_map(X2018_04_10_cleaned, content_transformer(tolower))
X2018_04_10_cleaned <- tm_map(X2018_04_10_cleaned, removeWords, stopwords("english"))
X2018_04_10_cleaned <- tm_map(X2018_04_10_cleaned, removePunctuation)
X2018_04_10_cleaned <- tm_map(X2018_04_10_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_04_10_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_04_10_cleaned)

X2018_04_11 <- read_csv("~/Desktop/Mémoire/Datas/2018-04-11.csv", col_types = cols(X1 = col_skip()))
X2018_04_11_cleaned <- X2018_04_11[-c(6,15),]
X2018_04_11_cleaned <- Corpus(VectorSource(X2018_04_11_cleaned$text))
inspect(X2018_04_11_cleaned)
removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
X2018_04_11_cleaned <- tm_map(X2018_04_11_cleaned, removeURL)
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
X2018_04_11_cleaned <- tm_map(X2018_04_11_cleaned, toSpace, "/")
X2018_04_11_cleaned <- tm_map(X2018_04_11_cleaned, toSpace, "@")
X2018_04_11_cleaned <- tm_map(X2018_04_11_cleaned, toSpace, "\\|")
X2018_04_11_cleaned <- tm_map(X2018_04_11_cleaned, content_transformer(tolower))
X2018_04_11_cleaned <- tm_map(X2018_04_11_cleaned, removeWords, stopwords("english"))
X2018_04_11_cleaned <- tm_map(X2018_04_11_cleaned, removePunctuation)
X2018_04_11_cleaned <- tm_map(X2018_04_11_cleaned, stripWhitespace)
dtm <- TermDocumentMatrix(X2018_04_11_cleaned)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 10)
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
inspect(X2018_04_11_cleaned)
###################################################################
###################################################################


############################ Polarity #############################
#write.csv(X2018_03_10_cleaned,file="2018-03-10_cleaned.csv") 
X2018_03_10_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-10_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_10_cleaned <- polarity(X2018_03_10_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_10_cleaned
AvgPolX2018_03_10_cleaned<-select(polX2018_03_10_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_11_cleaned,file="2018-03-11_cleaned.csv") 
X2018_03_11_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-11_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_11_cleaned <- polarity(X2018_03_11_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_11_cleaned
AvgPolX2018_03_11_cleaned<-select(polX2018_03_11_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_12_cleaned,file="2018-03-12_cleaned.csv") 
X2018_03_12_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-12_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_12_cleaned <- polarity(X2018_03_12_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_12_cleaned
AvgPolX2018_03_12_cleaned<-select(polX2018_03_12_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_13_cleaned,file="2018-03-13_cleaned.csv") 
X2018_03_13_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-13_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_13_cleaned <- polarity(X2018_03_13_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_13_cleaned
AvgPolX2018_03_13_cleaned<-select(polX2018_03_13_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_14_cleaned,file="2018-03-14_cleaned.csv") 
X2018_03_14_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-14_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_14_cleaned <- polarity(X2018_03_14_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_14_cleaned
AvgPolX2018_03_14_cleaned<-select(polX2018_03_14_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_15_cleaned,file="2018-03-15_cleaned.csv") 
X2018_03_15_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-15_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_15_cleaned <- polarity(X2018_03_15_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_15_cleaned
AvgPolX2018_03_15_cleaned<-select(polX2018_03_15_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_16_cleaned,file="2018-03-16_cleaned.csv") 
X2018_03_16_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-16_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_16_cleaned <- polarity(X2018_03_16_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_16_cleaned
AvgPolX2018_03_16_cleaned<-select(polX2018_03_16_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_17_cleaned,file="2018-03-17_cleaned.csv") 
X2018_03_17_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-17_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_17_cleaned <- polarity(X2018_03_17_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_17_cleaned
AvgPolX2018_03_17_cleaned<-select(polX2018_03_17_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_18_cleaned,file="2018-03-18_cleaned.csv") 
X2018_03_18_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-18_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_18_cleaned <- polarity(X2018_03_18_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_18_cleaned
AvgPolX2018_03_18_cleaned<-select(polX2018_03_18_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_19_cleaned,file="2018-03-19_cleaned.csv") 
X2018_03_19_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-19_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_19_cleaned <- polarity(X2018_03_19_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_19_cleaned
AvgPolX2018_03_19_cleaned<-select(polX2018_03_19_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_20_cleaned,file="2018-03-20_cleaned.csv") 
X2018_03_20_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-20_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_20_cleaned <- polarity(X2018_03_20_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_20_cleaned
AvgPolX2018_03_20_cleaned<-select(polX2018_03_20_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_21_cleaned,file="2018-03-21_cleaned.csv") 
X2018_03_21_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-21_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_21_cleaned <- polarity(X2018_03_21_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_21_cleaned
AvgPolX2018_03_21_cleaned<-select(polX2018_03_21_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_22_cleaned,file="2018-03-22_cleaned.csv") 
X2018_03_22_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-22_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_22_cleaned <- polarity(X2018_03_22_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_22_cleaned
AvgPolX2018_03_22_cleaned<-select(polX2018_03_22_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_23_cleaned,file="2018-03-23_cleaned.csv") 
X2018_03_23_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-23_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_23_cleaned <- polarity(X2018_03_23_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_23_cleaned
AvgPolX2018_03_23_cleaned<-select(polX2018_03_23_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_24_cleaned,file="2018-03-24_cleaned.csv") 
X2018_03_24_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-24_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_24_cleaned <- polarity(X2018_03_24_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_24_cleaned
AvgPolX2018_03_24_cleaned<-select(polX2018_03_24_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_25_cleaned,file="2018-03-25_cleaned.csv") 
X2018_03_25_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-25_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_25_cleaned <- polarity(X2018_03_25_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_25_cleaned
AvgPolX2018_03_25_cleaned<-select(polX2018_03_25_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_26_cleaned,file="2018-03-26_cleaned.csv") 
X2018_03_26_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-26_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_26_cleaned <- polarity(X2018_03_26_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_26_cleaned
AvgPolX2018_03_26_cleaned<-select(polX2018_03_26_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_27_cleaned,file="2018-03-27_cleaned.csv") 
X2018_03_27_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-27_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_27_cleaned <- polarity(X2018_03_27_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_27_cleaned
AvgPolX2018_03_27_cleaned<-select(polX2018_03_27_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_28_cleaned,file="2018-03-28_cleaned.csv") 
X2018_03_28_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-28_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_28_cleaned <- polarity(X2018_03_28_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_28_cleaned
AvgPolX2018_03_28_cleaned<-select(polX2018_03_28_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_29_cleaned,file="2018-03-29_cleaned.csv") 
X2018_03_29_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-29_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_29_cleaned <- polarity(X2018_03_29_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_29_cleaned
AvgPolX2018_03_29_cleaned<-select(polX2018_03_29_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_30_cleaned,file="2018-03-30_cleaned.csv") 
X2018_03_30_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-30_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_30_cleaned <- polarity(X2018_03_30_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_30_cleaned
AvgPolX2018_03_30_cleaned<-select(polX2018_03_30_cleaned[["group"]], ave.polarity)

#write.csv(X2018_03_31_cleaned,file="2018-03-31_cleaned.csv") 
X2018_03_31_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-03-31_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_03_31_cleaned <- polarity(X2018_03_31_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_03_31_cleaned
AvgPolX2018_03_31_cleaned<-select(polX2018_03_31_cleaned[["group"]], ave.polarity)

#write.csv(X2018_04_01_cleaned,file="2018-04-01_cleaned.csv") 
X2018_04_01_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-04-01_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_04_01_cleaned <- polarity(X2018_04_01_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_04_01_cleaned
AvgPolX2018_04_01_cleaned<-select(polX2018_04_01_cleaned[["group"]], ave.polarity)

#write.csv(X2018_04_02_cleaned,file="2018-04-02_cleaned.csv") 
X2018_04_02_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-04-02_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_04_02_cleaned <- polarity(X2018_04_02_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_04_02_cleaned
AvgPolX2018_04_02_cleaned<-select(polX2018_04_02_cleaned[["group"]], ave.polarity)

#write.csv(X2018_04_03_cleaned,file="2018-04-03_cleaned.csv") 
X2018_04_03_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-04-03_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_04_03_cleaned <- polarity(X2018_04_03_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_04_03_cleaned
AvgPolX2018_04_03_cleaned<-select(polX2018_04_03_cleaned[["group"]], ave.polarity)

#write.csv(X2018_04_04_cleaned,file="2018-04-04_cleaned.csv") 
X2018_04_04_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-04-04_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_04_04_cleaned <- polarity(X2018_04_04_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_04_04_cleaned
AvgPolX2018_04_04_cleaned<-select(polX2018_04_04_cleaned[["group"]], ave.polarity)

#write.csv(X2018_04_05_cleaned,file="2018-04-05_cleaned.csv") 
X2018_04_05_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-04-05_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_04_05_cleaned <- polarity(X2018_04_05_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_04_05_cleaned
AvgPolX2018_04_05_cleaned<-select(polX2018_04_05_cleaned[["group"]], ave.polarity)

#write.csv(X2018_04_06_cleaned,file="2018-04-06_cleaned.csv") 
X2018_04_06_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-04-06_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_04_06_cleaned <- polarity(X2018_04_06_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_04_06_cleaned
AvgPolX2018_04_06_cleaned<-select(polX2018_04_06_cleaned[["group"]], ave.polarity)

#write.csv(X2018_04_07_cleaned,file="2018-04-07_cleaned.csv") 
X2018_04_07_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-04-07_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_04_07_cleaned <- polarity(X2018_04_07_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_04_07_cleaned
AvgPolX2018_04_07_cleaned<-select(polX2018_04_07_cleaned[["group"]], ave.polarity)

#write.csv(X2018_04_08_cleaned,file="2018-04-08_cleaned.csv") 
X2018_04_08_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-04-08_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_04_08_cleaned <- polarity(X2018_04_08_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_04_08_cleaned
AvgPolX2018_04_08_cleaned<-select(polX2018_04_08_cleaned[["group"]], ave.polarity)

#write.csv(X2018_04_09_cleaned,file="2018-04-09_cleaned.csv") 
X2018_04_09_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-04-09_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_04_09_cleaned <- polarity(X2018_04_09_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_04_09_cleaned
AvgPolX2018_04_09_cleaned<-select(polX2018_04_09_cleaned[["group"]], ave.polarity)

#write.csv(X2018_04_10_cleaned,file="2018-04-10_cleaned.csv") 
X2018_04_10_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-04-10_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_04_10_cleaned <- polarity(X2018_04_10_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_04_10_cleaned
AvgPolX2018_04_10_cleaned<-select(polX2018_04_10_cleaned[["group"]], ave.polarity)

#write.csv(X2018_04_11_cleaned,file="2018-04-11_cleaned.csv") 
X2018_04_11_cleaned <- read_csv("~/Desktop/Mémoire/Datas/2018-04-11_cleaned.csv", col_types = cols(X1 = col_skip()))
polX2018_04_11_cleaned <- polarity(X2018_04_11_cleaned$text, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)
polX2018_04_11_cleaned
AvgPolX2018_04_11_cleaned<-select(polX2018_04_11_cleaned[["group"]], ave.polarity)
###################################################################
###################################################################

######################### Cleaned Polarity ########################
polarity_cleaned <- c(AvgPolX2018_03_10_cleaned$ave.polarity, AvgPolX2018_03_11_cleaned$ave.polarity, AvgPolX2018_03_12_cleaned$ave.polarity, 
                 AvgPolX2018_03_13_cleaned$ave.polarity, AvgPolX2018_03_14_cleaned$ave.polarity, AvgPolX2018_03_15_cleaned$ave.polarity, 
                 AvgPolX2018_03_16_cleaned$ave.polarity, AvgPolX2018_03_17_cleaned$ave.polarity, AvgPolX2018_03_18_cleaned$ave.polarity, 
                 AvgPolX2018_03_19_cleaned$ave.polarity, AvgPolX2018_03_20_cleaned$ave.polarity, AvgPolX2018_03_21_cleaned$ave.polarity, 
                 AvgPolX2018_03_22_cleaned$ave.polarity, AvgPolX2018_03_23_cleaned$ave.polarity, AvgPolX2018_03_24_cleaned$ave.polarity, 
                 AvgPolX2018_03_25_cleaned$ave.polarity, AvgPolX2018_03_26_cleaned$ave.polarity, AvgPolX2018_03_27_cleaned$ave.polarity, 
                 AvgPolX2018_03_28_cleaned$ave.polarity, AvgPolX2018_03_29_cleaned$ave.polarity, AvgPolX2018_03_30_cleaned$ave.polarity, 
                 AvgPolX2018_03_31_cleaned$ave.polarity, AvgPolX2018_04_01_cleaned$ave.polarity, AvgPolX2018_04_02_cleaned$ave.polarity, 
                 AvgPolX2018_04_03_cleaned$ave.polarity, AvgPolX2018_04_04_cleaned$ave.polarity, AvgPolX2018_04_05_cleaned$ave.polarity, 
                 AvgPolX2018_04_06_cleaned$ave.polarity, AvgPolX2018_04_07_cleaned$ave.polarity, AvgPolX2018_04_08_cleaned$ave.polarity,
                 AvgPolX2018_04_09_cleaned$ave.polarity, AvgPolX2018_04_10_cleaned$ave.polarity, AvgPolX2018_04_11_cleaned$ave.polarity)
plot(polarity_cleaned, type="l", main="Twitter Average Cleaned Polarity about Bitcoin", xlab="Observation", ylab="Polarity", col="red")
###################################################################
###################################################################


################### Comparison BTC & Cleaned Polarity #####################
polarity_cleaned
length(polarity_cleaned)
return.daily_reduced
Adj_close_price_reduced <- Adj_close_price[-c(1:1164), ]
length(Adj_close_price_reduced$Date)
BTC <- cbind(Adj_close_price_reduced, return.daily_reduced)
BTC <- BTC[-17, ] # 25.03.18 appears two times
# Add the vector "polarity_cleaned"" into the matrix 
length(BTC$Date)
i<-length(BTC$Date) # The last date polarity is not available directly since the day is not over
BTC <- BTC[c(1:(i-1)),]
length(BTC$Date)
complete_data<-cbind(polarity_cleaned, BTC)

# Plot the Data together
par(mar = c(5, 5, 3, 5))
plot(complete_data$Date, complete_data$return.daily_reduced, type ="l", ylab="BTC returns (€)", main = "Comparison daily BTC returns & daily Polarity over time", xlab = "Time", col = "blue")
par(new = TRUE)
plot(complete_data$Date, complete_data$polarity_cleaned, type = "l", xaxt = "n", yaxt = "n", ylab = "", xlab = "", col = "red", lty = 1)
axis(side = 4)
mtext("Polarity", side = 4, line = 3)
par(mar=c(5.1, 4.1, 4.1, 8.1), xpd=TRUE)
legend("topright", inset=c(0,1.2), legend=c("BTC (€)","Polarity (RHS)"), col = c("blue", "red"), lty = c(1, 1), cex=0.7)
cor(complete_data$polarity_cleaned,complete_data$return.daily_reduced)
#install.packages("ggpubr")
library(ggpubr)

ggscatter(complete_data, x = "return.daily_reduced", y = "polarity_cleaned", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Daily BTC return (€)", ylab = "Daily Polarity")

shapiro.test(complete_data$return.daily_reduced)
shapiro.test(complete_data$polarity_cleaned)
# Shapiro-Wilk test can be performed as follow:
# Null hypothesis: the data are normally distributed
# Alternative hypothesis: the data are not normally distributed
ggqqplot(complete_data$polarity_cleaned, ylab = "Polarity")
ggqqplot(complete_data$return.daily_reduced, ylab = "BTC (€)")

####### Pearson correlation test (if non linear correlation) --> rank based #######
res <- cor.test(complete_data$return.daily_reduced, complete_data$polarity_cleaned, 
                method = "pearson")
res
# p-value = 0.1236 > 0.05 -> not significant
# The p-value of the test is 0.1236, which is more than the significance level alpha = 0.05. We can conclude that our two variables are not significantly correlated.

####### Kendall rank correlation test #######
res2 <- cor.test(complete_data$return.daily_reduced, complete_data$polarity_cleaned, 
                method = "kendall")
res2
# p-value = 0.1295 > 0.05 -> not significant
# The p-value of the test is 0.1295, which is more than the significance level alpha = 0.05. We can conclude that our two variables are not significantly correlated.

####### Spearman rank correlation coefficient #######
res3 <- cor.test(complete_data$return.daily_reduced, complete_data$polarity_cleaned, 
                method = "spearman")
res3
# p-value = 0.1675 > 0.05 -> not significant
# The p-value of the test is 0.1675, which is more than the significance level alpha = 0.05. We can conclude that our two variables are not significantly correlated.

return.daily_reduced.ls = lm(complete_data$return.daily_reduced ~ complete_data$polarity_cleaned, data=complete_data)
summary(return.daily_reduced.ls)
#plot(return.daily_reduced.ls)

#Lagg 1
polarity_cleaned_lagged <- lag(complete_data$polarity_cleaned, k = 1)
return.daily_reduced_lagged.ls = lm(complete_data$return.daily_reduced ~ polarity_cleaned_lagged, data=complete_data)
summary(return.daily_reduced_lagged.ls)

#Lagg 2
polarity_cleaned_lagged2 <- lag(complete_data$polarity_cleaned, k = 2)
return.daily_reduced_lagged.ls = lm(complete_data$return.daily_reduced ~ polarity_cleaned_lagged2, data=complete_data)
summary(return.daily_reduced_lagged.ls)

#Lagg 3
polarity_cleaned_lagged3 <- lag(complete_data$polarity_cleaned, k = 3)
return.daily_reduced_lagged.ls = lm(complete_data$return.daily_reduced ~ polarity_cleaned_lagged3, data=complete_data)
summary(return.daily_reduced_lagged.ls)
###################################################################
###################################################################


############### Robustness check using Facebook ###################
library(qdap)
library(dplyr)
library(magrittr)
library(Rfacebook)
library(devtools)
require(Rfacebook)
install_github("pablobarbera/Rfacebook/Rfacebook") # install latest version from GitHub
# token generated here: https://developers.facebook.com/tools/explorer
token <- "EAACEdEose0cBABuaabbZBqOla2zdOw7Nvj5J4xuIX7eu9dYPF24QuFbAlzpC8sXZApLFhr5ZAkHU2Td9zJJ06k1ARcxHO4uTUdfZAsCa4QFnpNtlk1B5ZCDgx2tXoKWwvNmafSXZB8Hee66vO4RofnZCLYZAuuTEawj7df3QKB7rxcFq5iNKskyz1veYGa7Q8qgZD"
################### Bloomberg #########################
bloomberg <- searchPages(string = "bloombergbusiness", token, n = 1)
#Takes the one page call bloomberg
bloomberg_selection <- bloomberg[which.max(bloomberg$talking_about_count), ] 
#will retrieve the page with the most like from the above research
bloomberg_posts <- getPage(page=bloomberg_selection$id, token=token, n = 200, since = "2017/01/01", until = NULL)
#will take the 200 shared pages on bloomberg
bloomberg_filter<-filter(bloomberg_posts,grepl("bitcoin|Bitcoin|crypto|currencies|currency", message))
#only keeping the comments with the words we want
bloomberg_posts_comments <- getPost(bloomberg_filter$id[2], token=token, n = 300, comments = TRUE, likes = TRUE)
#choose one of the page
CommentsVector<-select(bloomberg_posts_comments[["comments"]], message)
#only keeps the comments
polFB<-polarity(CommentsVector, grouping.var = NULL, polarity.frame = qdapDictionaries::key.pol, constrain = FALSE, negators = qdapDictionaries::negation.words, amplifiers = qdapDictionaries::amplification.words, deamplifiers = qdapDictionaries::deamplification.words, question.weight = 0, amplifier.weight = 0.8, n.before = 4, n.after = 2, rm.incomplete = FALSE, digits = 3)

polFB