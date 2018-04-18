library(devtools)
install_github("geoffjentry/twitteR")
library(twitteR)
api_key <- "cPV0KSO5TRZDftbXRYpxZ2NGr"
api_secret <- "hp9N9GYP06i0SDhsSSMTqdhprE4EZjsRLvlLRw0pKIa3Tmunhi"
access_token <- "195257267-gBzNcMYNGosqL3yHJZvaQcd76c3xemgqog43Znk9"
access_token_secret <- "uXU6sWxaZbcMjBioKtDhxGOnXVt8AcGkgrwWJORJ5vVwC"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
keywords <- c("New Relic", "#NewRelic")
tweets <- searchTwitter(keywords[1], n=1500)
tweets <- rbind(tweets, searchTwitter(keywords[2], n=1500))
df <- do.call("rbind", lapply(tweets, as.data.frame))
summary(df)
df$language <- factor(df$language) 
df$location <- factor(df$location)
counts=table(df$screenName)
barplot(counts)
df$text=sapply(df$text,function(row) iconv(row,to='UTF-8'))
trim <- function (x) sub('@','',x)
library(stringr)

#Pull out who a message is to
df$to <- sapply(df$text,function(tweet) str_extract(tweet,"^(@[[:alnum:]_]*)")) 
df$to <- sapply(df$to,function(name) trim(name))
#And here's a way of grabbing who has been RT'd
df$rt=sapply(df$text,function(tweet) trim(str_match(tweet,"^RT (@[[:alnum:]_]*)")[2]))

library(tm) 
library(wordcloud)
twitterCorpus <- Corpus(VectorSource(df$text))
twitterCorpus <- tm_map(twitterCorpus, content_transformer(tolower))
twitterCorpus <- tm_map(twitterCorpus, removeNumbers)
twitterCorpus <- tm_map(twitterCorpus, removeWords, stopwords("english")) 
twitterCorpus <- tm_map(twitterCorpus, removeWords, c("http", "https"))
twitterCorpus <- tm_map(twitterCorpus, removePunctuation)
twitterCorpus <- tm_map(twitterCorpus, stripWhitespace)
dtm <- TermDocumentMatrix(twitterCorpus) 
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
dT <- data.frame(words = names(v),freq=v)
wordcloud(words = dT$word, freq = dT$freq, min.freq = 1, max.words=200, random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
