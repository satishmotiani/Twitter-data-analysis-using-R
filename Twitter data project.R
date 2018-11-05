library(twitteR)
library(ROAuth)
library(RCurl)
library(wordcloud)
library(ggplot2)
library(dplyr)
library(sentiment)
library(syuzhet)
library(stringr)
library(plyr)
library(httr)
library(tm)
library(widyr)
library(tidytext)
library(RColorBrewer)
library(igraph)
library(ggraph)
library(lubricate)


consumerKey="xxxxxxxxxxxxxxx"
consumerSecret="xxxxxxxxxxxxx"
accesstoken="xxxxxxxxxxxxxxx"
accesstokensecret="xxxxxxxxxxxxx"

twitteR:::setup_twitter_oauth(consumerKey, consumerSecret, accesstoken, accesstokensecret)

tweets<-searchTwitter("#MarchForOurLives", n=30000, lang = 'en')

tweets.df <- ldply(tweets, function(t) t$toDataFrame())

write.csv(tweets.df, "data.csv")

text = tweets.df$text

text1 = gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",text)
text2 = gsub("http[^[:blank:]]+", "", text1)
text3 = gsub("@\\w+","",text2)
text4 = gsub("[[:punct:]]"," ",text3)
text5 = gsub("[^[:alnum:]]"," ",text4)
text6 = gsub("RT", "", text5)

write.csv(text6, "text.csv")


#sentiment analysis
mysentiment <- get_nrc_sentiment(text6)
SentimentScores <- data.frame(colSums(mysentiment[,]))
names(SentimentScores) <- "Score"
SentimentScores <- cbind("sentiment" = rownames(SentimentScores), SentimentScores)
rownames(SentimentScores) <- NULL
ggplot(data = SentimentScores, aes(x = sentiment, y = Score)) + geom_bar(aes(fill = sentiment), stat = "identity") + theme(legend.position = "none") + xlab("Sentiment") + ylab("Score") + ggtitle("Total Sentiment Score Based on Tweets")



#corpus
text7 = Corpus(VectorSource(text6))
text7.removeEmoji = function(x) gsub("\\p{So}|\\p{Cn}", "", x, perl = TRUE)
text7.removeSpecialChar = function(x) gsub("[^[:alnum:]///' ]", "", x)
text7 = tm_map(text7, content_transformer(text7.removeEmoji))
text = tm_map(text7, content_transformer(text7.removeSpecialChar))
text7 = tm_map(text7, removePunctuation, preserve_intra_word_dashes = TRUE)
text7 = tm_map(text7, content_transformer(tolower))
text7 = tm_map(text7, removeWords, c(stopwords("english"), "will", "can"))
text7 = tm_map(text7, removeNumbers)
text7 = tm_map(text7, stripWhitespace)
text7 = tm_map(text7, stemDocument)

#wordcloud

pal <- brewer.pal(8, "Dark2")
wordcloud(text7, min.freq = 5, max.words = Inf, width = 1000, height = 1000, random.order = FALSE, color = pal)

tdm <- TermDocumentMatrix(text6)


tweetsdata$created_date=as.Date(tweets.df$created,format='%Y-%m-%d %H:%M:%S')
tweetsdata$hour = format(as.POSIXct(tweets.df$created,format="%Y-%m-%d %H:%M:%S"),"%H")
tweetsdata$isRetweetNum=ifelse(tweets.df$isRetweet==FALSE,0,1)
tweetsdata$retweetedNum=ifelse(tweets.df$retweeted==FALSE,0,1)
tweetsdata$tweet=c(1)

options(repr.plot.width=6, repr.plot.height=4)
HourFrame=as.data.frame(table(tweetsdata$hour))
colnames(HourFrame)=c("Hour","TweetCount")
HourFrame$Hour=as.numeric(HourFrame$Hour)
y=ddply(tweetsdata, .(tweetsdata$hour), numcolwise(sum))
HourFrame$retweetedNum=y$isRetweetNum
ggplot(HourFrame,aes(x=Hour))+geom_line(aes(y = TweetCount, colour = "TotalTweets")) + 
  geom_line(aes(y = retweetedNum, colour = "Retweets"))



tdm <- TermDocumentMatrix(text7)
docterm <- as.matrix(tdm)

#most common words
termfreq <- rowSums(docterm)
termfreq <- sort(termfreq, decreasing = TRUE)
barplot(termfreq[1:12], col = 'blue', las=2)

#word association
associations <- findAssocs(tdm, "month", 0.2)
associations_df <- list_vect2df(associations)[, 2:3]
ggplot(associations_df, aes(y = associations_df[, 1])) + 
  geom_point(aes(x = associations_df[, 2]), 
             data = associations_df, size = 3)

Replies=tweets.df[is.na(tweets.df$replyToSN)==FALSE,]
y=ddply(Replies, .(replyToSN), numcolwise(sum))
Replies=y[,c("replyToSN","tweet")]
Replies=Replies[order(-Replies$tweet),]
Replies=head(Replies,n=20)
colnames(Replies)=c("User","RepliesReceived")
Replies


y=ddply(tweets.df, .(screenName), numcolwise(sum))
popularUsers=y[,c("screenName","retweetCount","tweet")]
popularUsers=popularUsers[order(-popularUsers$retweetCount),]
popularUsers=head(popularUsers,n=10)
popularUsers

devices=tweets.df$statusSource
devices <- gsub("","", devices)
devices <- strsplit(devices, ">")
devices <- sapply(devices,function(x) ifelse(length(x) > 1, x[2], x[1]))

devices_source=as.data.frame(table(devices))
colnames(devices_source)=c("Device","TweetCount")
devices_source=devices_source[devices_source$TweetCount>50,]
devices_source=devices_source[order(-devices_source$TweetCount),]

ggplot(devices_source,aes(x=reorder(Device, -TweetCount),y=TweetCount,fill=TweetCount))+geom_bar(stat='identity') +coord_flip()

