#Load required packages
library(ggplot2)
library(lubridate)
library(Scale)
library(reshape2)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(stringr)
library(syuzhet) 
library(dplyr ) 
library(tidyverse)
library(lubridate)


#1)eda - to detrmine the stats w.r.t date and time
#load the whatsap group chat data for exploration purpose
data <- read.csv("mel.txt",header = F)
View(data)
#split date and time
data1 <- separate(data,V2,"time",sep = "-")
View(data1)
#convert the date variable to date type
data1$V1<-dmy(data1$V1)
#extract hours
data1 <- separate(data1,time,"hour",sep = ":")
#remove NAs
data1<-na.omit(data1)
dim(data1)
#plot the counts against hour
ggplot(data1,aes(x=hour))+geom_bar()
data1$dayofweek<-day(data1$V1)
#obtain weekdays
data1$week<-wday(data1$V1,label = T)
summary(data1$week)
#plot the counts against week
ggplot(data1,aes(x=week))+geom_bar()
month(data1$V1)
data1$mnth<-month(data1$V1)
#plot the counts against month
ggplot(data1,aes(x=factor(mnth)))+geom_bar()

#2)for sentiment analysis
#upload whatsap group data 
text <- readLines("mel.txt")
# create a corpus
docs <- Corpus(VectorSource(text))

#clean the data,remove unwanted symbols and words and stem
trans <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, trans, "/")
docs <- tm_map(docs, trans, "@")
docs <- tm_map(docs, trans, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, removeWords, c("pradeep","c603","el","ky","tarun","b903","<",">"))
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)

#create the document term matrix
dtm <- TermDocumentMatrix(docs)
mat <- as.matrix(dtm)
v <- sort(rowSums(mat),decreasing=TRUE)

#Data frame
data <- data.frame(word = names(v),freq=v)
head(data, 10)


#generate the wordcloud
set.seed(1056)
wordcloud(words = data$word, freq = data$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))


#fetch sentiment words from texts
Sentiment <- get_nrc_sentiment(text)
head(Sentiment)
text <- cbind(text,Sentiment)

#count the sentiment words by category
TotalSentiment <- data.frame(colSums(text[,c(2:11)]))
names(TotalSentiment) <- "count"
TotalSentiment <- cbind("sentiment" = rownames(TotalSentiment), TotalSentiment)
rownames(TotalSentiment) <- NULL

#total sentiment score of all texts
ggplot(data = TotalSentiment, aes(x = sentiment, y = count)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total Count") + ggtitle("Total Sentiment Score for Melange Residence Whatsap messaging group")


