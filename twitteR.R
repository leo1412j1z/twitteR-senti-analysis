#                                         "sentimental tweets analysis"

#initializing libraries
library(twitteR)
library(tm)
library(SnowballC)
library(syuzhet)


# "copy paste your keys and tokens"
consumer_key <- 'copy paste your key '
consumer_secret <- 'copy paste your secret key'
access_token <- 'copy paste your  token'
access_secret <- 'copy paste your access secret keys '


#get  authentication from twitter developer site
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)


#download tweets which you want to analyze
tweets = userTimeline("@id which you want to analyze ",n=1000)


#converting tweets into data frame 
df <- twListToDF(tweets) 


#remove all gaps spaces and special char using gsub
df2 = gsub("http.*","",df$text)
df2 <- gsub("https.*","",df2)
df2 <- gsub("#.*","",df2)
df2 <- gsub("@.*","",df2)


#segrecation of sentences
word.df <- as.vector(df2)
emotion.df <- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(df2, emotion.df) 


#setting values for each sentences
sent.value <- get_sentiment(word.df)


#categorizing each with tweets as positive , negative and neutal
positive.tweets <- word.df[sent.value > 0]
negative.tweets <- word.df[sent.value < 0] 
neutral.tweets <- word.df[sent.value == 0] 
category_senti <- ifelse(sent.value < 0, "Negative", ifelse(sent.value > 0, "Positive", "Neutral"))
category_senti2 <- cbind(tweets,category_senti) 


#out put
ddf = table(category_senti)
barplot(ddf)


#most positive and negative
most.positive <- word.df[sent.value == max(sent.value)]
most.negative <- word.df[sent.value == min(sent.value)]
most.positive
most.negative


