library(ROAuth)
library(plyr)
library(stringr)
library(ggplot2)
library(twitteR)
library(RJSONIO)
library(sentiment)

################################################################################
setwd('C:/Users/Jinyi/Dropbox/Jinyi/Github/data-mining-twitteR')
download.file(url='http://curl.haxx.se/ca/cacert.pem',destfile='cacert.pem')

requestURL <- 'https://api.twitter.com/oauth/request_token'
accessURL <- 'https://api.twitter.com/oauth/access_token'
authURL <- 'https://api.twitter.com/oauth/authorize'

consumerKey <- 'bHBkjtbHg0CFhLVVQHtAPBJqL'
consumerSecret <- 'ISBt4WsdKMb5RgBeyYDw2IYp4rqv7a2EWVdOXduQKLpYiL3csW'

twitCred <- OAuthFactory$new(consumerKey=consumerKey,
                             consumerSecret=consumerSecret,
                             requestURL=requestURL,
                             accessURL=accessURL,
                             authURL=authURL)

twitCred$handshake(cainfo="cacert.pem")
save(twitCred, file = "twitter_auth.Rdata")

################################################################################
setwd("C:/Users/Jinyi/Dropbox/Jinyi/Github/data-mining-twitteR")
load("twitter_auth.Rdata")
registerTwitterOAuth(twitCred)

geocode_list = list()
geocode_list[[1]] = "44.654813,-63.601594,"
names(geocode_list)[1] = "Halifax"

region = 'Halifax'
radius = 100
key_word = c('#Halifax')

geocode_string = paste(geocode_list[region],radius,'mi', sep='')
some_tweets = searchTwitter(searchString = key_word, 
                            n=1500, 
                         #   geocode= geocode_string,
                            since='2001-03-01',
                            lang = 'en',
                            cainfo="cacert.pem")

some_txt = sapply(some_tweets, function(x) x$getText())

some_txt = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", some_txt)
# remove at people
some_txt = gsub("@\\w+", "", some_txt)
# remove punctuation
some_txt = gsub("[[:punct:]]", "", some_txt)
# remove numbers
some_txt = gsub("[[:digit:]]", "", some_txt)
# remove html links
some_txt = gsub("http\\w+", "", some_txt)
# remove unnecessary spaces
some_txt = gsub("[ \t]{2,}", "", some_txt)
some_txt = gsub("^\\s+|\\s+$", "", some_txt)



# define "tolower error handling" function 
try.error = function(x)
{
  # create missing value
  y = NA
  # tryCatch error
  try_error = tryCatch(tolower(x), error=function(e) e)
  # if not an error
  if (!inherits(try_error, "error"))
    y = tolower(x)
  # result
  return(y)
}
# lower case using try.error with sapply 
some_txt = sapply(some_txt, try.error)

# remove NAs in some_txt
some_txt = some_txt[!is.na(some_txt)]
names(some_txt) = NULL

# classify emotion
class_emo = classify_emotion(some_txt, algorithm="bayes", prior=1.0)
# get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(some_txt, algorithm="bayes")
# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=some_txt, emotion=emotion,
                     polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df,
                 emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
geom_bar(aes(y=..count.., fill=emotion)) +
scale_fill_brewer(palette="Dark2")

# plot distribution of polarity
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="polarity categories", y="number of tweets") +
  opts(title = "Sentiment Analysis of Tweets about Starbucks\n(classification by polarity)",
       plot.title = theme_text(size=12))
