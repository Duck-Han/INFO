#Scraping Twitter data

#1)install & load twitterR package
install.packages("twitteR")
install.packages('tidytext')

library(tidytext)
library(ggplot2)
library(dplyr)
library(twitteR)
library(tidyverse)

library(remotes)
install_github("EmilHvitfeldt/textdata")
install_github("juliasilge/tidytext")

#2) load API
API_key <- 'VRbDhQuwyHXovZky2Ol6Hnlxy'
API_secret_key <- 'SnwUXDoBQV1GarmsT66dP4hzpzZuzVmXRUDFbfASFxmnvjuaxm'
access_token <- '1242263591996145664-GYXNWciKrIznYPlij34WG0PnlKHXTq'
access_token_secret <- 'J4RqygDVs59JWglMJSwmS29E6pNHagjVxZHIMdzWZ27t3'
setup_twitter_oauth(API_key, API_secret_key, access_token, access_token_secret)

#3) sentimental analysis

# import data
COVID_19 <- searchTwitter("COVID-19",n=1000,lang = "en")
COVID_19_df <- twListToDF(COVID_19) # Convert to data frame
tweet_words <- COVID_19_df %>% select(id, text) %>% unnest_tokens(word,text)

#First glance of raw data
tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")

#create a list of stop words: a list of words that aren't worth including
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("19", "t.co", "rt", "covid","que","de","el","https","amp")))

#look for meaningful words
tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)
tweet_words_interesting %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word,n, function(n) -n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, 
                                                                                                                           hjust = 1)) + xlab("")

#conduct sentiment analysis
bing_lex <- get_sentiments("nrc")
fn_sentiment <- tweet_words_interesting %>% left_join(bing_lex)
fn_sentiment %>% filter(!is.na(sentiment)) %>% group_by(sentiment) %>% summarise(n=n())

#Parse stock market data
# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

start <- as.Date("2020-01-01")
end <- as.Date("2020-03-21")

getSymbols("AAPL", src = "yahoo", from = start, to = end)
plot(AAPL[, "AAPL.Close"], main = "AAPL")
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")
