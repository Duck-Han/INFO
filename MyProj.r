#1. Track Covid-19 Over time
remotes::install_github("GuangchuangYu/nCov2019")
require(nCov2019)
require(dplyr)
library(nCov2019)

x <- get_nCov2019()
y <- load_nCov2019()

library(magick)

d <- c(paste0("2020-02-", 12:29), paste0("2020-03-0", 1:9), paste0("2020-03-", 10:31), 
       paste0("2020-04-0", 1:9), paste0("2020-04-", 10:29))

#plot antimated photo
img <- image_graph(1200, 700, res = 96)

out <- lapply(d, function(date){
  p <- plot(y, date=date,
            label=FALSE, continuous_scale=TRUE)
  print(p)
})

dev.off()
animation <- image_animate(img, fps = 2)
print(animation)

#Predict next 10 days 
data <- y[['global']]
usdata <- data %>% dplyr::filter(data$country == 'United States' & data$cum_confirm>100) 

library(forecast)
us <- usdata[44:53,]
case <- ts(us[,3], start=1,frequency = 1)
fit <- tslm(log(case) ~ trend)
fc <- forecast(fit, h=10, lambda = 0)
plot(fc)

plot(us$time,log(us$cum_confirm))
abline(coef = c(0,1))

#2. Linguistic Variation on Twitter
#1) Obtain Twitter Handles
library(rtweet)
leg_dets <- 'https://theunitedstates.io/congress-legislators/legislators-current.csv'

twitters <- read.csv((url(leg_dets)),
                     stringsAsFactors = FALSE) %>%
  #filter(type == 'rep') %>% # & twitter!=''
  rename (state_abbrev = state,
          district_code = district)

# Scrape 1000 tweets from congress
# Load API
consumer_key <- "ot3PamN0AcoAN8oCoQ1LDlVkz"
consumer_secret <- "Iej0ex4HlmxiKJhoadzOLdgpIkXwVMhm4BBWqwP7LGjlqj3YrJ"
access_token <- "1242263591996145664-m33ppD5nDmnrbgtx8wzbE92qLr58Sr"
access_secret <- "406EETPpoyVCqVT9FrhX1Ej7o7wKacaf728cpHrG3jZ56"

token <- create_token(
  app = "COVID-19 SA",
  consumer_key,
  consumer_secret,
  access_token,
  access_secret)

#2) Search Tweets
congress_tweets <- rtweet::get_timeline( 
  twitters$twitter, 
  n = 200,
  check = FALSE) %>%
  mutate(created_at = as.Date(gsub(' .*$', '', 
                                   created_at))) %>%
  filter(is_quote == 'FALSE' & 
           is_retweet == 'FALSE' & 
           created_at >= '2020-01-01' &
           display_text_width > 0)
#3) Join Data
congress_tweets1 <- congress_tweets %>%
  mutate(twitter = toupper(screen_name)) %>%
  select(status_id, created_at, twitter, text) %>%
  inner_join(twitters %>% mutate(twitter = toupper(twitter)))

all_tweets <- congress_tweets1 %>%
  group_by(created_at, party) %>%
  summarise(ts = n()) %>%
  rename(date = created_at)

library(ggplot2)

all_tweets %>%
  filter(party != 'Independent') %>% # Justin Amash & Bernie Sanders & Angus King
  ggplot() +
  geom_line(aes(x = date, 
                y= ts, 
                color = party
  ),
  size = 1.25) +
  theme_minimal() +
  ggthemes::scale_color_stata() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(date_breaks = '1 week', date_labels = "%b %d") +
  theme(legend.position = 'bottom')  +
  labs(title = 'Total congressional tweets by party affiliation')

#4) Linguistic Variation
pan <- 'pandemic|'
cv <- 'coronavirus|corona virus|'
covid <- 'covid19|covid|covid 19|covid-19|'
cvp <- 'coronavirus pandemic'
searches <- paste0(pan, cv, covid, cvp)

covid_tweets <- lapply(1:nrow(congress_tweets1), function(x) {
  
  spots <- gregexpr(pattern = searches, congress_tweets1$text[x], ignore.case=TRUE)
  covid_gram <- regmatches(congress_tweets1$text[x], spots)[[1]] 
  
  if (-1 %in% spots){} else {
    data.frame(doc_id = congress_tweets1$status_id[x],
               date = congress_tweets1$created_at[x],
               twitter = congress_tweets1$twitter[x],
               party = congress_tweets1$party[x],
               covid_gram = covid_gram,
               stringsAsFactors = FALSE)}  })  %>% 
  data.table:::rbindlist() 

covid_tweets <- covid_tweets %>% filter(covid_gram == c("coronavirus","COVID19","pandemic","coronavirus pandemic"))
table(covid_tweets$covid_gram)

covid_tweets <- covid_tweets %>%
  mutate(covid_gram = tolower(covid_gram),
         covid_gram = ifelse(grepl('covid', covid_gram), 'covid19', covid_gram),
         covid_gram = ifelse(grepl('corona virus', covid_gram), 'coronavirus', covid_gram))

covid_tweets %>% sample_n(10) %>% select(-doc_id) %>%knitr::kable()

covid_tweets %>%
  group_by(covid_gram) %>%
  filter(date == min(date)) %>%
  arrange(date) %>%
  select(covid_gram, date, twitter) %>%
  knitr::kable()

all <- covid_tweets %>%
  group_by(date) %>%
  summarize(n = n()) %>%
  left_join(all_tweets %>% group_by(date) %>% summarise(ts = sum(ts))) %>%
  mutate(per = n/ts,
         covid_gram = 'total') %>%
  select(date, covid_gram, n:per)

#data clean 
covid_tweets %>%
  group_by(date, covid_gram) %>% #,party,   
  summarize(n = n()) %>%
  left_join(all_tweets %>% group_by(date) %>% summarise(ts = sum(ts))) %>%
  mutate(per = n/ts) %>%
  bind_rows(all) %>%
  
  ggplot() +
  geom_line(aes(x = date, 
                y= per, 
                color = covid_gram
  ), size = 1.5
  ) +
  theme_minimal() +
  ggthemes::scale_color_stata() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_date(date_breaks = '2 days', date_labels = "%b %d") +
  theme(legend.position = 'top',
        legend.title = element_blank())  + 
  ylab('Daily reference rate') +
  labs(title = 'Rates of reference to 2019 NOVEL CORONAVIRUS',
       subtitle = 'Among US Senators & House Representatives')

covid_tweets %>%
  group_by(date, party, covid_gram) %>% #,party,   
  summarize(n = n()) %>%
  left_join(all_tweets) %>%
  mutate(per = n/ts) %>%
  
  #filter(date < '2020-3-27') %>% # & date < '2020-3-27'
  filter(party != 'Independent') %>%
  ggplot() +
  geom_line(aes(x = date, 
                y= per, 
                color = covid_gram
  ),
  size = 1.25) +
  theme_minimal() +
  ggthemes::scale_color_stata() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.title = element_blank(),
        legend.position = 'top')+
  scale_x_date(date_breaks = '1 week', date_labels = "%b %d") +
  facet_wrap(~party) + ylab('Daily reference rate') +
  labs(title = 'Rates of reference to 2019 NOVEL CORONAVIRUS by party affiliation',
       subtitle = 'Among US Senators & House Representatives')

#5) Probability Distribution
x1 <- covid_tweets %>%
  filter(date > '2020-2-25') %>%
  group_by(date, covid_gram) %>% #,party,   
  summarize(n = n()) %>%
  mutate(per = n/sum(n)) 

x2 <- x1 %>% 
  ggplot(aes(x=date, y=per, fill = covid_gram))+
  geom_bar(alpha = 0.65, stat = 'identity', width = .9) + #
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme(legend.position = "none")+
  ggthemes::scale_fill_economist() +
  scale_x_date(date_breaks = '1 day', date_labels = "%b %d") +
  labs(title = 'Referring to 2019 NOVEL CORONAVIRUS',
       subtitle = 'Among US Senators & House Representatives')

x2 +
  annotate(geom="text", 
           x = c(rep(as.Date('2020-3-22'), 4)), 
           y = c(.05, .35, .6, .8), 
           label = c('pandemic', 'covid19', 'coronavirus pandemic', 'coronavirus'),
           size = 4, color = 'black')
#3. Governmental Intervention 
suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(gghighlight)
  library(ggrepel)
  library(readr)
  library(stringr)
})


merged <- read_csv("https://joachim-gassen.github.io/data/merged_data_2020-03-27.csv", 
                   col_types = cols()) %>%
  mutate(date = ymd(date))

read_csv("https://joachim-gassen.github.io/data/npi_acaps_2020-03-27.csv",
         col_types = cols()) %>%
  mutate(npi_date = ymd(date_implemented)) %>%
  rename(npi_type = category) %>%
  mutate(
    npi_regional = !is.na(admin_level_name),
    npi_targeted_pop_group = targeted_pop_group == "Yes",
    npi_lockdown = str_detect(measure, "General lockdown")
  ) %>%
  select(iso3c, npi_date, npi_type, npi_regional, 
         npi_targeted_pop_group, npi_lockdown) -> npi

#Over Calender Time
ggplot(npi, aes(x = npi_date, fill = npi_type)) + 
  geom_bar(position = "stack") + theme_minimal() +
  labs(title = "Implementation of Interventions over Calendar Time",
       x = "Date",
       y = "Number of interventions")+labs(fill = "Intervention")

#Over Time
merged %>% 
  group_by(iso3c) %>%
  filter(deaths >= 10) %>%
  summarise(edate = min(date)) -> ctry_edate

merged %>%
  select(iso3c, country) %>%
  unique() -> ctry_names

npi %>%
  left_join(ctry_edate, by = "iso3c") %>%
  filter(!is.na(edate)) %>%
  mutate(npi_edate = as.numeric(npi_date - edate)) %>%
  left_join(ctry_names, by = "iso3c") %>%
  select(iso3c, country, npi_date, npi_edate, npi_type, npi_lockdown) -> npi_edates

lab_x <- "Days relative to the date where the number of deaths reached 10"

ggplot(npi_edates, aes(x = npi_edate, fill = npi_type)) + 
  geom_bar(position = "stack") + theme_minimal() +
  labs(title = "Implementation of Interventions over Time",
       x = lab_x,
       y = "Number of interventions") + labs(fill = "Intervention")

npi_edates %>%
  group_by(npi_edate, npi_type) %>%
  summarise(
    npi_count = n()
  ) %>%
  ungroup() %>%
  arrange(npi_type, npi_edate) %>%
  group_by(npi_type) %>%
  mutate(npi_count =  cumsum(npi_count)) %>%
  tidyr::complete(npi_edate = min(npi_edates$npi_edate):max(npi_edates$npi_edate)) %>%
  tidyr::fill(npi_count) %>% 
  tidyr::replace_na(list(npi_count = 0)) %>%
  ggplot(aes(x = npi_edate, fill = npi_type, y = npi_count)) +
  theme_minimal() + labs(
    x = lab_x,
    y = "Percentage of all interventions at event date",
    fill = "Intervention type"
  ) + 
  geom_area(position = "fill") + 
  scale_y_continuous(labels = scales::percent)

#4. Sentiment Analysis on Twitter & Reddit
#A Twitter
#1)install & load twitterR package
install.packages("twitteR")
install.packages('tidytext')

#Twitter Parse
library(tidytext)
library(ggplot2)
library(dplyr)
library(twitteR)
library(tidyverse)

library(remotes)
install_github("EmilHvitfeldt/textdata")
install_github("juliasilge/tidytext")

#2) load Twitter API
API_key <- 'ot3PamN0AcoAN8oCoQ1LDlVkz' 
API_secret_key <- 'Iej0ex4HlmxiKJhoadzOLdgpIkXwVMhm4BBWqwP7LGjlqj3YrJ'
access_token <- '1242263591996145664-m33ppD5nDmnrbgtx8wzbE92qLr58Sr'
access_token_secret <- '406EETPpoyVCqVT9FrhX1Ej7o7wKacaf728cpHrG3jZ56'
setup_twitter_oauth(API_key, API_secret_key, access_token, access_token_secret)


#3) sentimental analysis
#COVID-19
# import data
COVID_19 <- searchTwitter("COVID-19",n=5000,lang = "en")
COVID_19_df <- twListToDF(COVID_19) # Convert to data frame
tweet_words <- COVID_19_df %>% select(id, text) %>% unnest_tokens(word,text)

#First glance of raw data
tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")

#create a list of stop words: a list of words that aren't worth including
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("19", "t.co", "rt", "covid","que","de","el","https","amp",
                                "b.c","2,000","60","pandemic","coronavirus","covid19","1","2")))

#look for meaningful words
tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)
tweet_words_interesting %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word,n, function(n) -n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, 
                                                                                                                           hjust = 1)) + xlab("")

#conduct sentiment analysis
bing_lex <- get_sentiments("nrc")
fn_sentiment <- tweet_words_interesting %>% left_join(bing_lex)
fn_sentiment %>% filter(!is.na(sentiment)) %>% group_by(sentiment) %>% summarise(n=n())


#People's thoughts on Trump handling
# import data
Trump_19 <- searchTwitter(c("COVID19","Trump"),n=5000,lang = "en")
Trump_19_df <- twListToDF(Trump_19) # Convert to data frame
tweet_words <- Trump_19_df %>% select(id, text) %>% unnest_tokens(word,text)

#First glance of raw data
tweet_words %>% count(word,sort=T) %>% slice(1:20) %>% 
  ggplot(aes(x = reorder(word, n, function(n) -n), y = n)) + geom_bar(stat = "identity") + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) + xlab("")

#create a list of stop words: a list of words that aren't worth including
my_stop_words <- stop_words %>% select(-lexicon) %>% 
  bind_rows(data.frame(word = c("19", "t.co", "rt", "covid","que","de","el","https","amp",
                                "b.c","2,000","60","pandemic","coronavirus","covid19","1","2")))

#look for meaningful words
tweet_words_interesting <- tweet_words %>% anti_join(my_stop_words)
tweet_words_interesting %>% group_by(word) %>% tally(sort=TRUE) %>% slice(1:25) %>% 
  ggplot(aes(x = reorder(word,n, function(n) -n), y = n)) + geom_bar(stat = "identity") + theme(axis.text.x = element_text(angle = 60, 
                                                                                                                           hjust = 1)) + xlab("")

#conduct sentiment analysis
bing_lex <- get_sentiments("nrc")
fn_sentiment <- tweet_words_interesting %>% left_join(bing_lex)
fn_sentiment %>% filter(!is.na(sentiment)) %>% group_by(sentiment) %>% summarise(n=n())

#Reddit

# 5. Social Impacts
# A. Stock Market 
# Get quantmod
if (!require("quantmod")) {
  install.packages("quantmod")
  library(quantmod)
}

start <- as.Date("2020-01-01")
end <- as.Date("2020-04-26")

#Apple
getSymbols("AAPL", src = "yahoo", from = start, to = end)
plot(AAPL[, "AAPL.Close"], main = "AAPL")
candleChart(AAPL, up.col = "black", dn.col = "red", theme = "white")

#Alphabet 
getSymbols("GOOG", src = "yahoo", from = start, to = end)
plot(GOOG[, "GOOG.Close"], main = "GOOG")
candleChart(GOOG, up.col = "black", dn.col = "red", theme = "white")

#Delta
getSymbols("DAL", src = "yahoo", from = start, to = end)
plot(DAL[, "DAL.Close"], main = "Delta Airline")
candleChart(DAL, up.col = "black", dn.col = "red", theme = "white")

#Coca-Cola
getSymbols("KO", src = "yahoo", from = start, to = end)
plot(KO[, "KO.Close"], main = "Coca-Cola")
candleChart(KO, up.col = "black", dn.col = "red", theme = "white")

# B.Unemployeement Number
# load in data

unemp <- read.csv("unemployment.csv",stringsAsFactors = FALSE)
unemp <- unemp[,1:2]

# add color 
df=unemp
ylimit=100

for (i in 1:length(df$value)){
  if (df$value[i] <= 300){
    df$colors[i] <- c("orange")
  } else {
    df$colors[i] <- c("red")
  }
}

par(mar=c(5.1,4.1,4.1,2.1),mgp=c(3,1,0))

# add xlab
df <- df[order(as.Date(df$Date,"%Y-%m-%d")),]
df$year <- substring(df$Date,1,4)

# draw plot
df$value <- gsub(",","",df$value)
df$value <- as.numeric(as.character(df$value))
barplot(df$value, col = df$colors,arg=df$year,names.arg=df$year,
        main="",xlab="Date",
        ylab="US UI Claims per week (in thousands)",
        border = NA)
grid()

# add horizontal line
abline(h = 300,lty = 3)

# add text
mtext("Weekly change, seasonally adjusted", side = 3, line = 0, font = 1, cex = 1.1, col = "gray")
mtext("April 2019 - April 2020", side = 3, line = 1, font = 1, cex = 1.2, col = "black")
mtext("United States Employment Statistics", side = 3, line = 2, font = 2, cex = 1.2, col = "black")

# add a legend 
legend("topleft", c("Critical","Good"), col = c("red","orange"), pch = 16, bty = "n")

#5. Trend Analysis
library(gtrendsR)
library(tidyquant)

#Search for trend
grad_list <- gtrends(c("coronavirus",
                       "coronavirus pandemic",
                       "covid19",
                       "pandemic"),
                     gprop = "web",
                     geo = "US",
                     time = "today 3-m")
#Plot keyword trends
grad_list %>%
  pluck("interest_over_time") %>% 
  as_tibble() %>% drop_na() %>%
  mutate(hits = ifelse(grepl("<",hits),0,hits)) %>% 
  filter(!grepl("<",hits)) %>% 
  mutate(hits=as.numeric(hits)) %>% 
  ggplot(aes(date,hits,color=keyword)) +
  geom_line() +
  theme_tq(base_size=13) +
  scale_color_tq() + 
  labs(title="Keyword Trends Over Time") +
  xlab("Date") + ylab("Normalized Hits") +
  guides(colour = guide_legend(override.aes = list(size=3)))

#Add state info
states_tbl <- map_data("state") %>%
  mutate(region = str_to_title(region))

state_trends_tbl <- grad_list %>% 
  pluck("interest_by_region") %>% 
  left_join(states_tbl, by = c("location" = "region")) %>%
  as_tibble

state_trends_tbl %>% 
  ggplot(aes(long, lat)) +
  geom_polygon(aes(group = group, fill=hits)) +
  coord_map("albers",at0=45.5,lat1=29.5) +
  scale_fill_viridis_c() +
  scale_alpha(range=c(0.2, 0.4), guide=FALSE) +
  theme_tq() +
  facet_wrap(~keyword, ncol=2) +
  labs(title = "Keyword Trends US")

#Look for related topics
top_n_related_searches <- grad_list %>% 
  pluck('related_queries') %>% as_tibble() %>%
  filter(related_queries=="top") %>%
  filter(keyword == "coronavirus" | keyword=="covid19") %>% 
  mutate(interest = as.numeric(subject)) %>%
  select(keyword, value, interest) %>%
  group_by(keyword) %>%
  arrange(desc(interest)) %>%
  slice(1:9) %>%
  ungroup() %>% 
  mutate(value = as_factor(value) %>% fct_reorder(interest))

top_n_related_searches %>%
  ggplot(aes(x=value,y=interest,color=keyword)) +
  geom_segment(aes(xend = value, yend = 0)) +
  geom_point() + 
  coord_flip() +
  facet_wrap(~ keyword, scales="free_y",ncol=1) +
  theme_tq() + theme(axis.text=element_text(size=12)) +
  scale_color_manual(values=c("blue","orange"))
