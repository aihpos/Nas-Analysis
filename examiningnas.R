###################################################################
#      Sophia Dong
#      iX Data Science Analysis Project
#      Examining the evolution of Nas
###################################################################

library(httr)
library(jsonlite)
library(XML)
library(dplyr)
library(ROAuth)
library(magrittr)
library(rvest)
library(tidytext)
library(stringr)
library(readr)
library(ggplot2)
library(xlsx)
library(scales)

BASE_URL <- "api.genius.com/"
ACCESS_TOKEN <- "ygwgKtaPJEPG_1ge4GHb9Z5IWPWOT_CqeDCDORWqvTzZK4P8A-P6Lque_t-cyWhg"

#Information we want to compile into a table:
# Song Name
# Main Artist ID
# Link to Lyrics (must scrape)
# Album

#testing access

#  -----------------------------------------------------------------------------------
# FIRST PAGE OF DATA  ----------------------------------------------------------------
#  -----------------------------------------------------------------------------------

#stores entire contents of returned query in artist_data
COMPLETE_URL <- sprintf("%sartists/56/songs?per_page=50&sort=popularity&access_token=%s", BASE_URL, ACCESS_TOKEN)
songs <- GET(COMPLETE_URL) #stored as json
songs_data <-  fromJSON(content(songs, as = "text"))
View(songs_data)

songs_data$response$songs$full_title

#create new data frame with needed attributes
songname <- as.data.frame(songs_data$response$songs$title)
songid <- as.data.frame(songs_data$response$songs$id)
primaryartist <- as.data.frame(songs_data$response$songs$primary_artist$name)

nas_songs_p1 <- cbind(songname, songid)
nas_songs_p1 <- cbind(nas_songs_p1, primaryartist)

#rename columns to something pretty
colnames(nas_songs_p1) <- c("songname", "songid", "primaryartist")


#  -----------------------------------------------------------------------------------
# SECOND PAGE OF DATA ----------------------------------------------------------------
#  -----------------------------------------------------------------------------------

COMPLETE_URL <- sprintf("%sartists/56/songs?per_page=50&sort=popularity&page=2&access_token=%s", BASE_URL, ACCESS_TOKEN)
songs <- GET(COMPLETE_URL) #stored as json
songs_data <-  fromJSON(content(songs, as = "text"))

songname <- as.data.frame(songs_data$response$songs$title)
songid <- as.data.frame(songs_data$response$songs$id)
primaryartist <- as.data.frame(songs_data$response$songs$primary_artist$name)

nas_songs_p2 <- cbind(songname, songid)
nas_songs_p2 <- cbind(nas_songs_p2, primaryartist)
colnames(nas_songs_p2) <- c("songname", "songid", "primaryartist")


#  -----------------------------------------------------------------------------------
# THIRD PAGE OF DATA -----------------------------------------------------------------
#  -----------------------------------------------------------------------------------

COMPLETE_URL <- sprintf("%sartists/56/songs?per_page=50&sort=popularity&page=3&access_token=%s", BASE_URL, ACCESS_TOKEN)
songs <- GET(COMPLETE_URL) #stored as json
songs_data <-  fromJSON(content(songs, as = "text"))

songname <- as.data.frame(songs_data$response$songs$title)
songid <- as.data.frame(songs_data$response$songs$id)
primaryartist <- as.data.frame(songs_data$response$songs$primary_artist$name)

nas_songs_p3 <- cbind(songname, songid)
nas_songs_p3 <- cbind(nas_songs_p3, primaryartist)
colnames(nas_songs_p3) <- c("songname", "songid", "primaryartist")


#  -----------------------------------------------------------------------------------
# FOURTH PAGE OF DATA ----------------------------------------------------------------
#  -----------------------------------------------------------------------------------

COMPLETE_URL <- sprintf("%sartists/56/songs?per_page=50&sort=popularity&page=4&access_token=%s", BASE_URL, ACCESS_TOKEN)
songs <- GET(COMPLETE_URL) #stored as json
songs_data <-  fromJSON(content(songs, as = "text"))

songname <- as.data.frame(songs_data$response$songs$title)
songid <- as.data.frame(songs_data$response$songs$id)
primaryartist <- as.data.frame(songs_data$response$songs$primary_artist$name)

nas_songs_p4 <- cbind(songname, songid)
nas_songs_p4 <- cbind(nas_songs_p4, primaryartist)
colnames(nas_songs_p4) <- c("songname", "songid", "primaryartist")


#  -----------------------------------------------------------------------------------
# FILTERING OUT NON-NAS SONGS --------------------------------------------------------
#  -----------------------------------------------------------------------------------

nas_songs <- rbind(nas_songs_p1, nas_songs_p2)
nas_songs <- rbind(nas_songs, nas_songs_p3)
nas_songs <- rbind(nas_songs, nas_songs_p4)

nas_songs <- subset(nas_songs, primaryartist == "Nas")
#down to 120 observations


#  -----------------------------------------------------------------------------------
# ADDING SONG INFO -------------------------------------------------------------------
#  -----------------------------------------------------------------------------------

#need to get album name and release date

CLIENTKEY <- "7OZKNq-48rmrZChCVZClS-b0rvBHtE-fBEIK1PkrLtfU8Er4GpYbYWW5bmKUl1c1"
SECRETKEY <- "7McYG2X8OfGhBZRJr5kMI4Bq5D3imb_56JdegGQRDn-nsZDJAiKIQUSNS_8jaYCmE4AVzM5OPR7iRbKVetYO1g"

response = POST(
  'https://api.genius.com/oauth/token ',
  accept_json(),
  authenticate(CLIENTKEY, SECRETKEY),
  body = list(grant_type = 'client_credentials'),
  encode = 'form',
  verbose()
)
token = content(response)$access_token
authorization.header = paste0("Bearer ", token)

ACCESSTOKEN <- "GZKx3B3pWuvWn-tAVrqonDcZuvRAxXKIQBvB8MJoNlBKmd2AjGn1EKuavTtMmFtf"

#create albumname and releasedate columns
nas_songs$albumname <- NA
nas_songs$releasedate <- release_date

for (x in 1:120){
  #for every row in the nas_songs table
  print(x)
  songinfo <- GET(url = paste("https://api.genius.com/songs/", nas_songs[x,2], sep = ""),
                  config = add_headers(authorization = authorization.header))
  songinfo_data <-  fromJSON(content(songinfo, as = "text"))
  album_name <- songinfo_data$response$song$album$full_title
  print(album_name)
  
  if (is.null(songinfo_data$response$song$release_date)){
    release_date <- as.Date("0000-00-00", "%Y-%m-%d")
  }
  else{
    release_date <- as.Date(songinfo_data$response$song$release_date, "%Y-%m-%d") #saved as date variable
  }
  if (is.null(album_name)){
    album_name <- "None"
  }
  
  nas_songs[x,4] <- album_name
  nas_songs[x,5] <- release_date
  
}

# It was at this point (11:19PM the night before this project was due) that I realized that compiling this table
# wasn't nearly as helpful as I hoped it would be. All good, I'll try something new. In an effort to make the past
# hour of work useful, I pulled what seemed to be Nas's most popular albums:
# Illmatic, Stillmatic, It Was Written, Life Is Good, I Am..., and God's Son.

ILLMATIC <- subset(nas_songs, albumname == "Illmatic by Nas")
STILLMATIC <- subset(nas_songs, albumname == "Stillmatic by Nas")
ITWASWRITTEN <- subset(nas_songs, albumname == "It Was Written by Nas")
LIFEISGOOD <- subset(nas_songs, albumname == "Life Is Good by Nas")
IAMIAMIAM <- subset(nas_songs, albumname == "I Am... by Nas")
GODSSON <- subset(nas_songs, albumname == "God's Son by Nas")

#  -----------------------------------------------------------------------------------
# SENTIMENT ANALYSIS -----------------------------------------------------------------
#  -----------------------------------------------------------------------------------

my_access_token <- "ygwgKtaPJEPG_1ge4GHb9Z5IWPWOT_CqeDCDORWqvTzZK4P8A-P6Lque_t-cyWhg"

lyrics <- genius_lyrics(artist = "Nas", song = ILLMATIC[1,1], access_token = my_access_token)
lyrics <- select(lyrics, line, line_num)

lyrics_unneest <- unnest_tokens(lyrics, text, line)


lyrics_sentiment <- inner_join(lyrics_unneest, sentiments, by = c("text" = "word"))

lyrics_sentiment %>% count(text, sentiment, sort = TRUE) %>%
  group_by(sentiment) %>%
  top_n(5) %>%
  ungroup %>%
  mutate(text = reorder(text, n)) %>%
  ggplot(aes(x = text, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~ sentiment, scales = "free_y") +
  coord_flip() +
  labs(x = "", y = "Sentiment count") +
  theme(legend.position = "none")

#generate new columns for nas songs word sentiments
nas_songs$wordcount <- 0
nas_songs$anger <- 0
nas_songs$anticipation <- 0
nas_songs$disgust <- 0
nas_songs$fear <- 0
nas_songs$joy <- 0
nas_songs$litigious <- 0
nas_songs$negative <- 0
nas_songs$positive <- 0
nas_songs$sad <- 0
nas_songs$surprise <- 0
nas_songs$trust <- 0
nas_songs$uncertain <- 0
nas_songs$na <- 0

#count the number of each sentiment to the total number of words
lyrics_sentiment %>% count(text, sentiment)
words.total <- nrow(lyrics_sentiment)
nas_songs[1,6] <- words.total

lyrics.anger <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "anger")
words.anger <- nrow(lyrics.anger)
nas_songs[1,7] <- words.anger

lyrics.anticipation <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "anticipation")
words.anticipation <- nrow(lyrics.anticipation)
nas_songs[1,8] <- words.anticipation

lyrics.disgust <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "disgust")
words.disgust <- nrow(lyrics.disgust)
nas_songs[1,9] <- words.disgust

lyrics.fear <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "fear")
words.fear <- nrow(lyrics.fear)
nas_songs[1,10] <- words.fear

lyrics.joy <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "joy")
words.joy <- nrow(lyrics.joy)
nas_songs[1,11] <- words.joy

lyrics.litigious <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "litigious")
words.litigious <- nrow(lyrics.litigious)
nas_songs[1,12] <- words.litigious

lyrics.negative <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "negative")
words.negative <- nrow(lyrics.negative)
nas_songs[1,13] <- words.negative

lyrics.positive <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "positive")
words.positive <- nrow(lyrics.positive)
nas_songs[1,14] <- words.positive

lyrics.sad <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "sadness")
words.sad <- nrow(lyrics.sad)
nas_songs[1,15] <- words.sad

lyrics.surprise <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "surprise")
words.surprise <- nrow(lyrics.surprise)
nas_songs[1,16] <- words.surprise

lyrics.trust <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "trust")
words.trust <- nrow(lyrics.trust)
nas_songs[1,17] <- words.trust

lyrics.uncertain <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "uncertainty")
words.uncertain <- nrow(lyrics.uncertain)
nas_songs[1,18] <- words.uncertain

lyrics.na <- filter(lyrics_sentiment, is.na(lyrics_sentiment$sentiment))
words.na <- nrow(lyrics.na)
nas_songs[1,19] <- words.na

#View(lyrics.na)

for(x in 1:120){
  
  #try it for the next 10 songs
  
  lyrics <- genius_lyrics(artist = "Nas", song = nas_songs[x,1], access_token = my_access_token)
  lyrics <- select(lyrics, line, line_num)
  lyrics_unneest <- unnest_tokens(lyrics, text, line)
  lyrics_sentiment <- inner_join(lyrics_unneest, sentiments, by = c("text" = "word"))
  
  
  
  
  
  lyrics_sentiment %>% count(text, sentiment)
  words.total <- nrow(lyrics_sentiment)
  nas_songs[x,6] <- words.total
  
  lyrics.anger <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "anger")
  words.anger <- nrow(lyrics.anger)
  nas_songs[x,7] <- words.anger
  
  lyrics.anticipation <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "anticipation")
  words.anticipation <- nrow(lyrics.anticipation)
  nas_songs[x,8] <- words.anticipation
  
  lyrics.disgust <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "disgust")
  words.disgust <- nrow(lyrics.disgust)
  nas_songs[x,9] <- words.disgust
  
  lyrics.fear <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "fear")
  words.fear <- nrow(lyrics.fear)
  nas_songs[x,10] <- words.fear
  
  lyrics.joy <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "joy")
  words.joy <- nrow(lyrics.joy)
  nas_songs[x,11] <- words.joy
  
  lyrics.litigious <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "litigious")
  words.litigious <- nrow(lyrics.litigious)
  nas_songs[x,12] <- words.litigious
  
  lyrics.negative <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "negative")
  words.negative <- nrow(lyrics.negative)
  nas_songs[x,13] <- words.negative
  
  lyrics.positive <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "positive")
  words.positive <- nrow(lyrics.positive)
  nas_songs[x,14] <- words.positive
  
  lyrics.sad <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "sadness")
  words.sad <- nrow(lyrics.sad)
  nas_songs[x,15] <- words.sad
  
  lyrics.surprise <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "surprise")
  words.surprise <- nrow(lyrics.surprise)
  nas_songs[x,16] <- words.surprise
  
  lyrics.trust <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "trust")
  words.trust <- nrow(lyrics.trust)
  nas_songs[x,17] <- words.trust
  
  lyrics.uncertain <- filter(lyrics_sentiment, lyrics_sentiment$sentiment == "uncertainty")
  words.uncertain <- nrow(lyrics.uncertain)
  nas_songs[x,18] <- words.uncertain
  
  lyrics.na <- filter(lyrics_sentiment, is.na(lyrics_sentiment$sentiment))
  words.na <- nrow(lyrics.na)
  nas_songs[x,19] <- words.na
  
  
  
}


#sent.anger, sent.anticipation, sent.disgust, sent.fear, sent.joy, sent.litig
#sent.negative, sent.positive, sent.sad, sent.surprise, sent.trust, sent.uncertain, sent.na

#looking at specific albums

# ILLMATIC

ill.word <- sum(ILLMATIC$wordcount)
ill.anger <- sum(ILLMATIC$anger) / sum(ILLMATIC$wordcount)
ill.ant <- sum(ILLMATIC$anticipation) / sum(ILLMATIC$wordcount)
ill.disgust <- sum(ILLMATIC$disgust) / sum(ILLMATIC$wordcount)
ill.fear <- sum(ILLMATIC$fear) / sum(ILLMATIC$wordcount)
ill.joy <- sum(ILLMATIC$joy) / sum(ILLMATIC$wordcount)
ill.litig <- sum(ILLMATIC$litigious) / sum(ILLMATIC$wordcount)
ill.neg <- sum(ILLMATIC$negative) / sum(ILLMATIC$wordcount)
ill.pos <- sum(ILLMATIC$positive) / sum(ILLMATIC$wordcount)
ill.sad <- sum(ILLMATIC$sad) / sum(ILLMATIC$wordcount)
ill.surprise <- sum(ILLMATIC$surprise) / sum(ILLMATIC$wordcount)
ill.trust <- sum(ILLMATIC$trust) / sum(ILLMATIC$wordcount)
ill.uncertain <- sum(ILLMATIC$uncertain) / sum(ILLMATIC$wordcount)
ill.na <- sum(ILLMATIC$na) / sum(ILLMATIC$wordcount)

ill.chart <- data.frame(
  feeling = c("anger", "anticipation", "disgust", "fear", "joy", "litigious", "negative", "positive", "sadness", "surprise", "trust", "uncertainty", "NA"),
  value = c(ill.anger, ill.ant, ill.disgust, ill.fear, ill.joy, ill.litig, ill.neg, ill.pos, ill.sad, ill.surprise, ill.trust, ill.uncertain, ill.na)
)

library(reshape2)
library(HH)

chart<- ggplot(ill.chart, aes(x="", y=value, fill=feeling))+
  geom_bar(width = 1, stat = "identity") +
  theme_light()
chart

# melt() and dcast() with reshape2 package
melted <- melt(my.df,id.var="id", na.rm=TRUE)
summd <- dcast(data=melted,variable~value, length) # note: length()
# not robust if NAs present

# give names to cols and rows for likert() to use
names(summd) <- c("Question", "strongly disagree", 
                  "disagree", 
                  "agree", 
                  "strongly agree")
rownames(summd) <- summd[,1]  # question number as rowname
summd[,1] <- NULL             

# plot
likert(summd,
       as.percent=TRUE,       # automatically scales
       main = NULL,           # or give "title",
       xlab = "Percent",      # label axis
       positive.order = TRUE, # orders by furthest right
       ReferenceZero = 2.5,   # zero point btwn levels 2&3
       ylab = "Question",     # label for left side
       auto.key = list(space = "right", columns = 1,
                       reverse = TRUE)) # make positive items on top of legend







write.csv(nas_songs, file = "C:/Users/Sophia/Documents/nassongs.csv")

#profanity
