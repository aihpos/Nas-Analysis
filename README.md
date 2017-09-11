---
title: "NAS LYRIC ANALYSIS"
output: html_document
---

```{r actualcode, include=FALSE, eval=FALSE, echo = FALSE}
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

BASE_URL <- "api.genius.com/"
ACCESS_TOKEN <- "ygwgKtaPJEPG_1ge4GHb9Z5IWPWOT_CqeDCDORWqvTzZK4P8A-P6Lque_t-cyWhg"

COMPLETE_URL <- sprintf("%sartists/56/songs?per_page=50&sort=popularity&access_token=%s", BASE_URL, ACCESS_TOKEN)
songs <- GET(COMPLETE_URL) #stored as json
songs_data <-  fromJSON(content(songs, as = "text"))

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
nas_songs$releasedate <- as.Date("0000-00-00", "%Y-%m-%d")

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

for(x in 1:120){
  
  print(x)
  
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



```


### INTRODUCTION & BACKGROUND

Music has always been an integral part of my life. Although my taste throughout the years has changed, I've always appreciated hip hop for its lyricism. In college many of my friends exposed me to freestyle rapping and I began to get into old school rap. Nas has always been one of my favorite artists so I thought it would be cool to do an analysis of his lyrics and see how they've changed over time.

Before we start, some fun facts about Nas:
  1. He dropped out of school after the 8th grade
  2. His debut album Illmatic is commonly considered one of the best rap albums of all time
  3. Illmatic was recorded when Nas was 17-18 years old
  4. His full name is Nasir bin Olu Dara Jones and Harvard University has a Nasir Jones Hip-Hop Fellowship that funds students connected to hip hop
  5. He was suspended in 7th grade for stabbing a student with a pencil
  6. The original cover for Illmatic was a picture of Nas holding Jesus in a chokehold (scrapped and changed for obvious reasons)

### DATA SOURCE & DETAILS

I started this project off not really knowing what direction I wanted to go in. Originally, inspired by [these](http://people.ischool.berkeley.edu/~nikhitakoul/capstone/index.html) [cool](https://cdn.rawgit.com/eveskew/rap_album_sentiment_analysis/38f1827942a307e4ca23a7f96390128bb9f1063e/rap_album_sentiment_analysis.html) [projects](https://pudding.cool/2017/02/vocabulary/), I planned on analyzing metadata of various Nas songs to predict whether or not it would be popular. However, after spending hours just scraping the data I needed for the project and struggling with the Spotify API, I narrowed the scope of my analysis and simply look at the progression of his lyrics over time and through his most prolific albums.

All the data I needed came from the [Genius API](https://genius.com/developers). The way Genius stores its information, when you search an artist the API returns a maximum of 50 results. I decided to take the most popular 200 songs that came up under Nas, so I did the code below 4 times:

```{r nasstuff, eval = FALSE}
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
```

After retrieving all this data, there was a ton of entries of songs where Nas wasn't the main artist listed. I filtered these out and went from 200 entries to 120.

```{r filteringnas, eval = FALSE}
nas_songs <- subset(nas_songs, primaryartist == "Nas")
#down to 120 observations
```

The unfortunate part about using search results was that there was no album name or release date in the JSON information returned. I had to retrieve those separately with an individual song lookup API request. I struggled with this because when making a search API request, you simply had to input the request URL followed by the access token key. When I used this method with a song request, I got a 401: not authorized error.

Genius didn't provide an OAuth2 method for R so I hit a wall here. I ran into a similar problem while trying the Spotify API. After a lot of googling and research, I got my authentications to work:

```{r authentication, eval = FALSE}
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
```

With the authentication working, I pulled song data (specifically album names and release dates) for each of the 120 songs in my data frame with a simple for loop. For the actual lyrics I had to scrape them from the song page because Genius did not provide an API for song lyrics. The code I used to scrape lyrics from Genius was from [Roquentin on Github](https://github.com/Roquentin/R-apGeni-Us).

```{r albumreleasedate, eval = FALSE}
#create albumname and releasedate columns
nas_songs$albumname <- NA
nas_songs$releasedate <- release_date

for (x in 1:120){
  #for every row in the nas_songs table
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
```

It was at this point (11:19 PM the night before my project was due) that I realized compiling this table was probably 1/10th of the actual task I intended on accomplishing. I decided it would be cool to do a sentiment analysis on the songs and see if I could find any kind of interesting relationships or correlations between the main themes of different songs/albums.


### INITIAL EXPLORATORY ANALYSIS

I downloaded a sentiment data dictionary that was split into different "moods": anger, anticipation, disgust, fear, joy, litigious, negative, positive, sad, surprise, trust, and uncertainty. I figured this would be more interesting that just a positive and negative word score since Nas's songs tend to have important themes behind them. I added an example of the information pulled for the song "NY State of Mind" below:

```{r sentiments, eval = FALSE}
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
```


![Image](http://i66.tinypic.com/90aihi.png)
