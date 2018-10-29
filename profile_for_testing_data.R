library(rtweet)
create_token(app_name<-"app_for_duke_homework",
             consumer_key<-"YhPY9yIwObIZ3nkP1McTKa81i",
             consumer_secret<-"Tj88D18byxuTjjz3o1SOTwvAe3AINLsWLhHWWIgwkTCG22lrzX",
             access_token<-"1036759768659120128-OkzXcGDWCtvGuVPJDa4qA7KAx815Wz",
             access_token_secret<-"yoJUB2soEDLHNvRcwc6a0qljlRhNBBWUG1GEtB47jtGC2")

vaccine_tweet <- read.csv("/Users/xuanyu/Desktop/MIDS courses/text_tendency_analysis/group work/test.csv", stringsAsFactors = F)

#profile_data <- as.data.frame(NULL)
rate <- rate_limit()
for (i in 1:nrow(vaccine_tweet)){
  out <- tryCatch({
    profile <- lookup_users(as.character(vaccine_tweet$screen_name[i]))
  },
  error = function(msg) {
    message("Original error message:")
    message(paste0(msg,"\n"))
    return(NA)
  }) 
  if (nrow(profile) == 0 || is.na(out[1])){
    next
  }
  profile_data <- rbind(profile_data, profile)
  if (rate$remaining[11] == 0){
    Sys.sleep(900)
  }
  print(i)
}

df <- apply(profile_data,2,as.character)
write.csv(df, file = "vaccine_profile.csv")


vaccine_profile <- read.csv("/Users/xuanyu/Desktop/MIDS courses/text_tendency_analysis/group work/vaccine_profile.csv")

geocoded <- lat_lng(vaccine_profile)
library(maps)
par(mar = c(0, 0, 0, 0))
maps::map("state", lwd = .25)
with(geocoded, points(lng, lat, pch = 20, cex = .75, col = rgb(0, .3, .7, .75)))

sum(is.na(geocoded$lat))

#other
vaccine_tweet$retweet_count[vaccine_tweet$retweet_count > 5000] <- 0

hist(vaccine_tweet$retweet_count, breaks=20)

#location
library(tidytext)
library(dplyr)
vac_tweets<- vaccine_tweet %>%
  select(created_at, location) %>%
  unnest_tokens("word", location)

vac_tweets %>%
  count(word) %>%
  arrange(desc(n))

tidy_vac_DTM<-
  vac_tweets %>%
  count(created_at, word) %>%
  cast_dtm(created_at, word, n)

inspect(tidy_vac_DTM[1:5,3:8])

data("stop_words")

vac_tweets_top_words<-
  vac_tweets %>%
  anti_join(stop_words) %>%
  count(word) %>%
  arrange(desc(n))


vac_tweets_top_words<-
  vac_tweets_top_words[-grep("https|t.co|amp|rt",
                             vac_tweets_top_words$word),]

top_20<-vac_tweets_top_words[1:20,]

#create factor variable to sort by frequency
vac_tweets_top_words$word <- factor(vac_tweets_top_words$word, levels = vac_tweets_top_words$word[order(vac_tweets_top_words$n,decreasing=TRUE)])


library(ggplot2)
ggplot(top_20, aes(x=reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylab("Number of Times Word Appears in Location")+
  xlab("")+
  guides(fill=FALSE)
