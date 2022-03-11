################################################################################
# 01-table02.R
# Paper:    "Using Social Media Data to Reveal Patterns of Policy Engagement in 
#            State Legislatures"
# Journal:  State Politics and Policy Quarterly
# Authors:  Julia Payson, Andreu Casas, Jonathan Nagler, Richard Bonneau, and 
#           Joshua A. Tucker.
# Purpose:  To replicate Table 2 of the paper, describing the Twitter activity
#           of state legislators from 15 states, by state and party.
# Data In:  
#           1. A dataset with metadata for each state legislators included in
#              the analysis
#              - ./data/state-leg-indiv-level-meta-twitter-basics-2july2020.csv
#           2. A dataset with the key IDs for the tweets sent in 2018 by these
#              state legislators
#              - ./data/tweets-only-id-variables.csv
#           3. A dataset with information about when each state legislature was
#              in session in 2018
#              - ./data/state-legislatures-when-in-session-2018.csv
# Output:
#           1. Latex code for generating Table 2
################################################################################

# PACKAGES
#===============================================================================
# - uncomment the following lines if any of the required packages need to be 
#   installed
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("xtable")
library(dplyr)
library(ggplot2)
library(xtable)


# DATA
#===============================================================================
# - metadata for state legislators
leg_meta <- read.csv(
  "./data/state-leg-indiv-level-meta-twitter-basics-2july2020.csv",
  colClasses = "character")

# - metadat for the tweets sent in 2018
tweets <- read.csv("./data/tweets-only-id-variables.csv",
                   colClasses = "character")

# - information about when each state legislature was in session in 2018
in_session <- read.csv("./data/state-legislatures-when-in-session-2018.csv")


# DATA WRANGLING
#===============================================================================
# - exclude those state legislators elected in the mid-2018 elections (this 
#   dataset already includes info for those who started serving in 2019, which
#   are not part of the analysis). Also, give 0 tweets to those that have an
#   account but for which we have no tweet.
tab_out_pre <- leg_meta %>%
  filter(updated2019 != 1) %>%
  mutate(tweets_n = ifelse(Twitter != "" & is.na(tweets_n), 0, tweets_n))

# - calculate some statistics by state and party
tab_out <- tab_out_pre %>%
  group_by(State, party) %>%
  summarise(n = n(),
            legtwitter_n = length(which(Twitter != "")),
            legtwitter_prop = round(legtwitter_n / n, 2),
            tweets_total = sum(as.numeric(tweets_n), na.rm = TRUE),
            daily_avg = round((sum(as.numeric(tweets_n), na.rm = TRUE) / 
                                 legtwitter_n) / 365, 2))

# - count tweets only while in session. Making sure first that the merging
#   variables (state and dates) are all in the right/same format.
tweets$date <- as.Date(tweets$date)
in_session <- in_session %>%
  rename(State = state) %>%
  mutate(State = as.character(State))
in_session$convenes <- as.character(
  sapply(as.character(in_session$convenes), function(x)
  ifelse(x != "no session", 
         paste0(
           strsplit(x, split = "-")[[1]][3], "-", 
           strsplit(x, split = "-")[[1]][1], "-",
           strsplit(x, split = "-")[[1]][2]),
         "no session")))
in_session$adjourns <- as.character(
  sapply(as.character(in_session$adjourns), function(x)
  ifelse(x != "no session", 
         paste0(
           strsplit(x, split = "-")[[1]][3], "-", 
           strsplit(x, split = "-")[[1]][1], "-",
           strsplit(x, split = "-")[[1]][2]),
         "no session")))

in_session <- in_session %>%
  mutate(convenes = ifelse(convenes == "no session",
                           "2017-01-01",
                           as.character(convenes)),
         adjourns = ifelse(adjourns == "no session",
                           "2017-01-01",
                           as.character(adjourns)))

# - creating a crosswalk table for the merging legislators metadata, tweet
#   metadata, and info about when legislatures in session
twitter_cross <- leg_meta %>%
  dplyr::select(Twitter, user_id, State, party) %>%
  filter(Twitter != "",
         !is.na(user_id))

# - merging tweet and legislature level info, so we can easily identify which
#   tweets were sent while in session
twitter_cross$user_id <- as.character(twitter_cross$user_id)
tweets$user_id <- as.character(tweets$user_id)
tweets02 <- left_join(tweets, twitter_cross)
tweets02 <- tweets02[!duplicated(tweets02$tweet_id),]

tweets02$State <- as.character(tweets02$State)
in_session$State <- as.character(in_session$State)

tweets03 <- left_join(tweets02, in_session) %>%
  mutate(date = as.Date(date),
         convenes = as.Date(convenes),
         adjourns = as.Date(adjourns)) # N = 599,203

# - select only those sent while in session
tweets_insession <- tweets03 %>%
  filter(date >= convenes & date <= adjourns) # N = 320,090

# - the legislators for which we have in session tweets
tab_out02_pre01 <- tweets_insession %>%
  group_by(user_id) %>%
  summarise(Twitter = Twitter[1],
            State = State[1],
            party = party[1],
            tweets_n = n())

# - the info for the remaining legislators (for which the legislatures didn't 
#   meet at all that year)
tab_out02_pre02 <- tab_out_pre %>%
  filter(Twitter != "") %>%
  dplyr::select(user_id, Twitter, State, party, tweets_n) %>%
  filter(!(as.character(user_id) %in% as.character(tab_out02_pre01$user_id))) %>%
  mutate(tweets_n = 0)

# - merging the tweet counts for legislators for which the legislature meet v.
#   didn't meet in 2018
tab_out02_pre <- rbind(tab_out02_pre01, tab_out02_pre02)

# - aggregating the counts at the state/party level -- distinguishing again btw
#   those for which the legislature met v not.
tab_out02a <- tab_out02_pre %>%
  group_by(State, party) %>%
  summarise(tweets_total_insession = sum(tweets_n),
            n = n())

tab_out02b <- in_session %>%
  mutate(days_in_session = as.numeric(as.Date(in_session$adjourns, format = "%Y-%m-%d")  - 
                                        as.Date(in_session$convenes, format = "%Y-%m-%d"))) %>%
  dplyr::select(State, days_in_session)

tab_out02a$State <- as.character(tab_out02a$State)
tab_out02b$State <- as.character(tab_out02b$State)
tab_out02c <- left_join(tab_out02a, tab_out02b) %>%
  mutate(daily_avg_insession = round(
    (tweets_total_insession / n) / days_in_session, 2)) %>%
  dplyr::select(-n, -days_in_session)

# - merge the tweeting rate while in session to the rest of the states calculated
#   in the beginning by state and party
tab_out$State <- as.character(tab_out$State)
tab_out$party <- as.character(tab_out$party)
tab_out02c$State <- as.character(tab_out02c$State)
tab_out02c$party <- as.character(tab_out02c$party)

tab_final <- left_join(tab_out, tab_out02c) %>%
  mutate(tweets_total = format(tweets_total, big.mark = ","))


# MAIN
#===============================================================================
# - generating the final table to include in the paper (Table 2)
names(tab_final) <- c(
  "State", "Party", "Legislators\n(N)", "Legislators on\nTwitter (N)",
  "Legislators on\nTwitter (Prop.)", "Total\nTweets '18",
  "Daily Avg.\nTweets '18", "tweets_total_insession", "Daily Avg.\nwhile In\nSession '18"
)

tab_final <- tab_final %>%
  dplyr::select(-tweets_total_insession)

total_row <- data.frame(
  `State` = "Total", 
  `Party` = "", 
  `Legislators\n(N)` = sum(tab_final$`Legislators\n(N)`),
  `Legislators on\nTwitter (N)` = sum(tab_final$`Legislators on\nTwitter (N)`),
  `Legislators on\nTwitter (Prop.)` = round(sum(tab_final$`Legislators on\nTwitter (N)`) / 
                                              sum(tab_final$`Legislators\n(N)`), 2),
  `Total\nTweets '18` = format(sum(as.numeric(
    gsub(",", "", tab_final$`Total\nTweets '18`))), big.mark = ","),
  `Daily Avg.\nTweets '18` = round(sum(tab_final$`Legislators on\nTwitter (N)` * 
                                         tab_final$`Daily Avg.\nTweets '18`) / sum(tab_final$`Legislators on\nTwitter (N)`), 2),
  `Daily Avg.\nwhile In\nSession '18` = round(sum(
    tab_final$`Legislators on\nTwitter (N)`[
      which(!is.na(tab_final$`Daily Avg.\nwhile In\nSession '18`))] * 
      tab_final$`Daily Avg.\nwhile In\nSession '18`[
        which(!is.na(tab_final$`Daily Avg.\nwhile In\nSession '18`))]) / 
      sum(tab_final$`Legislators on\nTwitter (N)`[
        which(!is.na(tab_final$`Daily Avg.\nwhile In\nSession '18`))]), 2)
)

names(total_row) <- c(
  "State", "Party", "Legislators\n(N)", "Legislators on\nTwitter (N)",
  "Legislators on\nTwitter (Prop.)", "Total\nTweets '18",
  "Daily Avg.\nTweets '18", "Daily Avg.\nwhile In\nSession '18"
)

final <- rbind(tab_final %>% as.data.frame(), total_row %>% as.data.frame())
final$`Daily Avg.\nwhile In\nSession '18`[
  which(is.na(final$`Daily Avg.\nwhile In\nSession '18`))] <- "-"


# - print latex code
library(xtable)
print(xtable(final, digits = c(0, 0,0,0,0,2,0,2,2)), include.rownames = FALSE)

