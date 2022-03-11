################################################################################
# 03-table06.R
# Paper:    "Using Social Media Data to Reveal Patterns of Policy Engagement in 
#            State Legislatures"
# Journal:  State Politics and Policy Quarterly
# Authors:  Julia Payson, Andreu Casas, Jonathan Nagler, Richard Bonneau, and 
#           Joshua A. Tucker.
# Purpose:  To replicate Table 6 of the paper, showing the top distinctive
#           features of tweets predicted to be about each topic.
# Data In:  
#           1. A dataset containing a random sample of max 10,000 tweets that
#              have been predicted to be about each topic category
#              - ./data/sample-10000-legtweets.csv
# Output:
#           1. Latex code for generating Table 6
################################################################################


# PACAKGES
#===============================================================================
library(dplyr)
library(xtable)

# DATA
#===============================================================================
db <- read.csv("./data/sample-10000-legtweets.csv", colClasses = "character")

# MAIN
#===============================================================================
# - a list of stopwords to ignore. Either informative, or they appear across
#   topics
stopw <- c(
  "", "-", "the", "a", "…", "in", "to", "of", "and", "for", "is", "on",
  "at", "that", "with", "was", "has", "his", "an", "after", "this", "he",
  "rt", "as", "from", "who", "by", "it", "her", "are", "austin", "have", "i",
  "you", "be", "nt", "s", "its", "she", "not", "they", "what", "been", "about",
  "when", "will", "”", "do", "their", "alief", "were", "but", "just", "out", "two",
  "losaltos", "texashouse", "txsenateigr", "years", "we", "had", "up", "us", 
  "via", "more", "said", "there", "into", "houston",
  "how", "being", "no", "off", "than", "can", "him", "if", "txlege", "our",
  "my", "your", "texas", "__year__", "m", "tx", "donnahowardtx", "so",
  "thehill", "am", "ananavarro", "great", "w", "today", "de", "betoorourke",
  "\U0001f1f2", "\U0001f1fd", "malctx", "right", "now", "txdot", "ve", "get",
  "all", "one", "thank", "day", "or", "texasdemocrats", "senatorsylvia",
  "make", "should", "accdistrict", "here", "safoodbank", "texans",
  "__link__", "__num__", "trump", "president" # /!\ the show in all topics
)

# - a list of unique topics to analyze
topics <- unique(as.character(db$top_topic))

# - initialize empty output dataframe
outdb <- NULL

# - calculate in general how often they use all words
main_wordfreq <- data.frame(as.data.frame(table(strsplit(
  paste0(db$clean_text_cnn, collapse = " "), 
  split = " "))))

names(main_wordfreq) <- c("feature", "freq_main")
main_wordfreq <- main_wordfreq %>%
  # - remove empty spaces and stopwords
  filter(!(as.character(feature) %in% as.character(stopw)))
# - iterate through topics
for (topic in topics) {
  group_topic <- db %>%
    filter(top_topic == topic)
  if (nrow(group_topic) > 0) {
    topic_wordfreq <- data.frame(as.data.frame(table(strsplit(
      paste0(group_topic$clean_text_cnn, collapse = " "), 
      split = " "))))
    names(topic_wordfreq) <- c("feature", "freq_topic")
    topic_wordfreq <- topic_wordfreq %>%
      # - remove empty spaces and stopwords
      filter(!(as.character(feature) %in% as.character(stopw)))
    # - merge feature frequencies for all tweets and only tweets on this topic
    main_wordfreq$feature <- as.character(main_wordfreq$feature)
    topic_wordfreq$feature <- as.character(topic_wordfreq$feature)
    both_freq <- left_join(topic_wordfreq, main_wordfreq)
    # - calculate proportions: how often used in general v. this topic, and
    #   take the difference and pull most predictive feature for this topic
    top_features <-  both_freq %>%
      mutate(topic_prop = round(freq_topic / sum(both_freq$freq_topic), 4),
             main_prop = round(freq_main / sum(both_freq$freq_main), 4),
             # - take the difference to compare
             topic_diff = topic_prop - main_prop) %>%
      arrange(desc(topic_prop)) %>%
      head(15)
    top_features_str <- paste0(top_features$feature, collapse = ", ")
    # - save info to out-of-the-loop db
    new_row <- data.frame(
      topic = topic,
      top_features = top_features_str
    )
    outdb <- rbind(outdb, new_row)
  }
}

# - sort the topics 
outdb <- outdb %>%
  mutate(topic = factor(as.character(topic),
                        levels = c(
                          "No policy issue", 
                          "Gov. Operations",
                          "Healthcare",
                          "Intl. Affairs",
                          "Public Lands", 
                          "Labor",
                          "Law and Crime", 
                          "Defense",
                          "Immigration",
                          "Domestic Commerce",
                          "Civil Rights",
                          "Economy",
                          "Environment", 
                          "Transportation",
                          "Energy",
                          "Agriculture",
                          "Social Welfare",
                          "Education",
                          "Technology",
                          "Foreign Trade", 
                          "Housing"
                        ))) %>%
  arrange(topic)

# OUTPUT
#===============================================================================
print(xtable(outdb), include.rownames = FALSE)
