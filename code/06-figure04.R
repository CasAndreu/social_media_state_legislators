################################################################################
# 06-figure04.R
# Paper:    "Using Social Media Data to Reveal Patterns of Policy Engagement in 
#            State Legislatures"
# Journal:  State Politics and Policy Quarterly
# Authors:  Julia Payson, Andreu Casas, Jonathan Nagler, Richard Bonneau, and 
#           Joshua A. Tucker.
# Purpose:  To replicate Figure 4 of the paper, showing Proportion of attention 
#           that legislators from each state devoted to each issue area in 2018
# Data In:  
#           1. Dataset with data on tweets per topic by legislator 
#              - ./data/legislators_topic_tweet_count-25june2020.csv
# Output:
#           1. Figure 4 of the paper
#              - ./figures/figure04.pdf
################################################################################

# PACAKGES
#===============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)

# DATA
#===============================================================================
# - dataset with data on tweets per topic by legislator 
leg_counts <- read.csv("./data/legislators_topic_tweet_count-25june2020.csv",
                       colClasses = "character")


# DATA WRANGLING
#===============================================================================
leg_toprop <- leg_counts %>%
  dplyr::select(-X0) %>%
  gather(topic_code, tweets_n, -user_id, -party, -state, -chamber) %>%
  mutate(tweets_n = as.numeric(as.character(tweets_n))) %>%
  group_by(state, topic_code) %>%
  summarise(n = sum(tweets_n))

leg_toprop_totals <- leg_counts %>%
  dplyr::select(-X0) %>%
  gather(topic_code, tweets_n, -user_id, -party, -state, -chamber) %>%
  mutate(tweets_n = as.numeric(as.character(tweets_n))) %>%
  group_by(state) %>%
  summarise(total_tweets = sum(tweets_n))

leg_toprop02 <- left_join(leg_toprop, leg_toprop_totals) %>%
  mutate(prop = round(n / total_tweets, 3)) %>%
  as.data.frame() %>%
  mutate(state = as.character(state))

# - human-readable labels for the topics
leg_toprop03 <- leg_toprop02 %>%
  mutate(topic_label = recode(topic_code,
                              `X0` = "No Policy Issue",
                              `X1` = "Economy",
                              `X2` = "Law & Crime",
                              `X3` = "Defense",
                              `X4` = "Technology",
                              `X5` = "Foreign Trade",
                              `X6` = "Intl. Affairs",
                              `X7` = "Govt. Operations",
                              `X8` = "Public Lands",
                              `X9` = "Gun Control",
                              `X10` = "Civil Rights",
                              `X12` = "Healthcare",
                              `X13` = "Agriculture",
                              `X14` = "Labor",
                              `X15` = "Education",
                              `X16` = "Environment",
                              `X17` = "Energy",
                              `X18` = "Immigration",
                              `X19` = "Transportation",
                              `X20` = "Social Welfare",
                              `X21` = "Housing",
                              `X25` = "Domestic Commerce"
  ))

# - final dataset for the plot
comp_plotdb <- leg_toprop03 %>%
  dplyr::select(state, prop, topic_label) %>%
  gather(leg, prop, -state, -topic_label)  %>%
  mutate(leg = recode(leg,
                      `prop` = "State Legislators"),
         prop_log = log((prop*100)+1)) %>%
  # Only keep the ones about specific policy areas
  filter(topic_label != "No Policy Issue") %>%
  mutate(topic_label = factor(
    topic_label, 
    levels = rev(c(
      "Govt. Operations", "Economy", "Healthcare", "Law & Crime", "Civil Rights",
      "Defense", "Domestic Commerce", "Education", "Labor", "Environment",
      "Energy", "Immigration", "Intl. Affairs", "Agriculture", "Transportation",
      "Public Lands", "Technology", "Social Welfare", "Housing", "Foreign Trade"
    ))))


# MAIN -- OUTPUT
#===============================================================================
pdf("./figures/figure04.pdf", width = 8, height = 6)
ggplot(comp_plotdb,
       aes(x = state, y = as.numeric(topic_label))) +
  geom_tile(aes(fill = prop_log), color = "black") +
  geom_text(aes(x = state, y = as.numeric(topic_label), label = prop * 100),
            color = "white", size = 2.5) +
  scale_fill_gradient(low = "gray80", high = "gray10", guide = FALSE) +
  scale_y_continuous("",breaks = seq(1, length(levels(comp_plotdb$topic_label))),
                     labels = levels(comp_plotdb$topic_label),
                     limits = c(0.5, length(levels(comp_plotdb$topic_label))+0.5),
                     expand = c(0,0),
                     sec.axis = sec_axis(~.,
                                         breaks = seq(1, length(levels(comp_plotdb$topic_label))),
                                         labels = levels(comp_plotdb$topic_label))) +
  scale_x_discrete("") +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 12),
        panel.background = element_blank())
dev.off()
