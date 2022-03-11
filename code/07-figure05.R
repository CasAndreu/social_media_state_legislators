################################################################################
# 07-figure05.R
# Paper:    "Using Social Media Data to Reveal Patterns of Policy Engagement in 
#            State Legislatures"
# Journal:  State Politics and Policy Quarterly
# Authors:  Julia Payson, Andreu Casas, Jonathan Nagler, Richard Bonneau, and 
#           Joshua A. Tucker.
# Purpose:  To replicate Figure 5 of the paper, Comparing attention by 
#           representatives in Congress and State Legislators
# Data In:  
#           1. Dataset with data on tweets per topic by legislator 
#              - ./data/legislators_topic_tweet_count-25june2020.csv
#           2. Dataset with data on tweets per topic by members of Congress 
#              - ./data/memberscongress_topic_tweet_count-25june2020.csv
# Output:
#           1. Figure 4 of the paper
#              - ./figures/figure04.pdf
################################################################################

# PACAKGES
#===============================================================================
library(dplyr)
library(tidyr)
library(gridExtra)
library(ggplot2)


# DATA
#===============================================================================
# - dataset with data on tweets per topic by legislator 
leg_counts <- read.csv("./data/legislators_topic_tweet_count-25june2020.csv",
                       colClasses = "character")

# - dataset with data on tweets per topic by members of Congress
mc_counts <- read.csv("./data/memberscongress_topic_tweet_count-25june2020.csv",
                      colClasses = "character")


# DATA WRANGLING
#===============================================================================
# - calculating prop. of attention paid to each topic, by state legislators as
#   well as by members of congress
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

# - merging to state-level covariates
state_covstomerge <- state_covs %>%
  dplyr::select(-state) %>%
  rename(state = abbr) %>%
  mutate(state = as.character(state))

# - the same for tweets from members of Congress
mc_toprop <- mc_counts %>%
  dplyr::select(-X0) %>%
  gather(topic_code, tweets_n, -user_id, -party, -state, -chamber) %>%
  mutate(tweets_n = as.numeric(as.character(tweets_n))) %>%
  group_by(state, topic_code) %>%
  summarise(n = sum(tweets_n))

mc_toprop_totals <- mc_counts %>%
  dplyr::select(-X0) %>%
  gather(topic_code, tweets_n, -user_id, -party, -state, -chamber) %>%
  mutate(tweets_n = as.numeric(as.character(tweets_n))) %>%
  group_by(state) %>%
  summarise(total_tweets = sum(tweets_n))

mc_toprop02 <- left_join(mc_toprop, mc_toprop_totals) %>%
  mutate(prop = round(n / total_tweets, 3)) %>%
  as.data.frame() %>%
  mutate(state = as.character(state)) %>%
  rename(mc_n = n, mc_total_tweets = total_tweets, mc_prop = prop)


leg_toprop03 <- left_join(leg_toprop02,  mc_toprop02) %>%
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
  dplyr::select(state, prop, mc_prop, topic_label) %>%
  gather(leg, prop, -state, -topic_label)  %>%
  mutate(leg = recode(leg,
                      `prop` = "State Legislators",
                      `mc_prop` = "Members of Congress"),
         prop_log = log((prop*100)+1)) %>%
  arrange(prop) %>%
  # Only keep the ones about specific policy areas
  filter(topic_label != "No Policy Issue") %>%
  mutate(topic_label = factor(topic_label, levels = unique(topic_label)))  

# - type of issues (federal, state, both)
issue_type <- comp_plotdb %>%
  dplyr::select(topic_label) %>%
  mutate(type = ifelse(
    topic_label %in% c("Govt. Operations",
                       "Economy",
                       "Civil Rights",
                       "Public Lands",
                       "Environment",
                       "Energy",
                       "Agriculture",
                       "Immigration"),
    "Both",
    ifelse(topic_label %in% c("Intl. Affairs",
                              "Defense",
                              "Domestic Commerce",
                              "Technology",
                              "Foreign Trade"),
           "Federal", "State")
  )) %>%
  mutate(topic_label = factor(topic_label, levels = levels(comp_plotdb$topic_label)),
         type_color = recode(type,
                             `Both` = "gray50",
                             `Federal` = "plum3",
                             `State` = "darkolivegreen3")) %>%
  arrange(topic_label) %>%
  mutate(type_color = factor(type_color, levels = unique(type_color))) %>%
  unique()


# - calculating final proportions for both groups and arranging these to be
#   plotted
comp_plotdb02 <- leg_toprop03 %>%
  dplyr::select(state, n, mc_n, topic_label) %>%
  group_by(topic_label) %>%
  summarise(stateleg = sum(n),
            mc = sum(mc_n))

totals <- data.frame(
  state_total = sum(comp_plotdb02$stateleg),
  mc_total = sum(comp_plotdb02$mc)
)

comp_plotdb02final <- comp_plotdb02 %>%
  filter(topic_label != "No Policy Issue") %>%
  mutate(state_prop = round(stateleg / totals$state_total, 3),
         mc_prop = round(mc / totals$mc_total, 3),
         type = ifelse(
           topic_label %in% c("Govt. Operations",
                              "Economy",
                              "Civil Rights",
                              "Public Lands",
                              "Environment",
                              "Energy",
                              "Agriculture",
                              "Immigration"),
           "Both",
           ifelse(topic_label %in% c("Intl. Affairs",
                                     "Defense",
                                     "Domestic Commerce",
                                     "Technology",
                                     "Foreign Trade"),
                  "Federal", "State"))) %>%
  mutate(type = factor(type, levels = rev(c("Both", "State", "Federal")))) %>%
  arrange(type, state_prop) %>%
  mutate(topic_label = factor(topic_label, levels = unique(topic_label))) %>%
  gather(group, prop, -topic_label, -stateleg, -mc, -type) %>%
  mutate(group = recode(group,
                        `state_prop` = "State Legislators",
                        `mc_prop` = "Members of Congress"))

# MAIN
#===============================================================================
# - the figure combines the following two plots (p1 and p2)
p1 <- ggplot(comp_plotdb02final,
             aes(x = topic_label, y = prop, fill = type)) +
  geom_bar(stat = "identity", position = "dodge",
           aes(alpha = group)) +
  coord_flip() +
  scale_x_discrete("") +
  scale_fill_manual("Issue domain of:",
                    values = c("plum3", "darkolivegreen3",  "gray50"),
                    guide = FALSE) +
  scale_y_continuous("\nPercentage of substantive tweets on each policy area",
                     breaks = seq(0, 1, 0.05),
                     labels = paste0(seq(0, 100, 5), "%")) +
  scale_alpha_discrete("", range = c(0.5, 1), guide = FALSE) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10))

# - the same info but taking the diff between MCs' and StateLeg's att.
comp_plotdb03final <- comp_plotdb02final %>%
  mutate(group = ifelse(grepl("State", group), "state_leg", "mc")) %>%
  spread(group, prop) %>%
  mutate(diff = state_leg - mc)

p2 <- ggplot(comp_plotdb03final %>%
               arrange(diff) %>%
               mutate(topic_label = factor(topic_label, levels = unique(topic_label))),
             aes(x = topic_label, y = diff, fill = type)) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  scale_x_discrete("") +
  scale_fill_manual("Issue domain of:",
                    values = c("plum3", "darkolivegreen3",  "gray50")) +
  scale_y_continuous("How much more/less attention State Legislators paid\n to each topic compared to Members of Congress",
                     breaks = seq(-0.035, 0.065, 0.0175),
                     labels = paste0(seq(-3.5, 6.5, 1.75), "%")) +
  scale_alpha_discrete("", range = c(0.5, 1), guide = FALSE) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        axis.text.x = element_text(size = 8),
        axis.title.x = element_text(size = 10))

# OUTCOME
#===============================================================================
pdf("./figures/figure05.pdf", width = 12, height = 6,)
gridExtra::grid.arrange(p1, p2, nrow = 1)
dev.off()
