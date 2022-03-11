################################################################################
# 05-figure03.R
# Paper:    "Using Social Media Data to Reveal Patterns of Policy Engagement in 
#            State Legislatures"
# Journal:  State Politics and Policy Quarterly
# Authors:  Julia Payson, Andreu Casas, Jonathan Nagler, Richard Bonneau, and 
#           Joshua A. Tucker.
# Purpose:  To replicate Figure 3 of the paper, showing the results OLS models 
#           predicting the proportion of tweets legislators dedicate to 
#           discussing each topic as a function of being on a committee on the topic
# Data In:  
#           1. Core data needed for the data modeling
#              - ./data/model_data_basic.csv
#           2. Dataset with data on tweets per topic by legislator 
#              - ./data/legislators_topic_tweet_count-25june2020.csv
#           3. Additional meta info about state leg
#              - ./data/state-leg-indiv-level-meta-twitter-basics-2july2020.csv
#           4. Information about the topics of the committee they serve
#              - ./data/committee_topic.csv
# Output:
#           1. Figure 3 of the paper
#              - ./figures/figure03.pdf
################################################################################

# PACAKGES
#===============================================================================
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(broom)
library(sandwich)
library(lmtest)

# DATA
#===============================================================================
# - dataset with data on tweets per topic by legislator 
leg_counts <- read.csv("./data/legislators_topic_tweet_count-25june2020.csv",
                       colClasses = "character")

# - additional meta info about state leg
meta <- read.csv("./data/state-leg-indiv-level-meta-twitter-basics-2july2020.csv",
                 colClasses = "character")

# - core data needed for the data modeling
model_data <- read.csv("./data/model_data_basic.csv")

# - information about the topic of the committee in which they serve
committee_topic <- read.csv("./data/committee_topic.csv", 
                            colClasses = "character")

# DATA WRANGLING
#===============================================================================
# - generating information about how often each legislators discussed each issue
indiv_legdb <- leg_counts %>%
  # /!\ exclude those elected in the mid-2018 election (they didn't start until
  #     2019)
  filter(as.character(user_id) %in%
           as.character(meta$user_id[which(meta$updated2019 != 1)])) %>%
  gather(topic_code, tweets_n, -user_id, -party, -state, -chamber) %>%
  mutate(tweets_n = as.numeric(as.character(tweets_n)))

# - calculate prop. of tweets by each legislator that are about each topic
indiv_totals <- leg_counts %>%
  # /!\ exclude those elected in the mid-2018 election (they didn't start until
  #     2019)
  filter(as.character(user_id) %in%
           as.character(meta$user_id[which(meta$updated2019 != 1)])) %>%
  gather(topic_code, tweets_n, -user_id, -party, -state, -chamber) %>%
  group_by(user_id) %>%
  summarise(leg_total = sum(as.numeric(as.character(tweets_n))))

indiv_legdb$user_id <- as.character(indiv_legdb$user_id)
indiv_totals$user_id <- as.character(indiv_totals$user_id)
indiv_legdb02 <- left_join(indiv_legdb, indiv_totals) %>%
  mutate(tweets_prop = round(tweets_n / leg_total, 4))

topic_props <- indiv_legdb02 %>%
  filter(topic_code != "X0") %>%
  filter(topic_code != "X9") %>%
  mutate(topic_label = recode(topic_code,
                              `X1` = "Economy_prop",
                              `X2` = "Law_Crime_prop",
                              `X3` = "Defense_prop",
                              `X4` = "Science_Tech_prop",
                              `X5` = "Foreign_Trade_prop",
                              `X6` = "International_Affairs_prop",
                              `X7` = "Government_Operations_prop",
                              `X8` = "Public_Lands_Water_prop",
                              `X10` = "Civil_Rights_prop",
                              `X12` = "Health_prop",
                              `X13` = "Agriculture_prop",
                              `X14` = "Labor_prop",
                              `X15` = "Education_prop",
                              `X16` = "Environment_prop",
                              `X17` = "Energy_prop",
                              `X18` = "Immigration_prop",
                              `X19` = "Transportation_prop",
                              `X20` = "Social_Welfare_prop",
                              `X21` = "Housing_prop",
                              `X25` = "Finance_Commerce_prop")) %>%
  dplyr::select(user_id, topic_label, tweets_prop) %>%
  spread(topic_label, tweets_prop)

# - preparing all other necessary covariates for the model
model_data <- model_data %>%
  mutate(on_twitter = ifelse(Twitter != "", 1, 0),
         professional = factor(professional, 
                               levels = c("Least Professional",
                                          "Middle-ground",
                                          "Most Professional")),
         election18 = factor(election18,
                             levels = c("No election",
                                        "Safe election",
                                        "Competitive election")),
         ethnicity = factor(ethnicity, 
                            levels = c("white", "black", "hispanic", 
                                       "asian", "other_ethn")),
         in_session = ifelse(State %in% c("MT", "ND", "NV", "TX"), 0, 1),
         mov02_log = log(mov02 + 1),
         democrat = ifelse(grepl("Dem", Party), 1, 0))

# - adding user ids to the model data so it can be merged to the topic data
#   (getting rid of info for those who are not on Twitter, as they are excluded
#    from this analysis)
ids_cross <- meta %>% 
  dplyr::select(Twitter, user_id) %>%
  mutate(Twitter = tolower(Twitter))

model_data$Twitter <- tolower(model_data$Twitter)

mdata <- left_join(model_data, ids_cross) %>%
  filter(!is.na(user_id))
mdata <- mdata[!duplicated(mdata$user_id),]

# - merging with the rest of key covariates for the models
topic_props$user_id <- as.character(topic_props$user_id)
mdata$user_id <- as.character(mdata$user_id)
final_mdata <- left_join(mdata, topic_props)

# - adding finally information about the topic of the committee in which they
#   serve
final_mdata <- left_join(final_mdata, committee_topic)

#-------------------------------------------------------------------------------
# Model D: Talk about a given topic
#-------------------------------------------------------------------------------
model_d <- NULL
topics <- names(final_mdata)[54:73]
for (topic in topics) {
  model_formula <- formula(
    paste0(topic, "_prop ~legprofscore_sd + in_session + has_term_limits + leadership + 
    committee.count_sd + seniority_sd + last_term + ethnicity + 
    male + mov02_sd + democrat +", topic))
  # - fit the model
  model_d_fit <- lm(model_formula, data = final_mdata)
  # - cluster SE by state
  model_d_cerr <- coeftest(model_d_fit, vcov = vcovCL, cluster = ~State)
  #model_d_run <- broom::tidy(lm(model_formula, data = model_data04))
  coef_interest <- broom::tidy(model_d_cerr) %>%
    filter(term == paste0(topic, "1")) %>%
    mutate(term = gsub("1", "", term))
  coef_interest$intercept <- (broom::tidy(model_d_cerr) %>%
                                filter(term == "(Intercept)"))$estimate
  
  model_d <- rbind(model_d, coef_interest)
}

# OUTPUT
#-------------------------------------------------------------------------------
# - calculate 95% CIs
model_d02 <- model_d %>%
  mutate(lwr_pre = estimate - (1.96 * std.error),
         upr_pre = estimate + (1.96 * std.error),
         pe = (intercept + estimate) / intercept,
         lwr = (intercept + lwr_pre) / intercept,
         upr = (intercept + upr_pre) / intercept) %>%
  arrange(desc(pe)) %>%
  rename(v = term) %>%
  mutate(v = factor(v,  levels = v))


pdf("./figures/figure03.pdf", width = 7, height = 5)
ggplot(model_d02 %>%
         filter(!(v %in% c("Housing"))) %>%
         mutate(v = gsub("_", " ", v),
                significant = ifelse((lwr > 1 & upr > 1) |
                                       (lwr < 1 & upr < 1), 
                                     "p.value < 0.05", "p.value >= 0.05")) %>%
         arrange(pe) %>%
         mutate(v = factor(v, levels = unique(v))),
       aes(x = v, y = pe)) +
  geom_point(aes(x = v, y = pe, shape = significant),
             size = 3) +
  coord_flip() +
  geom_hline(yintercept = 1, color = "red") +
  scale_shape_manual("", values = c(17, 1)) +
  scale_x_discrete("") +
  scale_y_continuous(
    "\nLikelihood of discussing if legislator on a on-topic committee",
    breaks = seq(0, 7, 1), 
    labels = paste0("x", seq(0, 7, 1))) +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        legend.position = "bottom",
        legend.text = element_text(size = 8),
        axis.title.x = element_text(size = 9))
dev.off()        
