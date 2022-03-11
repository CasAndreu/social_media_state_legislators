################################################################################
# 02-table05.R
# Paper:    "Using Social Media Data to Reveal Patterns of Policy Engagement in 
#            State Legislatures"
# Journal:  State Politics and Policy Quarterly
# Authors:  Julia Payson, Andreu Casas, Jonathan Nagler, Richard Bonneau, and 
#           Joshua A. Tucker.
# Purpose:  To replicate Table 5 of the paper, describing the performance of the
#           topic classifier by topic.
# Data In:  
#           1. A dataset containing true v. predicted topics for the messages in
#              the test set.
#              - ./data/final-LEG-model-acc-bytopic-VALSET.csv
# Output:
#           1. Latex code for generating Table 5
################################################################################

# PACKAGES
#===============================================================================
library(dplyr)
library(tidyr)
library(xtable)

# DATA
#===============================================================================
# - load true labels and model predictions for Legislators validation set
#   N = 930
leg_db <- read.csv("./data/final-LEG-model-acc-bytopic-VALSET.csv")


# DATA WRANGLING
#===============================================================================
# - calculate accuracy measures by topic and by model
acc_db <- NULL
topics <- unique(leg_db$Y_val)
leg_db <- leg_db %>%
  mutate(correct = as.numeric(Y_val == ccn_prediction))
for (topic in topics) {
  pred0 <- leg_db %>% filter(Y_val == topic)
  pred1 <- leg_db %>% filter(ccn_prediction == topic)
  class_prop <- round(nrow(pred0) / nrow(leg_db), 2)
  accuracy <- round(sum(pred0$correct) / nrow(pred0), 2)
  precision <- round(sum(pred1$correct) / nrow(pred1), 2)
  recall <- round(sum(pred0$correct) / nrow(pred0), 2)
  fscore <- 2 * ((precision * recall) / (precision + recall))
  new_row <- data.frame(
    group = "Stage Legislators",
    topic, class_prop, accuracy, precision, recall, fscore
  )
  acc_db <- rbind(acc_db, new_row)
}


# MAIN
#===============================================================================
# - give human readable labels to topic codes
acc_db <- acc_db %>%
  mutate(topic_label = recode(topic,
                              `0` = "No Policy Issue",
                              `1` = "Economy",
                              `2` = "Civil Rights",
                              `3` = "Health",
                              `4` = "Agriculture",
                              `5` = "Labor",
                              `6` = "Education",
                              `7` = "Environment", 
                              `8` = "Energy",
                              `9` = "Immigration",
                              `10` = "Transportation", 
                              `11` = "Law & Crime", 
                              `12` = "Social Welfare", 
                              `13` = "Housing", 
                              `14` = "Domestic Commerce", 
                              `15` = "Defense", 
                              `16` = "Technology", 
                              `17` = "Foreign Trade", 
                              `18` = "Intl. Affairs", 
                              `19` = "Govt. Operations", 
                              `20` = "Public Lands",
                              `21` = "Gun Control"),
         topic_label = factor(topic_label, levels = c(
           "No Policy Issue",
           "Economy",
           "Civil Rights",
           "Health",
           "Agriculture",
           "Labor",
           "Education",
           "Environment", 
           "Energy",
           "Immigration",
           "Transportation", 
           "Law & Crime", 
           "Social Welfare", 
           "Housing", 
           "Domestic Commerce", 
           "Defense", 
           "Technology", 
           "Foreign Trade", 
           "Intl. Affairs", 
           "Govt. Operations", 
           "Public Lands",
           "Gun Control"
         ))) %>%
  arrange(desc(class_prop))

# - version of the table for the first LFS paper
acc_db01 <- acc_db %>%
  # - "Gun Control" excluded from analysis. "Foreign Trade" excluded from table
  #   because we only have 1 labeled doc -- not enough really to assess accuracy
  filter(!topic_label %in% c("Gun Control", "Foreign Trade")) %>%
  dplyr::select(topic_label, class_prop, accuracy, fscore)


library(xtable)
print(xtable(acc_db01), include.rownames = FALSE)
