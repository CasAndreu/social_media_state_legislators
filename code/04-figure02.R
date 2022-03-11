################################################################################
# 04-figure02.R
# Paper:    "Using Social Media Data to Reveal Patterns of Policy Engagement in 
#            State Legislatures"
# Journal:  State Politics and Policy Quarterly
# Authors:  Julia Payson, Andreu Casas, Jonathan Nagler, Richard Bonneau, and 
#           Joshua A. Tucker.
# Purpose:  To replicate Figure 2 of the paper, showing the results for three 
#           statistical models predicting Being on Twitter, Being Active, and
#           Discussing Policy Issues.
# Data In:  
#           1. Core data needed for the data modeling
#              - ./data/model_data_basic.csv
#           2. Dataset with data on tweets per topic by legislator 
#              - ./data/legislators_topic_tweet_count-25june2020.csv
#           3. Additional meta info about state leg
#              - ./data/state-leg-indiv-level-meta-twitter-basics-2july2020.csv
# Output:
#           1. Figure 2 of the paper
#              - ./figures/figure02.pdf
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
# - core data needed for the data modeling
model_data <- read.csv("./data/model_data_basic.csv")

# - dataset with data on tweets per topic by legislator 
leg_counts <- read.csv("./data/legislators_topic_tweet_count-25june2020.csv",
                       colClasses = "character")

# - additional meta info about state leg
meta <- read.csv("./data/state-leg-indiv-level-meta-twitter-basics-2july2020.csv",
                 colClasses = "character")


# DATA WRANGLING
#===============================================================================
# - adding some additional variables to the model data, and preparing the 
#   dataset needed to fit of the models in Fig.2

# - dataset for model 1
model_data01 <- model_data %>%
  mutate(on_twitter = ifelse(Twitter != "", 1, 0),
         ethnicity = factor(ethnicity, 
                            levels = c("white", "black", "hispanic", 
                                       "asian", "other_ethn")),
         in_session = ifelse(State %in% c("MT", "ND", "NV", "TX"), 0, 1),
         democrat = ifelse(grepl("Dem", Party), 1, 0))

# - dataset for model 2
twitter_tomerge <- meta %>%
  dplyr::select(Twitter, user_id, tweets_n) %>%
  filter(Twitter != "")

twitter_tomerge$Twitter <- tolower(as.character(twitter_tomerge$Twitter))
model_data$Twitter <- tolower(as.character(model_data$Twitter))
model_data02 <- left_join(model_data, twitter_tomerge) %>%
  mutate(on_twitter = ifelse(Twitter != "", 1, 0),
         ethnicity = factor(ethnicity, 
                            levels = c("white", "black", "hispanic", 
                                       "asian", "other_ethn")),
         in_session = ifelse(State %in% c("MT", "ND", "NV", "TX"), 0, 1),
         # - if have an account but we don't have any tweet == 0
         tweets_n = ifelse(Twitter != "" & is.na(tweets_n), 0,
                           as.numeric(tweets_n)),
         tweets_log = log(tweets_n + 1),
         mov02_log = log(mov02 + 1),
         democrat = ifelse(grepl("Dem", Party), 1, 0))

# - dataset for model 3
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

policy_prop <- indiv_legdb02 %>%
  filter(topic_code == "X0") %>%
  mutate(policy_prop = 1 - tweets_prop) %>%
  dplyr::select(user_id, policy_prop)

policy_prop$user_id <- as.character(policy_prop$user_id)
model_data02$user_id <- as.character(model_data02$user_id)
model_data03 <- left_join(model_data02, policy_prop) %>%
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

# MAIN
#===============================================================================

#-------------------------------------------------------------------------------
# Model A: Pr of being on Twitter
#-------------------------------------------------------------------------------
# - First regression: logistic with state-level covariates and no state fixed 
#                     effects of MLM. Using LegProf and not Staff.
model_a <- glm(on_twitter ~ 
                 # state-level covariates
                 legprofscore_sd + 
                 #staff_sd +
                 in_session + has_term_limits +
                 # individual-level covariates
                 leadership + committee.count_sd + seniority_sd + last_term +
                 ethnicity + male + mov02_sd + democrat, 
               data = model_data01, 
               family = binomial(link = "logit"))

# ... state-clustered SEs
model_a_cerr <- coeftest(model_a, vcov = vcovCL, cluster = ~State)

# - Second regression: logistic with state-level covariates and no state fixed 
#                      effects o4 MLM. Using Staff instead of LegProf
model_a_staff <- glm(on_twitter ~ 
                       # state-level covariates
                       staff_sd +
                       in_session + has_term_limits +
                       # individual-level covariates
                       leadership + committee.count_sd + seniority_sd + last_term +
                       ethnicity + male + mov02_sd + democrat, 
                     data = model_data01, 
                     family = binomial(link = "logit"))

# ... state-clustered SEs
model_astaff_cerr <- coeftest(model_a_staff, vcov = vcovCL, cluster = ~State)

# - Third regression: MLM version. Dropping state-level covaraites
model_aMLM <- glmer(on_twitter ~ 
                      # individual-level covariates
                      leadership + committee.count_sd + seniority_sd + last_term +
                      ethnicity + male + mov02_sd + democrat +
                      (1 | State),
                    data = model_data01, 
                    family=binomial(link="logit"))


#-------------------------------------------------------------------------------
# Model B: How active they are on Twitter
#-------------------------------------------------------------------------------
# - First regression: linear model with state-level covariates and no state fixed 
#                     effects or MLM. Using LegProf and not Staff.
model_b <- lm(tweets_log ~ 
                legprofscore_sd + 
                in_session + has_term_limits +
                leadership + committee.count_sd + seniority_sd + last_term +
                ethnicity + male + mov02_sd + democrat,
              data = model_data02)

# ... state-clustered SEs
model_b_cerr <- coeftest(model_b, vcov = vcovCL, cluster = ~State)

# - Second regression: linear model with state-level covariates and no state fixed 
#                     effects or MLM. Using Staff instead of LegProf
model_b_staff <- lm(tweets_log ~ 
                      staff_sd +
                      in_session + has_term_limits +
                      leadership + committee.count_sd + seniority_sd + last_term +
                      ethnicity + male + mov02_sd + democrat,
                    data = model_data02)

# ... state-clustered SEs
model_bstaff_cerr <- coeftest(model_b_staff, vcov = vcovCL, cluster = ~State)

# - Third regression: MLM version. Dropping state-level covaraites
model_bMLM <- lmer(tweets_log ~ 
                     # individual-level covariates
                     leadership + committee.count_sd + seniority_sd + last_term +
                     ethnicity + male + mov02_sd + democrat +
                     (1 | State),
                   data = model_data02)

#-------------------------------------------------------------------------------
# Model C: Prop of tweets about policy topics
#-------------------------------------------------------------------------------
# - First regression: linear model with state-level covariates and no state fixed 
#                     effects or MLM. Using LegProf and not Staff.
model_c <- lm(policy_prop ~ 
                legprofscore_sd +
                in_session + has_term_limits +
                leadership + committee.count_sd + seniority_sd + last_term +
                ethnicity + male + mov02_sd + democrat,
              data = model_data03)

# ... state-clustered SEs
model_c_cerr <- coeftest(model_c, vcov = vcovCL, cluster = ~State)

# - Second regression: linear model with state-level covariates and no state fixed 
#                     effects or MLM. Using Staff instead of LegProf
model_c_staff <- lm(policy_prop ~ 
                      staff_sd +
                      in_session + has_term_limits +
                      leadership + committee.count_sd + seniority_sd + last_term +
                      ethnicity + male + mov02_sd + democrat,
                    data = model_data03)

# ... state-clustered SEs
model_cstaff_cerr <- coeftest(model_c_staff, vcov = vcovCL, cluster = ~State)

# - Third regression: MLM version. Dropping state-level covaraites
model_cMLM <- lmer(policy_prop ~ 
                     # individual-level covariates
                     leadership + committee.count_sd + seniority_sd + last_term +
                     ethnicity + male + mov02_sd + democrat +
                     (1 | State),
                   data = model_data03)



# COEFFICIENT PLOTS
#-------------------------------------------------------------------------------
# MODEL 1: logistic regression. Expressing coefficients as change in likelihood

# ... function to pull coef table for lmer models
coeftable_lmer <- function(model) {
  Vcov <- vcov(model, useScale = FALSE)
  betas <- as.numeric(coef(model)$State[1,])
  se <- sqrt(diag(Vcov))
  zval <- betas / se
  pval <- 2 * pnorm(abs(zval), lower.tail = FALSE)
  out <- as.data.frame(cbind(betas, se, zval, pval))
  out <- out %>%
    mutate(term = colnames(coef(model)$State)) %>%
    rename(estimate = betas,
           std.error = se,
           statistic = zval,
           p.value = pval) 
  return(out)
}

# - combining the information from the 3 model versions into a single dataframe
model_1 <- rbind(
  tidy(model_a_cerr) %>% mutate(model = "Being on Twitter", 
                                model_type = "LegProf"),
  tidy(model_astaff_cerr) %>% mutate(model = "Being on Twitter", 
                                     model_type = "Staff"),
  coeftable_lmer(model_aMLM) %>% mutate(model = "Being on Twitter", 
                                        model_type = "MLM")
)

# - calculating lwr and upr 95% CIs, expressing point estimates and CIs in 
#   likelihood change
model_1 <- model_1 %>%
  mutate(lwr_pre = estimate - (1.96 * std.error),
         upr_pre = estimate + (1.96 * std.error),
         pe = (exp(estimate) / (1 + exp(estimate))) /
           (1 - (exp(estimate) / (1 + exp(estimate)))),
         lwr = (exp(lwr_pre) / (1 + exp(lwr_pre))) /
           (1 - (exp(lwr_pre) / (1 + exp(lwr_pre)))),
         upr = (exp(upr_pre) / (1 + exp(upr_pre))) /
           (1 - (exp(upr_pre) / (1 + exp(upr_pre)))))


# MODEL 2: linear model. Expressing coefficients as change in likelihood

# - calculating lwr and upr 95% CIs, expressing point estimates and CIs in 
#   likelihood change. For each model version separately (diff intercepts), and
#   then merging them all into a single dataset.
model_b02 <- tidy(model_b_cerr) %>% mutate(model = "Being Active: Num. Tweets (log)", 
                                           model_type = "LegProf")
model_b02_int <- model_b02$estimate[which(model_b02$term == "(Intercept)")]
model_b02 <- model_b02 %>%
  mutate(lwr_pre = estimate - (1.96 * std.error),
         upr_pre = estimate + (1.96 * std.error),
         pe = (model_b02_int + estimate) / model_b02_int,
         lwr = (model_b02_int + lwr_pre) / model_b02_int,
         upr = (model_b02_int + upr_pre) / model_b02_int)

model_b_staff02 <- tidy(model_bstaff_cerr) %>% mutate(model = "Being Active: Num. Tweets (log)", 
                                                      model_type = "Staff")
model_b02staff_int <- model_b_staff02$estimate[which(model_b_staff02$term == "(Intercept)")]
model_b_staff02 <- model_b_staff02 %>%
  mutate(lwr_pre = estimate - (1.96 * std.error),
         upr_pre = estimate + (1.96 * std.error),
         pe = (model_b02staff_int + estimate) / model_b02staff_int,
         lwr = (model_b02staff_int + lwr_pre) / model_b02staff_int,
         upr = (model_b02staff_int + upr_pre) / model_b02staff_int)


model_bMLM02 <- coeftable_lmer(model_bMLM) %>% 
  mutate(model = "Being Active: Num. Tweets (log)", 
         model_type = "MLM")
model_bMLM02_int <- 3.99478
model_bMLM02 <- model_bMLM02 %>%
  mutate(lwr_pre = estimate - (1.96 * std.error),
         upr_pre = estimate + (1.96 * std.error),
         pe = (model_bMLM02_int + estimate) / model_bMLM02_int,
         lwr = (model_bMLM02_int + lwr_pre) / model_bMLM02_int,
         upr = (model_bMLM02_int + upr_pre) / model_bMLM02_int)

model_2 <- rbind(model_b02, model_b_staff02, model_bMLM02)


# MODEL 3: linear model. Expressing coefficients as change in likelihood

# - calculating lwr and upr 95% CIs, expressing point estimates and CIs in 
#   likelihood change. For each model version separately (diff intercepts), and
#   then merging them all into a single dataset.
model_c02 <- tidy(model_c_cerr) %>% mutate(model = "Discussing Policy Issues", 
                                           model_type = "LegProf")
model_c02_int <- model_c02$estimate[which(model_c02$term == "(Intercept)")]
model_c02 <- model_c02 %>%
  mutate(lwr_pre = estimate - (1.96 * std.error),
         upr_pre = estimate + (1.96 * std.error),
         pe = (model_c02_int + estimate) / model_c02_int,
         lwr = (model_c02_int + lwr_pre) / model_c02_int,
         upr = (model_c02_int + upr_pre) / model_c02_int)

model_c_staff02 <- tidy(model_cstaff_cerr) %>% mutate(model = "Discussing Policy Issues", 
                                                      model_type = "Staff")
model_c02staff_int <- model_c_staff02$estimate[which(model_c_staff02$term == "(Intercept)")]
model_c_staff02 <- model_c_staff02 %>%
  mutate(lwr_pre = estimate - (1.96 * std.error),
         upr_pre = estimate + (1.96 * std.error),
         pe = (model_c02staff_int + estimate) / model_c02staff_int,
         lwr = (model_c02staff_int + lwr_pre) / model_c02staff_int,
         upr = (model_c02staff_int + upr_pre) / model_c02staff_int)


model_cMLM02 <- coeftable_lmer(model_cMLM) %>% 
  mutate(model = "Discussing Policy Issues", 
         model_type = "MLM")
model_cMLM02_int <- 0.6513732
model_cMLM02 <- model_cMLM02 %>%
  mutate(lwr_pre = estimate - (1.96 * std.error),
         upr_pre = estimate + (1.96 * std.error),
         pe = (model_cMLM02_int + estimate) / model_cMLM02_int,
         lwr = (model_cMLM02_int + lwr_pre) / model_cMLM02_int,
         upr = (model_cMLM02_int + upr_pre) / model_cMLM02_int)

model_3 <- rbind(model_c02, model_c_staff02, model_cMLM02)

# - combine information for all versions of the 3 models
all_models <- rbind(model_1, model_2, model_3) %>%
  mutate(predictors = recode(
    term,
    `legprofscore_sd` = "Legislative professionalization",
    `has_term_limits` = "Legislature w. Term Limits",
    `leadership` = "Leadership",
    `democrat` = "Democrat",
    `staff_sd` = "Staff",
    `last_term` = "Last term",
    `in_session` = "Legislature in session '18",
    `committee.count_sd` = "Number of committees",
    `seniority_sd` = "Seniority (in years)",
    `ethnicity:black` = "Black [v. White]",
    `ethnicity:hispanic` = "Hispanic [v. White]",
    `ethnicity:asian` = "Asian [v. White]",
    `ethnicity:other_ethn` = "Other [v. White]",
    `ethnicityblack` = "Black [v. White]",
    `ethnicityhispanic` = "Hispanic [v. White]",
    `ethnicityasian` = "Asian [v. White]",
    `ethnicityother_ethn` = "Other [v. White]",
    `male` = "Male [v. Female]",
    `mov02_sd` = "Electoral margin of victory"
  )) %>%
  filter(predictors != "(Intercept)") %>%
  mutate(predictors = factor(predictors, levels = rev(c(
    "Legislative professionalization",
    "Staff",
    "Legislature in session '18",
    "Legislature w. Term Limits",
    "Democrat",
    "Leadership",
    "Last term",
    "Number of committees",
    "Seniority (in years)",
    "Black [v. White]",
    "Hispanic [v. White]",
    "Asian [v. White]",
    "Other [v. White]",
    "Male [v. Female]",
    "Electoral margin of victory"))),
    model = factor(model, levels = c(
      "Being on Twitter",
      "Being Active: Num. Tweets (log)",
      "Discussing Policy Issues"
    )))

model_plot01_final <- all_models %>%
  filter(model_type != "Staff") %>%
  mutate(model_type = as.character(model_type)) %>%
  mutate(sig = ifelse(sign(1-lwr) == sign(1-upr), 1, 0)) %>%
  # - remove the coeff for "other ethnicity": very few, large CI
  filter(predictors != "Other [v. White]") %>%
  mutate(predictors = factor(predictors, levels = rev(c(
    "Legislative professionalization",
    "Legislature in session '18",
    "Legislature w. Term Limits",
    "Democrat",
    "Leadership",
    "Last term",
    "Number of committees",
    "Seniority (in years)",
    "Black [v. White]",
    "Hispanic [v. White]",
    "Asian [v. White]",
    #"Other [v. White]",
    "Male [v. Female]",
    "Electoral margin of victory"))),
    model = factor(model, levels = c(
      "Being on Twitter",
      "Being Active: Num. Tweets (log)",
      "Discussing Policy Issues"
    )),
    upr = ifelse(grepl("Being on Twitter", model) & 
                   grepl("Black", predictors), 5, upr))

pdf("./figures/figure02.pdf", width = 9, height = 4.5)
ggplot(model_plot01_final %>%
         filter(model_type != "Staff"),
       aes(x = predictors, y = pe)) +
  # - shaded area 1st facet
  geom_point(inherit.aes = FALSE,
             data = data.frame(x = c(15, 16), y = c(1, 1), 
                               model_type = c("All legislators pooled together\n(state-clustered SE)",
                                              "State random intercept\n(MLM)")),
             aes(x = x, y = y, color = model_type), size = 3) +
  geom_polygon(inherit.aes = FALSE, 
               data = data.frame(y = c(0, 0, 7, 7), x = c(10.5, 13.5, 13.5, 10.5),
                                 model = factor("Being on Twitter",
                                                levels = c(
                                                  "Being on Twitter",
                                                  "Being Active: Num. Tweets (log)",
                                                  "Discussing Policy Issues"
                                                ))),
               aes(x = x , y = y),
               fill = "gray90", alpha = 0.5) +
  # - shaded area 2nd facet
  geom_polygon(inherit.aes = FALSE, 
               data = data.frame(y = c(0.5, 0.5, 1.45, 1.45), x = c(10.5, 13.5, 13.5, 10.5),
                                 model = factor("Being Active: Num. Tweets (log)",
                                                levels = c(
                                                  "Being on Twitter",
                                                  "Being Active: Num. Tweets (log)",
                                                  "Discussing Policy Issues"
                                                ))),
               aes(x = x , y = y),
               fill = "gray90", alpha = 0.5) +
  # - shaded area 3rd facet
  geom_polygon(inherit.aes = FALSE, 
               data = data.frame(y = c(0.85, 0.85, 1.15, 1.15), x = c(10.5, 13.5, 13.5, 10.5),
                                 model = factor("Discussing Policy Issues",
                                                levels = c(
                                                  "Being on Twitter",
                                                  "Being Active: Num. Tweets (log)",
                                                  "Discussing Policy Issues"
                                                ))),
               aes(x = x , y = y),
               fill = "gray90", alpha = 0.5) +
  geom_pointrange(inherit.aes = FALSE,
                  data = model_plot01_final %>%
                    filter(model_type == "LegProf"),
                  aes(x = ifelse(predictors %in% 
                                   c("Legislative professionalization", 
                                     "Legislature in session '18",
                                     "Legislature w. Term Limits"), 
                                 as.numeric(predictors), 
                                 as.numeric(predictors) - 0.15), y = pe,
                      ymin = lwr, ymax = upr, alpha = factor(sig)),
                  color = "blue4") +
  geom_pointrange(inherit.aes = FALSE,
                  data = model_plot01_final %>%
                    filter(model_type == "MLM"),
                  aes(x = ifelse(predictors %in% 
                                   c("Legislative professionalization", 
                                     "Legislature in session '18",
                                     "Legislature w. Term Limits"), 
                                 as.numeric(predictors), 
                                 as.numeric(predictors) + 0.15), y = pe,
                      ymin = lwr, ymax = upr, alpha = factor(sig)),
                  color = "orange4") +
  coord_flip() +
  facet_wrap(~model, scales = "free_x") +
  scale_color_manual("", values = c("blue4", "orange4")) +
  scale_alpha_discrete(range = c(0.2, 0.9), guide = FALSE) +
  geom_hline(yintercept = 1, color = "red") +
  scale_x_continuous("", 
                     breaks = seq(
                       1, length(levels(factor(model_plot01_final$predictors)))),
                     labels = levels(factor(model_plot01_final$predictors)),
                     limits = c(0.5, 13.5)) +
  scale_y_continuous("\nMarginal effect on the likelihood of...") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_line(color = "gray80", linetype = "dotted"),
        legend.position = "top")
dev.off()
