# Using Social Media Data to Reveal Patterns of Policy Engagement in State Legislatures

This repository contains the replication code for the paper _"Using Social Media Data to Reveal Patterns of Policy Engagement in State Legislatures"_, authored by Julia Payson, Andreu Casas, Jonathan Nagler, Richard Bonneau, and Joshua A. Tucker. Accepted for publication at _State Politics and Policy Quarterly_.

> __Abstract__:
> State governments are tasked with making important policy decisions in the United States. How do state legislators use their public communications -particularly social media- to engage with policy debates? Due to previous data limitations, we lack systematic information about whether and how state legislators publicly discuss policy and how this behavior varies across contexts. Using Twitter data and state of the art topic modeling techniques, we introduce a method to study state legislator policy priorities and apply the method to fifteen U.S. states in 2018. We show that we are able to accurately capture the policy issues discussed by state legislators with substantially more accuracy than existing methods. We then present initial findings that validate the method and speak to debates in the literature. For example, state legislators in competitive districts are more likely to discuss policy than those in less competitive districts, and legislators from more professional legislatures discuss policy at similar rates to those in less professional legislatures. We conclude by discussing promising avenues for future state politics research using this new approach.

## Data
The `./data/` directory contains the necessary data to replicate the analytical figures and tables of the paper. Below, we describe each of the datasets in this directory:

- `tweets-only-id-variables.csv`: a dataset with the `user_name`, `user_id`, `tweet_id` and `date` of the 2018 collected for the state legislators under analysis.

- `sample-10000-legtweets.csv`: a dataset with about 10,000 tweets sent in 2018 by state legislators under analysis, and the topic we predicted the tweets to be about. We use this dataset to explore what are the most distinctive text features for each topic. The variable `tweet_id` contains the ID for each tweet, `top_topic` contains the predited topic, `clean_text_cnn` contains processed text used to generate topic predictions with the CNN model, `user_id` provides information about the Twitter ID of the user, and `user_name` provides the user handle.

- `model_data_basic.csv`: a dataset with the basic legislator-level covariates included in the regressions reported in the paper. The dataset contains the following variables: `State`, `Chamber`, `District`, `Name`, `Twitter` (screen name), `Party`, `office.firstElect` (when they first elected), `office.lastElect` (last time they were elected), `committee.count` (number of committees in which they serve), `mov02` (margin of victory in the last elections, in percentage of votes), `ethnicity`, `male` (=1, female = 0), `year_first_elected`, `seniority` (number of years the person has been in the state legislature), `leadership` (whether they hold any leadership role in the chamber), `legprofscore` (score indicating the level of professionalization of the legislature, source: The Correlates of State Policy Project), `professional` (3-class variable indicating the level of professionalization of the legislature: Least, Middle-ground, and Most professional, based on the continuous `legprofscore`), `has_term_limits` (whether the legislature specifies term limits = 1, =0 otherwise), `last_term` (whether legislators are in their last term), `staff` (number of staff members in that state legislature, source: https://www.ncsl.org/research/about-state-legislatures/staff-change-chart-1979-1988-1996-2003-2009.aspx). In addition, the dataset also contains a standardized version of the following variables (expressed in standard deviation changes, and represented with `_sd` at the end): `legprofscore_sd`, `staff_sd`, `committee.count_sd`, `seniority_sd`, `mov02_sd`.

- `legislators_topic_tweet_count-25june2020.csv`: a dataset with information about the number of tweets each legislator sent about each topic. The dataset contains  the Twitter `user_id` of each legislator, as well as their `party`, `state`, and `chamber`. Then, the columns starting with an `X` contain the number of tweets on each topic. See lines 81-101 in `05-figure03.R` for a crosswalk table indicating the topic label for each code.

- `final-LEG-model-acc-bytopic-VALSET.csv`: a dataset used to assess the accuracy of the model by topic. `X_val` contains the processed text used by the CNN model to generate the predictions, `tokens` specify the number of tokens in the processed text, `Y_val` contains the true label given by human coders, and `cnn_prediction` contains the topic predicted by the CNN model.

- `memberscongress_topic_tweet_count-25june2020.csv`: a dataset with information about the number of tweets members of Congress sent about each topic in 2018. The dataset contains the Twitter `user_id` for each member, as well as their `party`, `state`, and `chamber`. Then, the columns starting with an `X` contain the number of tweets on each topic. See lines 81-101 in `05-figure03.R` for a crosswalk table indicating the topic label for each code.

- `committee_topic.csv`: a dataset with information about whether a state legislator served in a committee about each of the topics under study. The dataset contains the Twitter `user_id` of the legislators, and then several columns indicating whether they served on a committee on that topic.


## Code
The `./code/` directory contains separate scripts to replicate each analytical figure in the article. The `./figures/` directory contains a copy of each of the figures generated by these scripts. 

- [`01-table02.R`](https://github.com/CasAndreu/social_media_state_legislators/blob/main/code/01-table02.R): To replicate Table 2 of the paper, describing the Twitter activity of state legislators from 15 states, by state and party.

<img src = "https://github.com/CasAndreu/social_media_state_legislators/blob/main/images/table02.png">

- [`02-table05.R`](https://github.com/CasAndreu/social_media_state_legislators/blob/main/code/02-table05.R): To replicate Table 5 of the paper, describing the performance of the topic classifier by topic.

<img src = "https://github.com/CasAndreu/social_media_state_legislators/blob/main/images/table05.png">

- [`03-table06.R`](https://github.com/CasAndreu/social_media_state_legislators/blob/main/code/03-table06.R): To replicate Table 6 of the paper, showing the top distinctive features of tweets predicted to be about each topic.

<img src = "https://github.com/CasAndreu/social_media_state_legislators/blob/main/images/table06.png">

- [`04-figure02.R`](https://github.com/CasAndreu/social_media_state_legislators/blob/main/code/04-figure02.R): To replicate Figure 2 of the paper, showing the results for three statistical models predicting Being on Twitter, Being Active, and iscussing Policy Issues.

<img src = "https://github.com/CasAndreu/social_media_state_legislators/blob/main/images/figure02-SAFE.png">

- [`05-figure03.R`](https://github.com/CasAndreu/social_media_state_legislators/blob/main/code/05-figure03.R): To replicate Figure 3 of the paper, showing the results OLS models predicting the proportion of tweets legislators dedicate to discussing each topic as a function of being on a committee on the topic

<img src = "https://github.com/CasAndreu/social_media_state_legislators/blob/main/images/figure03-SAFE.png">

- [`06-figure04.R`](https://github.com/CasAndreu/social_media_state_legislators/blob/main/code/06-figure04.R): To replicate Figure 4 of the paper, showing Proportion of attention that legislators from each state devoted to each issue area in 2018

<img src = "https://github.com/CasAndreu/social_media_state_legislators/blob/main/images/figure04-SAFE.png">

- [`07-figure05.R`](https://github.com/CasAndreu/social_media_state_legislators/blob/main/code/07-figure05.R): To replicate Figure 5 of the paper, Comparing attention by representatives in Congress and State Legislators

<img src = "https://github.com/CasAndreu/social_media_state_legislators/blob/main/images/figure05-SAFE.png">
