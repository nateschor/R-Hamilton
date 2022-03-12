
pacman::p_load(tidyverse,
               tidytext,
               sentimentr,
               topicmodels,
               ggrepel,
               kableExtra)

source("code/01_calculate_sentiment.R")
source("code/02_remove_stop_words.R")
source("code/03_calculate_tf_idf.R")
source("code/04_run_topic_model.R")

