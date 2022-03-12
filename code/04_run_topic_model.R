
Run_LDA <- function(tokenized_df, num_topics) {
  
  tokenized_df %>% 
    filter(!word %in% stop_words$word) %>% 
    count(word) %>% 
    transmute(
      document = "Hamilton",
      term = word,
      count = n
    ) %>% 
    cast_dtm(document, term, count) %>% 
    LDA(., k = num_topics, control = list(seed = 1234))
  
}

Plot_Topics <- function(topic_model) {
  
  tidy(topic_model, matrix = "beta") %>% 
    group_by(topic) %>% 
    slice_max(beta, n = 10) %>% 
    ungroup() %>% 
    arrange(topic, -beta) %>% 
    mutate(term = reorder_within(term, beta, topic)) %>%
    ggplot(aes(beta, term, fill = factor(topic))) +
    geom_col(show.legend = FALSE) +
    facet_wrap(~ topic, scales = "free") +
    scale_y_reordered() + 
    theme_minimal() +
    labs(
      x = expression(beta),
      y = "Term"
    )
  
  
}

map(5:10, ~ Run_LDA(df_tokenized, .) %>% Plot_Topics())

df_tokenized %>% 
  filter(str_detect(speaker, "HAMILTON|ELIZA|WASHINGTON"),
         str_detect(speaker, "EXCEPT|EXEPT", negate = TRUE)) %>%
  Run_LDA(., 3) %>% 
  Plot_Topics()

p_topic_model <- df_tokenized %>% 
  filter(str_detect(speaker, "^HAMILTON$|^ELIZA$|^WASHINGTON$")) %>% 
  Run_LDA(., 3) %>% 
  Plot_Topics()

ggsave(plot = p_topic_model, 
       filename = "paper/figures/topic_model.png")

df_tokenized %>% 
  filter(str_detect(speaker, "^HAMILTON$|^ELIZA$|^KING GEORGE$")) %>% 
  Run_LDA(., 3) %>% 
  Plot_Topics()

df_tokenized %>% 
  filter(str_detect(speaker, "^HAMILTON$|^ELIZA$|^WASHINGTON$")) %>% 
  Run_LDA(., 3) %>% 
  tidy(., matrix = "beta")  %>%
  mutate(topic = paste0("topic", topic)) %>% 
  pivot_wider(names_from = topic, values_from = beta) %>% 
  filter(topic1 > .005 | topic2 > .005) %>%
  mutate(log_ratio = log2(topic3 / topic2)) %>% 
  ggplot(aes(log_ratio, reorder(term, log_ratio))) +
  geom_col() +
  labs(x = "Log2 ratio of beta in topic 2 / topic 1", y = NULL) +
  theme_minimal()

# load movie data and see if topic model can distinguish between movies and hamilton
  
