
df_raw <- read_csv("data/raw/ham_lyrics.csv") %>% 
  glimpse()

set.seed(1234)


# Data Sample -------------------------------------------------------------

df_data_sample <- df_raw %>% 
  sample_n(5) %>% 
  mutate(
    entry = row_number()
  ) %>% 
  select(entry, everything()) %>% 
  rename(
    Entry = entry,
    Title = title,
    Speaker = speaker,
    Lines = lines
  ) %>% 
  mutate(
    Speaker = str_to_title(Speaker),
  )

kable_data_sample <- df_data_sample %>% 
  kable(., escape = TRUE, format = "latex") %>% 
  row_spec(0, bold = TRUE) %>% 
  column_spec(., column = 4, width = "6cm") %>%
  save_kable(., file = "paper/tables/example_raw_data.tex")


# Sentiment ---------------------------------------------------------------

df_song_order <- df_raw %>% 
  select(title) %>% 
  distinct() %>% 
  mutate(
    song_number = row_number()
  )

df_sentimentr <- df_raw %>% 
  mutate(
    replace = gsub('[^ -~]', '', lines)
  ) %>% 
  pull(replace) %>% 
  get_sentences() %>% 
  sentiment() %>% 
  as_tibble() %>% 
  select(element_id, sentiment)

df_sentimentr_sentiment <- df_raw %>% 
  mutate(
    element_id = row_number()
  ) %>% 
  left_join(., df_sentimentr, by = "element_id") %>% 
  left_join(., df_song_order, by = "title") %>% 
  group_by(song_number) %>% 
  summarize(
    sentimentr_sentiment = sum(sentiment)
  )

df_tokenized <- df_raw %>% 
  left_join(., df_song_order, by = "title") %>% 
  unnest_tokens(word, lines, to_lower = TRUE)


afinn <- get_sentiments("afinn") %>% glimpse()
bing <- get_sentiments("bing") %>% glimpse()
nrc <- get_sentiments("nrc") %>% glimpse()
loughran <- get_sentiments("loughran") %>% glimpse()

df_afinn_sentiment <- df_tokenized %>% 
  left_join(., afinn, by = "word") %>% 
  group_by(song_number) %>% 
  summarize(
    afinn_sentiment = sum(value, na.rm = TRUE)
  ) 

df_bing_sentiment <- df_tokenized %>% 
  left_join(., bing, by = "word") %>% 
  mutate(
    sentiment = case_when(
      sentiment == "positive" ~ 1,
      sentiment == "negative" ~ -1,
      TRUE ~ NA_real_
    )
  ) %>% 
  group_by(song_number) %>% 
  summarize(
    bing_sentiment = sum(sentiment, na.rm = TRUE)
  ) 

df_nrc_sentiment <- df_tokenized %>% 
  left_join(., nrc, by = "word") %>% 
  filter(sentiment %in% c("positive", "negative")) %>% 
  mutate(
    sentiment = case_when(
      sentiment == "positive" ~ 1,
      sentiment == "negative" ~ -1,
      TRUE ~ NA_real_
    )
  ) %>% 
  group_by(song_number) %>% 
  summarize(
    nrc_sentiment = sum(sentiment, na.rm = TRUE)
  ) 

df_sentiment <- list(df_afinn_sentiment, df_bing_sentiment, df_nrc_sentiment, df_sentimentr_sentiment) %>% 
  reduce(left_join, by = "song_number") %>% 
  pivot_longer(., cols = -song_number, names_to = "Sentiment Source", values_to = "Sentiment") %>% 
  mutate(
    "Sentiment Source" = str_remove(`Sentiment Source`, "_sentiment")
  )

df_sentiment %>% 
  group_by(`Sentiment Source`) %>% 
  mutate(
    previous_song_sentiment = lag(Sentiment)
  ) %>% 
  ungroup() %>% 
  mutate(
    delta_sentiment = Sentiment - previous_song_sentiment
  ) %>% 
  select(song_number, delta_sentiment, `Sentiment Source`) %>% 
  left_join(., df_song_order, by = "song_number") %>% 
  group_by(`Sentiment Source`) %>% 
  slice_max(delta_sentiment, n = 2)

df_sentiment <- df_sentiment %>% 
  left_join(., df_song_order, by = "song_number")

p_sentiment <- ggplot(df_sentiment, aes(x = song_number, y = Sentiment)) +
  geom_line() +
  geom_hline(color = "red", yintercept = 0) +
  geom_point() +
  facet_wrap(~ `Sentiment Source`, ncol = 1, scales = "free_y") +
  theme_minimal() +
  labs(x = "Song Number") +
  geom_text_repel(data = df_sentiment %>% filter(song_number %in% c(10, 11, 23)),
             aes(label = title))
  
  
ggsave(plot = p_sentiment, filename = "paper/figures/sentiment_by_stopwords.png")  

