
v_speakers <- df_raw %>% 
  count(speaker, sort = TRUE) %>% 
  filter(str_detect(speaker, "^[A-Za-z]+$"),
         n > 10) %>% 
  pull(speaker)

Calc_TFIDF_Stopwords <- function(tokenized_data, speakers_vector, stopwords_lexicon) {
  
  tokenized_data %>% 
    filter(speaker %in% speakers_vector,!word %in% (stop_words %>% filter(lexicon == stopwords_lexicon) %>% pull(word))) %>% 
    count(speaker, word, sort = TRUE) %>% 
    bind_tf_idf(word, speaker, n) %>% 
    group_by(speaker) %>% 
    slice_max(tf_idf, n = 1) %>% 
    transmute(
      speaker,
      "word_{stopwords_lexicon}" := word,
      "tf_idf_{stopwords_lexicon}" := tf_idf
    )
  
  
}

df_SMART_tfidf <- Calc_TFIDF_Stopwords(df_tokenized, v_speakers, "SMART")
df_snowball_tfidf <- Calc_TFIDF_Stopwords(df_tokenized, v_speakers, "snowball")
df_onix_tfidf <- Calc_TFIDF_Stopwords(df_tokenized, v_speakers, "onix")

df_all_lexicons_tfidf <- df_tokenized %>% 
  filter(speaker %in% v_speakers, !word %in% stop_words$word) %>% 
  count(speaker, word, sort = TRUE) %>% 
  bind_tf_idf(word, speaker, n) %>% 
  group_by(speaker) %>% 
  slice_max(tf_idf, n = 1) %>% 
  transmute(
    speaker,
    "All Lexicons" = word,
    tf_idf_all_stop_words = tf_idf
  )

df_all_stop_words_tfidf <- df_tokenized %>% 
  filter(speaker %in% v_speakers) %>% 
  count(speaker, word, sort = TRUE) %>% 
  bind_tf_idf(word, speaker, n) %>% 
  group_by(speaker) %>% 
  slice_max(tf_idf, n = 1) %>% 
  transmute(
    speaker,
    "All Words" = word,
    tf_idf_all_stop_words = tf_idf
  )
  


df_tfidf <- list(df_SMART_tfidf, df_snowball_tfidf, df_onix_tfidf, df_all_lexicons_tfidf, df_all_stop_words_tfidf) %>% 
  reduce(inner_join, by = "speaker") %>% 
  arrange(speaker) %>% 
  group_by(speaker) %>% 
  mutate(
    number = row_number()
  ) %>% 
  ungroup() %>% 
  filter(number %in% 1:2) %>% 
  select(Speaker = speaker, starts_with("word"), starts_with("All")) %>% 
  rename_with(.cols = starts_with("word"), .fn = ~ str_remove_all(., "word_")) %>% 
  mutate(
    Speaker = str_to_title(Speaker),
    Entry = row_number()
  ) %>% 
  select(Entry, everything())
  

kable(df_tfidf, escape = TRUE, format = "latex") %>% 
  row_spec(0, bold = TRUE) %>% 
  save_kable(., file = "paper/tables/speaker_tfidf.tex")

