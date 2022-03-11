
df_SMART_filtered %>% 
  count(song_number, word, sort = TRUE) %>% 
  bind_tf_idf(word, song_number, n) %>% 
  group_by(song_number) %>% 
  slice_max(tf_idf, n = 1) %>% 
  left_join(., df_song_order, by = "song_number") %>% 
  select(title, word, tf_idf) %>% 
  view()

df_SMART_filtered %>% 
  count(song_number, word, sort = TRUE) %>% 
  bind_tf_idf(word, song_number, n) %>% 
  slice_max(tf_idf, n = 5)

df_raw %>% 
  unnest_tokens(bigram, lines, token = "ngrams", n = 3) %>% 
  separate(bigram, c("word1", "word2", "word3")) %>% 
  filter(!word1 %in% stop_words$word, !word2 %in% stop_words$word, !word3 %in% stop_words$word) %>% 
  unite(bigram, word1, word2, word3, sep = " ") %>% 
  left_join(., df_song_order, by = "title") %>% 
  count(song_number, bigram, sort = TRUE) %>% 
  bind_tf_idf(bigram, song_number, n) %>% 
  group_by(song_number) %>% 
  slice_max(tf_idf, n = 1) %>% 
  left_join(., df_song_order, by = "song_number") %>% 
  select(title, bigram, tf_idf) %>% 
  view()

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
  rename_with(.cols = starts_with("word"), .fn = ~ str_remove_all(., "word_"))

kable(df_tfidf, escape = TRUE, format = "latex") %>% 
  save_kable(., file = "paper/tables/speaker_tfidf.tex")

# create table of most important words by speaker comparing how it changes with the different stop word removals and without removing stopwords  
