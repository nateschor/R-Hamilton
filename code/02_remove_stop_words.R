
df_SMART_stop_words <- stop_words %>%
  filter(lexicon == "SMART") 

df_onix_stop_words <- stop_words %>%
  filter(lexicon == "onix") 

df_snowball_stop_words <- stop_words %>%
  filter(lexicon == "snowball") 

df_SMART_filtered <- df_tokenized %>% 
  anti_join(., df_SMART_stop_words, by = "word")

df_onix_filtered <- df_tokenized %>% 
  anti_join(., df_onix_stop_words, by = "word")

df_snowball_filter <- df_tokenized %>% 
  anti_join(., df_snowball_stop_words, by = "word")

df_all_lexicons_filtered <- df_tokenized %>% 
  anti_join(., stop_words, by = "word")

write_csv(df_all_lexicons_filtered, "data/intermediate/tokenized_and_stop_words_removed.csv")
