
Choose_Stopwords <- function(char_lexicon) {
  
  stop_words %>% 
    filter(lexicon == char_lexicon)
  
}

df_SMART_stop_words <- Choose_Stopwords("SMART") 
df_onix_stop_words <- Choose_Stopwords("onix") 
df_snowball_stop_words <- Choose_Stopwords("snowball") 

Remove_Stopwords <- function(df_stop_words) {
  
  df_tokenized %>% 
    anti_join(., df_stop_words, by = "word")
  
}

df_SMART_filtered <- Remove_Stopwords(df_SMART_stop_words)
df_onix_filtered <- Remove_Stopwords(df_onix_stop_words)
df_snowball_filter <- Remove_Stopwords(df_snowball_stop_words)
df_all_lexicons_filtered <- Remove_Stopwords(stop_words)

write_csv(df_all_lexicons_filtered, "data/intermediate/tokenized_and_stop_words_removed.csv")
