
df_SMART_filtered %>% 
  count(song_number, word, sort = TRUE) %>% 
  bind_tf_idf(word, song_number, n) %>% 
  group_by(song_number) %>% 
  slice_max(tf_idf, n = 1)

df_SMART_filtered %>% 
  count(song_number, word, sort = TRUE) %>% 
  bind_tf_idf(word, song_number, n) %>% 
  slice_max(tf_idf, n = 5)
