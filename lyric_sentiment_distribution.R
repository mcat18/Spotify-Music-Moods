song_lyrics = read.csv(here("Input", "fave_songs_lyrics.csv"))

tokens = song_lyrics  %>% 
  select(song, lyrics) %>% 
  tidytext::unnest_tokens(output = word, input = lyrics) %>% 
  anti_join(stop_words) %>% 
  filter(!str_detect(word, "[:digit:]")) %>% 
  filter(!str_detect(word, "[:punct:]"))

sentiment_bing = tokens %>% 
  inner_join(tidytext::get_sentiments("bing"), 
             by = "word") %>% 
  count(song, sentiment) %>% 
  pivot_wider(names_from = sentiment, 
              values_from = n) %>% 
  mutate(positive = as.numeric(positive), 
         positive = case_when(is.na(positive) == TRUE ~ 0, 
                              is.na(positive) == FALSE ~ positive), 
         negative =  as.numeric(negative),
         negative = case_when(is.na(negative) == TRUE ~ 0, 
                              is.na(negative) == FALSE ~ negative), 
         sentiment = positive - negative) %>% 
  mutate(method = "bing")


final_data = fave_songs_ever3 %>% dplyr::left_join(sentiment_bing, by = c("song" = "song"))

sentiment_bing_2 = 
  sentiment_bing %>% 
  arrange(desc(sentiment))

sentiment_songs_for_viz = fave_songs_ever2 %>% dplyr::left_join(sentiment_bing_2, by = c("song" = "song"))

sentiment_songs_for_viz_nomiss = na.omit(sentiment_songs_for_viz)

sentiment_cluster1 = sentiment_songs_for_viz_nomiss %>% dplyr::filter(cluster == "1")

sentiment_cluster1 = 
  sentiment_cluster1 %>% 
  arrange(desc(sentiment))

density(sentiment_cluster1$sentiment) %>% 
  hchart(name = "Density",  color = "#0d0887") %>% 
  # hc_xAxis(title = list(text = "Distribution of Sentiment Cluster 1")) %>%
  hc_yAxis(title = list(text = "Density")) %>% 
  hc_title(text = "Distribution of Sentiment Cluster 1",
           align = "left")


sentiment_cluster2 = sentiment_songs_for_viz_nomiss %>% dplyr::filter(cluster == "2")

sentiment_cluster2 = 
  sentiment_cluster2 %>% 
  arrange(desc(sentiment))

density(sentiment_cluster2$sentiment) %>% 
  hchart(name = "Density",  color = "#cc4778") %>% 
  # hc_xAxis(title = list(text = "Distribution of Sentiment Cluster 1")) %>%
  hc_yAxis(title = list(text = "Density")) %>% 
  hc_title(text = "Distribution of Sentiment Cluster 2",
           align = "left")


sentiment_cluster3 = sentiment_songs_for_viz_nomiss %>% dplyr::filter(cluster == "3")

sentiment_cluster3 = 
  sentiment_cluster3 %>% 
  arrange(desc(sentiment))

density(sentiment_cluster3$sentiment) %>% 
  hchart(name = "Density",  color = "#f0f921") %>% 
  # hc_xAxis(title = list(text = "Distribution of Sentiment Cluster 1")) %>%
  hc_yAxis(title = list(text = "Density")) %>% 
  hc_title(text = "Distribution of Sentiment Cluster 3",
           align = "left")