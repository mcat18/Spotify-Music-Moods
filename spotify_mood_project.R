# Package names
packages <- c("dplyr", "ggplot2", "lubridate", "here", "knitr", "stringr", "tidytext", 
              "wordcloud", "devtools", "RColorBrewer", "ggridges", "wordcloud2", 
              "highcharter", "tm", "ggwordcloud", "syuzhet", "stm", "quanteda", 
              "data.table", "plotly", "tidyverse", "reshape2", "gapminder", "textdata", 
              "spotifyr", "ggiraph", "cluster", "factoextra", "kableExtra")
# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}
# Packages loading
invisible(lapply(packages, library, character.only = TRUE))


client_id = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' #cannot display this information 
client_secret = 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx' #cannot display this information 

#Accessing spotify data

Sys.setenv(SPOTIFY_CLIENT_ID = client_id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = client_secret)
get_spotify_authorization_code()

#getting the username and playlist information so I can pull all the songs I want to analyze 
playlist_username1 = 'xxxxxxx' #cannot display this information 
playlist_uris4 = c('xxxxxxxxxxxxxxx') #cannot display this information 

#getting the audio playlist features so I can analyze those 
fave_songs_ever = get_playlist_audio_features(playlist_username1, playlist_uris4)

#the artist name is inside fave_songs_ever are inside a nested list so I will have to get the artist name 
#using a for loop 

artist_names = fave_songs_ever$track.artists

names = 0 

for (i in 1:548) {
  names[i] = artist_names[[i]]$name
}

##adding artist name to fave_songs_ever

fave_songs_ever = 
  fave_songs_ever %>% 
  add_column(artist = names)

##going to create another data frame that includes only the relevant information for the analysis 

fave_songs_ever2 = 
  fave_songs_ever %>% 
  dplyr::select(track.name, artist, valence, 
                energy, tempo, speechiness, 
                acousticness, instrumentalness, danceability, 
                loudness) %>% 
  dplyr::rename(song = track.name)

##there is a song I did not mean to include in this playlist, so I am going to remove it completely 

fave_songs_ever2 = 
  fave_songs_ever2 %>% 
  dplyr::select(song, artist, valence, 
                energy, tempo, speechiness, 
                acousticness, instrumentalness, danceability, 
                loudness) %>% 
  dplyr::filter(song != "Dancing With A Ghost")

##need to rename 4 songs. There are two songs with the title Missing and two songs with the title Monster 
n_occur = data.frame(table(fave_songs_ever2$song))

n_occur[n_occur$Freq > 1,] 

fave_songs_ever2$id = 1:nrow(fave_songs_ever2)

fave_songs_ever2 %>% dplyr::filter(song == "Missing")

fave_songs_ever2[fave_songs_ever2$id==280, "song"] = "Missing Flyleaf"
fave_songs_ever2[fave_songs_ever2$id==320, "song"] = "Missing Evanescence" 

fave_songs_ever2 %>% dplyr::filter(song == "Monster")

fave_songs_ever2[fave_songs_ever2$id==229, "song"] = "Monster Lady Gaga"
fave_songs_ever2[fave_songs_ever2$id==339, "song"] = "Monster Meg & Dia" 


#scaling the data for kmeans clustering 

my_fave_songs_scaled = scale(fave_songs_ever2[, c(3:10)])
#summary(my_fave_songs_scaled)

#creating a scree plot and using the elbow method to determine the optimal number of clusters for this data
set.seed(123)

# function to compute total within-cluster sum of square 
wss = function(k) {
  kmeans(my_fave_songs_scaled, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values = 1:15

# extract wss for 2-15 clusters
wss_values = map_dbl(k.values, wss)

#creating this for highcharter viz 
elbow_method_values = data.frame(kvalues = k.values, 
                                 wwsvalues = wss_values)


elbow_method_values %>% 
  hchart(
    "line", 
    hcaes(x = kvalues, y = wss_values), color = "#7e03a8") %>% 
  hc_xAxis(title = list(text = "Number of Clusters")) %>% 
  hc_yAxis(title = list(text = "Total Within-clusters Sum of Sqaures")) %>% 
  hc_title(text = "Optimal Number of Clusters",
           align = "left") %>% 
  hc_subtitle(text = "The results suggest that 3 is the optimal number of clusters", 
              style = list(fontStyle = "italic"), 
              align = "left") %>% 
  hc_tooltip(
    useHTML = TRUE,                             
    formatter = JS(
      "
      function(){
        outHTML = '<b>' + this.point.kvalues
        return(outHTML)
      }
      "
    )
  )

#kmeans analysis 

set.seed(123)
final <- kmeans(my_fave_songs_scaled, 3, nstart = 25)
#print(final)


#fviz_cluster(final, data = my_fave_songs_scaled)

cluster_means = final$centers


#adding cluster to fave_songs_ever2 

fave_songs_ever2$cluster = as.character(final$cluster)


#doing pca to reduce the dimensions so I can visualize the clusters using highcharter

pca_x = princomp(my_fave_songs_scaled)
x_cluster = data.frame(pca_x$scores, final$cluster)
x_cluster$artist = fave_songs_ever2$artist
x_cluster$song = fave_songs_ever2$song


x_cluster = 
  x_cluster %>% 
  dplyr::select(Comp.1,Comp.2)

x_cluster$id = 1:nrow(x_cluster)

fave_songs_ever3 = 
  fave_songs_ever2 %>% 
  dplyr::left_join(x_cluster, by = c("id" = "id"))

fave_songs_ever3 %>% 
  hchart("scatter", 
         hcaes(x = Comp.1, y = Comp.2, group = cluster)) %>% 
  hc_xAxis(title = NULL) %>% 
  hc_yAxis(title = NULL) %>% 
  hc_title(text = "Clusters",
           align = "left") %>% 
  hc_subtitle(text = "Cluster 1 Mood: Sad/Pining <br> Cluster 2 Mood: Angsty/Energetic <br> Cluster 3 Mood: Happy/Lively", 
              style = list(fontStyle = "italic"), 
              align = "left") %>% 
  hc_colors(c("#0d0887", "#cc4778", "#f0f921")) %>% 
  hc_tooltip(
    useHTML = TRUE,                              # The output should be understood to be html markup
    formatter = JS(
      "
      function(){
        outHTML = '<b>' + this.point.song + '</b> <br> Artist: ' + this.point.artist + '<br> Valence: ' + this.point.valence 
        + '<br> Energy: ' + this.point.energy 
        + '<br> Tempo: ' + this.point.tempo + '<br> Speechiness: ' + this.point.speechiness
        + '<br> Acousticness: ' + this.point.acousticness + '<br> Instrumentalness: ' + this.point.instrumentalness
        + '<br> Danceability: ' + this.point.danceability + '<br> Loudness: ' + this.point.loudness
        return(outHTML)
      }
     " 
    )
  )


# making table for cluster means 

cluster_means = final$centers

cluster_means = as.data.frame(cluster_means)

cluster_means = 
  cluster_means %>% 
  add_column(cluster = c("1", "2", "3"))

cluster_means = cluster_means %>% 
  dplyr::select(cluster, valence, energy, tempo, speechiness, acousticness, instrumentalness, danceability, loudness)

cluster_means %>% 
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# making table of songs that are in cluster 1 

fave_songs_ever3 %>% 
  select(song, artist, valence, energy, tempo, speechiness, acousticness, instrumentalness, danceability, loudness) %>% 
  dplyr::filter(song == "Pink in the Night" |
                  song == "Motion Sickness" |
                  song == "Video Games" |
                  song == "Liability" |
                  song == "Gravity") %>% 
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


# making table of songs that are in cluster 2


fave_songs_ever3 %>% 
  select(song, artist, valence, energy, tempo, speechiness, acousticness, instrumentalness, danceability, loudness) %>% 
  dplyr::filter(song == "Easier than Lying" |
                  song == "Fear The Future" |
                  song == "Again" |
                  song == "This Is How I Disappear" |
                  song == "Heart-Shaped Box") %>% 
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))

# making table of songs that are in cluster 3

fave_songs_ever3 %>% 
  select(song, artist, valence, energy, tempo, speechiness, acousticness, instrumentalness, danceability, loudness) %>% 
  dplyr::filter(song == "Starlight (Taylor's Version)" |
                  song == "Physical" |
                  song == "Just Dance" |
                  song == "Electric Love" |
                  song == "Let’s Get Lost") %>% 
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))


##sentiment analysis 


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

#making the density plots to show the distribution of sentiment 

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


final_data_naomit = na.omit(final_data)

##kmeans analysis adding sentiment score variable


my_fave_songs_scaled2 = scale(final_data_naomit[, c(3,4,5,6,7,8,9,10,17)])
#summary(my_fave_songs_scaled2)

#creating a scree plot and using the elbow method to determine the optimal number of clusters for this data
set.seed(123)

# function to compute total within-cluster sum of square 
wss = function(k) {
  kmeans(my_fave_songs_scaled2, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values = 1:15

# extract wss for 2-15 clusters
wss_values = map_dbl(k.values, wss)

#creating this for highcharter viz 
elbow_method_values = data.frame(kvalues = k.values, 
                                 wwsvalues = wss_values)


elbow_method_values %>% 
  hchart(
    "line", 
    hcaes(x = kvalues, y = wss_values), color = "#7e03a8") %>%
  hc_xAxis(title = list(text = "Number of Clusters")) %>% 
  hc_yAxis(title = list(text = "Total Within-clusters Sum of Sqaures")) %>% 
  hc_title(text = "Optimal Number of Clusters",
           align = "left") %>% 
  hc_subtitle(text = "The results suggest that 3 is the optimal number of clusters", 
              style = list(fontStyle = "italic"), 
              align = "left") %>% 
  hc_tooltip(
    useHTML = TRUE,                              # The output should be understood to be html markup
    formatter = JS(
      "
      function(){
        outHTML = '<b>' + this.point.kvalues
        return(outHTML)
      }

      "
    )
  )

#kmeans analysis 

set.seed(123)
final_2 <- kmeans(my_fave_songs_scaled2, 3, nstart = 25)
#print(final_2)

final_data_naomit %>% 
  hchart("scatter", 
         hcaes(x = Comp.1, y = Comp.2, group = cluster)) %>% 
  hc_xAxis(title = NULL) %>% 
  hc_yAxis(title = NULL) %>% 
  hc_title(text = "Clusters",
           align = "left") %>% 
  hc_subtitle(text = "Cluster 1 Mood: Sad/Pining <br> Cluster 2 Mood: Angsty/Energetic <br> Cluster 3 Mood: Happy/Lively", 
              style = list(fontStyle = "italic"), 
              align = "left") %>% 
  hc_colors(c("#0d0887", "#cc4778", "#f0f921")) %>% 
  hc_tooltip(
    useHTML = TRUE,                           
    formatter = JS(
      "
      function(){
        outHTML = '<b>' + this.point.song + '</b> <br> Artist: ' + this.point.artist + '<br> Sentiment Score: ' + this.point.sentiment
        + '<br> Valence: ' + this.point.valence + '<br> Energy: ' + this.point.energy 
        + '<br> Tempo: ' + this.point.tempo + '<br> Speechiness: ' + this.point.speechiness
        + '<br> Acousticness: ' + this.point.acousticness + '<br> Instrumentalness: ' + this.point.instrumentalness
        + '<br> Danceability: ' + this.point.danceability + '<br> Loudness: ' + this.point.loudness
        return(outHTML)
      }

     " 
    )
  )

# creating table for the cluster means 

cluster_means_2 = final_2$centers

cluster_means_2 = as.data.frame(cluster_means_2)

cluster_means_2 = 
  cluster_means_2 %>% 
  add_column(cluster = c("1", "2", "3"))

cluster_means_2 = cluster_means_2 %>% 
  dplyr::select(cluster, valence, energy, tempo, speechiness, acousticness, instrumentalness, danceability, loudness, sentiment)

cluster_means_2 %>% 
  kbl() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"))




















