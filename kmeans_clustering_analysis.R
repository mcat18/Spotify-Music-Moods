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










