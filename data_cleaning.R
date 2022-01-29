source(here::here('R Scripts', 'accessing_spotify_data.R'))

#getting the username and playlist information so I can pull all the songs I want to analyze 
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
















