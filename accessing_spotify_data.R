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
