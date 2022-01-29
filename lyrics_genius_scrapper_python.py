import lyricsgenius as genius
import csv


# Insert client ID
api = genius.Genius("xxxxxxxxxxxxxxxxxxxx") #can't show this information 

#Code from https://www.storybench.org/download-song-lyrics-genius-using-python/

with open('fave_songs.csv') as csv_file:
    csv_reader = csv.reader(csv_file, delimiter=',')
    line_count = 0
    for row in csv_reader:
        if line_count == 0:
            line_count += 1
        else:
            item_artist = row[0]
            item_song = row[1]
            # print(f'\t{row[0]}\t{row[1]}\t')
            song = api.search_song(item_song,item_artist)
            filename = item_song.replace("/","_") + "|" + item_artist.replace("/","_")
            if song is not None:
                song.save_lyrics(extension='txt', overwrite=True)
            

            line_count += 1

    print('Processed {line_count} lines.')
