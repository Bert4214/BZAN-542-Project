import pandas as pd
import numpy as np


spotify_songs = pd.read_csv('spotify_songs.csv', delimiter=',')

## Find rows containing NA

# Remove songs with multiple artists containing NA
MultArtistsRemove = spotify_songs[spotify_songs['artist_name'].str.contains(',', na=False) == False]

# Remove missing value rows 
MultArtistsRemove[MultArtistsRemove['genres'].isna()]
MultArtistsRemove[MultArtistsRemove['track_name'].isna()]
NoMoreNAs = MultArtistsRemove.dropna(subset=['genres','track_name'])  # Only leaves 468,928 rows 
NoMoreNAs.isna().sum()

# Edit rows with NA without removing
NoNa = spotify_songs.replace(['NA',','],'', regex=True)  # Replace NA strings with ''
len(NoNa)
NoNa.isna().sum()
NoMoreNAs = NoNa.dropna(subset=['genres','track_name']) # Delete rows with entirely missing values 
NoMoreNAs.isna().sum()
len(NoMoreNAs)
NoMoreNAs['artist_popularity'] = pd.to_numeric(NoMoreNAs['artist_popularity']) 
NoMoreNAs.isna().sum() # Now there is NAs...
NoMoreNAs[NoMoreNAs['artist_popularity'].isna()] # locate the new NAs

NoMoreNAs.info()  # Look at variable types in dataframe

NoMoreNAs.describe()  # summary stats for the NA-free dataset 

NoMoreNAs.to_csv('Spotify_Songs_NoNA.csv', sep='\t')  # Create CSV file with no more NAs
