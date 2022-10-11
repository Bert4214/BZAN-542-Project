import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression

SpotifyNoNA = pd.read_csv('Spotify_Songs_NoNA.csv', delimiter='\t')
SpotifyNoNA

# Column Names
sorted(SpotifyNoNA)

# Cleaning
SpotifyNoNA.isna().sum()

# Summary Stats
SpotifyNoNA.describe()

# Top 5 in popularity
SpotifyNoNA.popularity.sort_values(ascending=False).head()
top_pop = [334683, 558728, 275327, 504722, 426612]

Top_Popular = SpotifyNoNA.iloc[top_pop,:]
Top_Popular.loc[:,['artist_name','track_name','genres','popularity']]

# Simple regression
RegressionPopularityData = SpotifyNoNA.dropna(subset = ['artist_popularity'])
len(RegressionPopularityData)

model = LinearRegression()
x = np.array(RegressionPopularityData['artist_popularity']).reshape((-1,1))
y = np.array(RegressionPopularityData['popularity'])
model.fit(x,y)

# R-Square
r_sq = model.score(x,y)
print(f"coefficient of determination: {r_sq}")

# Intercept and Slope
print(f"intercept: {model.intercept_}")
print(f"slope: {model.coef_}")

# Preidction
y_pred = model.predict(x)
print(f"predicted response:\n{y_pred}")