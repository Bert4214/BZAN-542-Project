import pandas as pd
import numpy as np
from sklearn.linear_model import LinearRegression

DATA = pd.read_table('songs_normalize.csv', delimiter=',')

# Change milliseconds to seconds 
DATA['duration_ms'] = DATA['duration_ms'] * 0.001
DATA.rename(columns={'duration_ms':'duration_s'}, inplace=True)

# Add a column for duration in minutes 
DATA['duration_m'] = DATA['duration_s'] / 60

# Summary statistics 
DATA.duration_s.describe()
DATA.duration_m.describe()
DATA.danceability.describe()
DATA.popularity.describe()
DATA.energy.describe()
DATA.tempo.describe()

# Top 5 in popularity
DATA.popularity.sort_values(ascending=False).head()

top_pop = [1322, 1311, 201, 1613, 6]
DATA.iloc[top_pop,:]

# Top 5 in danceability
DATA.danceability.sort_values(ascending=False).head()

top_dance = [714, 425, 225, 602, 1428]
DATA.iloc[top_dance,:]

# Number of songs by year
DATA.groupby(['year']).size()



## Simple Regression 
model = LinearRegression()
x = np.array(DATA['danceability']).reshape((-1,1))
y = np.array(DATA['popularity'])
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



# Correlation stuff
DATA['popularity'].corr(DATA['danceability'])
DATA['popularity'].corr(DATA['duration_s'])