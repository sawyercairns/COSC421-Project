import kagglehub
from kagglehub import KaggleDatasetAdapter
import pandas as pd

# Download latest version
path = kagglehub.dataset_download("fronkongames/steam-games-dataset")
path = path + "/games.csv"

# Load the CSV file into a DataFrame
df = pd.read_csv(path, usecols=[
    'AppID',
    'Name',
    'Release date',
    'Estimated owners',
    'Peak CCU',
    'Price',
    'Discount',
    'Metacritic score',
    'User score',
    'Positive',
    'Negative',
    'Recommendations',
    'Average playtime forever',
    'Average playtime two weeks',
    'Median playtime forever',
    'Median playtime two weeks',
    'Categories',
    'Genres',
    'Tags'
    ])

# Export a Preprocessed csv
df.to_csv('games_preprocessed.csv', index=False)