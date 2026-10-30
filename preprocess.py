import kagglehub
from kagglehub import KaggleDatasetAdapter
import pandas as pd

# Download latest version
path = kagglehub.dataset_download("fronkongames/steam-games-dataset")
path = path + "/games.csv"

# Fix missing comma between discount and dlc headers
with open(path, "r", encoding="utf-8") as f:
    lines = f.readlines()

lines[0] = lines[0].replace("DiscountDLC", "Discount, DLC")

with open(path, "w", encoding="utf-8") as f:
    f.writelines(lines)

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

# Interate over df to remove null elements, format data, and remove entries with small numbers of estimated owners
unimportant_indxs = []
null_indxs = []
for i in range(len(df)):
    if df.iloc[i]['Estimated owners'] == '0 - 0' or df.iloc[i]['Estimated owners'] == '0 - 20000':
        unimportant_indxs.append(i)

    if df.iloc[i]['Genres'] is None and df.iloc[i]['Tags'] is None and i not in unimportant_indxs:
        null_indxs.append(i)
            
df.drop(unimportant_indxs)
    
df['Genres'] = df['Genres'].str.lower()
df['Tags'] = df['Tags'].str.lower()

# Export a Preprocessed csv
df.to_csv('games_preprocessed.csv', index=False)