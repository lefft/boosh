

### overview 

```python 
# pip install peyton
# Obtain your API token from your Edit Profile page (click your username in the header)

throne = peyton.Throne(username='lefft', token="TOKEN")

# Get historical data for a competition
throne.competition('NBA').get_historical_data()
my_historical_data = throne.competition.historical_data

# Get competition data for a competition
throne.competition('NBA').get_competition_data()
my_competition_data = throne.competition.competition_data

# Submit predictions 
throne.competition('NBA').submit(my_submission_df)
```

### see [this walkthru](https://github.com/Throne-AI/Getting-Started/blob/master/Getting%20Started.ipynb) for example 

