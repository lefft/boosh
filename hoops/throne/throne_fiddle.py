import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import peyton
import scipy.stats as stats
from sklearn.linear_model import LinearRegression

throne = peyton.throne(username='your-username', token='your-token')

throne.competition('NFL').get_historical_data() # retrieve historical NFL data
data = throne.competition.historical_data

cutoff = int(data.shape[0]*0.75)
train_data = data.iloc[:cutoff, :]
test_data = data.iloc[cutoff+1:, :]

# Make a point spread variable
train_data['point_spread'] = train_data['team_1_score'] - train_data['team_2_score']

plt.figure(figsize=(15, 6))
plt.hist(train_data['point_spread']);
plt.xlabel('Point Spread');

# Train a simple model on the training data
feature_cols = ['d_ability_1', 'p_ability_1', 'p_ability_2', "d_h2h_1", "travel_3"]

new_train_data = train_data.dropna()
y = np.array([new_train_data['point_spread'].values]).T
X = new_train_data[feature_cols].values

reg = LinearRegression(fit_intercept=True)
reg.fit(X, y)

# Make predictions on test data
X_pred = test_data[feature_cols].values
point_spread_pred = reg.predict(X_pred).T[0]
test_data['team_1_prob'] = stats.norm.cdf(point_spread_pred, loc=0, scale=np.std(new_train_data['point_spread']))
test_data['team_2_prob'] = 1.0 - test_data['team_1_prob']
test_data.head()
test_data.to_csv("nfl_backtest.csv")

# The CSV can then be submitted through the backtesting module 
# We can then use this model on competition data

throne.competition('NFL').get_competition_data()
comp_data = throne.competition.competition_data

# Fill the team_1_prob and team_2_prob columns
X_pred = comp_data[feature_cols].values
point_spread_pred = reg.predict(X_pred).T[0]
comp_data['team_1_prob'] = stats.norm.cdf(point_spread_pred, loc=0, scale=np.std(train_data['point_spread']))
comp_data['team_2_prob'] = 1.0 - comp_data['team_1_prob']

# Now we can submit to the platform
throne.competition('NFL').submit(comp_data)

