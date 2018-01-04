import pandas as pd
import numpy as np
import gensim

# 'http://lefft.xyz/r_minicourse/datasets/top5k-word-frequency-dot-info.csv'
# d = pd.read_csv(url)

t = ['the queen likes her son', 
     'the king hates his son', 
     'the son likes his father', 
     'the son hates his mother', 
     'the daughter likes her mother', 
     'the daughter likes her father']

d = pd.DataFrame()
d['idx'] = [*range(6)]
d['text'] = t







import pandas as pd
import numpy as np

# Create an empty dataframe
data = pd.DataFrame()

# Create our target variable
data['gender'] = list(map(lambda x: 'm' if x < 4 else 'f', range(8)))
# ['m','m','m','m','f','f','f','f']

# Create our feature variables
data['height'] = [6,5.92,5.58,5.92,5,5.5,5.42,5.75]
data['weight'] = [180,190,170,165,100,150,130,150]
data['foot'] = [12,11,12,10,6,8,7,9]

# View the data
data

# Create an empty dataframe
person = pd.DataFrame()

# Create some feature values for this single row
person['height'] = [6]
person['weight'] = [130]
person['foot'] = [8]

# View the data 
person

# Number of males
n_male = data.gender[data.gender == 'm'].count()

# Number of females
n_female = data.gender[data.gender == 'f'].count()

# Total rows
total_ppl = data.gender.count()


# Number of males divided by the total rows
P_male = n_male/total_ppl

# Number of females divided by the total rows
P_female = n_female/total_ppl

# Group the data by gender and calculate the means of each feature
data_means = data.groupby('gender').mean()

# View the values
data_means

# Group the data by gender and calculate the variance of each feature
data_var = data.groupby('gender').var()

# View the values
data_var


# Means for m 
m_height_mean = data_means.height[data_var.index == 'm'].values[0]
m_weight_mean = data_means.weight[data_var.index == 'm'].values[0]
m_foot_mean = data_means.foot[data_var.index == 'm'].values[0]

# Variance for m
m_height_var = data_var['height'][data_var.index == 'm'].values[0]
m_weight_var = data_var['weight'][data_var.index == 'm'].values[0]
m_foot_var = data_var['foot'][data_var.index == 'm'].values[0]

# Means for f
f_height_mean = data_means['height'][data_var.index == 'f'].values[0]
f_weight_mean = data_means['weight'][data_var.index == 'f'].values[0]
f_foot_mean = data_means['foot'][data_var.index == 'f'].values[0]

# Variance for f
f_height_var = data_var['height'][data_var.index == 'f'].values[0]
f_weight_var = data_var['weight'][data_var.index == 'f'].values[0]
f_foot_var = data_var['foot'][data_var.index == 'f'].values[0]



# Create a function that calculates p(x | y):
pxgy = lambda x, mean_y, var_y: \
  1/(np.sqrt(2*np.pi*var_y)) * np.exp((-(x-mean_y)**2)/(2*var_y))


def p_x_given_y(x, mean_y, variance_y):
  # Input the arguments into a probability density function
  p = 1/(np.sqrt(2*np.pi*variance_y)) * np.exp((-(x-mean_y)**2)/(2*variance_y))
  # return p
  return p


# Numerator of the posterior if the unclassified observation is a m
P_male * \
  pxgy(person['height'][0], m_height_mean, m_height_var) * \
    pxgy(person['weight'][0], m_weight_mean, m_weight_var) * \
      pxgy(person['foot'][0], m_foot_mean, m_foot_var)

boosh = [*map(pxgy, 
              [person.height[0], person.weight[0], person.foot[0]], 
              [m_height_mean, m_weight_mean, m_foot_mean], 
              [m_height_var, m_weight_var, m_foot_var])]

P_male * np.product(boosh)



# Numerator of the posterior if the unclassified observation is a f
P_female * \
  pxgy(person['height'][0], f_height_mean, f_height_var) * \
    pxgy(person['weight'][0], f_weight_mean, f_weight_var) * \
      pxgy(person['foot'][0], f_foot_mean, f_foot_var)





