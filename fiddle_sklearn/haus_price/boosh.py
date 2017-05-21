import os
import sys
import pandas as pd

# check python version
print(sys.version)

# get cwd
print(os.getcwd())

rel_path = 'pie/'
abs_path = '/Users/timothyleffel/Google Drive/sandboxxxe/pie/'

# set cwd with relative path
os.chdir(rel_path)

# see objects in workspace
dir()

# see contents of wd 
os.listdir('bl-states/')

# The file bl-MSA.zip contains data used to produce MSA level estimates.
# The file bl-states.zip contains data for state level analysis. See
# below for a description of the variables in the data files.

# msa_cd		MSA code (identifier)
# st_cd		state code (identifier)
# year		year
# lhpi_real	log of real housing price index
# lpci_real	log of real per capita disposible income
# pgr		population growth rate
# rcb		real cost of borrowing


# read in a dataset
# dat2v = pd.read_csv('bl-states/state_2v.txt', sep='\t')
# dat4v = pd.read_csv('bl-states/state_4v.txt', sep='\t')
# dat4v has a space after the fourth column name -- so cols read incorrectly

dat = pd.read_csv('bl-states/state_4v.txt', sep='\t')
# can use inplace=True to avoid reassignment
dat = dat.drop('rcb', axis=1)
dat = dat.rename(columns={'Unnamed: 5':'rcb'})



# to rename cols:
# df = df.rename(columns={'oldName1': 'newName1', 'oldName2': 'newName2'})
# # OR
# df.rename(columns={'oldName1': 'newName1', 'oldName2': 'newName2'}, inplace=True)

# df = df.drop('column_name', 1)
# where 1 is the axis number (0 for rows and 1 for columns.)

# To delete the column without having to reassign df you can do:

# df.drop('column_name', axis=1, inplace=True)
# Finally, to drop by column number instead of by column label, try this to delete, e.g. the 1st, 2nd and 4th columns:

# df.drop(df.columns[[0, 1, 3]], axis=1)  # df.columns is zero-based pd.Index 


### STUFF FROM EMPTY DOC ##############

import numpy as np
import pandas as pd
from matplotlib import pyplot as plt

N = 100
df = pd.DataFrame({
    'A': pd.date_range(start='2016-01-01',periods=N,freq='D'),
    'x': np.linspace(0,stop=N-1,num=N),
    'y': np.random.rand(N),
    'C': np.random.choice(['Low','Medium','High'],N).tolist(),
    'D': np.random.normal(100, 10, size=(N)).tolist()
})

df.head()


df.boxplot()


with plt.style.context('fivethirtyeight'):
    plt.plot(df.x, np.sin(df.x*5) + 1   * df.x + np.random.randn(N)*15)
    plt.plot(df.x, np.sin(df.x*5) + .5  * df.x + np.random.randn(N)*5)
    plt.plot(df.x, np.sin(df.x*1) + 2   * df.x + np.random.randn(N)*20)

plt.title('Random lines')
plt.show()



