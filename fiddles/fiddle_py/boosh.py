

import nltk

nltk.boolean_ops()

read_expr = nltk.sem.Expression.fromstring
read_expr('-(P & Q)')
read_expr('P & Q')
read_expr('P | (R -> Q)')
read_expr('P <-> -- P')

# Arguments can be tested for "syntactic validity" by using a proof system. 
# We will say a little bit more about this later on in 3. 
# Logical proofs can be carried out with NLTK's inference module, 
# for example via an interface to the third-party theorem prover Prover9. 
# The inputs to the inference mechanism first have to be converted into 
# logical expressions.

lp = nltk.sem.Expression.fromstring
SnF = read_expr('SnF')
NotFnS = read_expr('-FnS')
R = read_expr('SnF -> -FnS')
from nltk import Prover9
prover = nltk.Prover9()
prover.prove(NotFnS, [SnF, R])

val = nltk.Valuation([('P', True), ('Q', True), ('R', False)])
val['P']

dom = set()
g = nltk.Assignment(dom)

m = nltk.Model(dom, val)

print(m.evaluate('(P & Q)', g))


read_expr = nltk.sem.Expression.fromstring
expr = read_expr('walk(angus)', type_check=True)
expr.argument

expr.argument.type
e
expr.function

expr.function.type


sig = {'walk': '<e, t>'}
expr = read_expr('walk(angus)', signature=sig)
expr.function.type


read_expr = nltk.sem.Expression.fromstring
read_expr('dog(cyril)').free()
set()
read_expr('dog(x)').free()
{Variable('x')}
read_expr('own(angus, cyril)').free()
set()
read_expr('exists x.dog(x)').free()
set()
read_expr('((some x. walk(x)) -> sing(x))').free()
{Variable('x')}
read_expr('exists x.own(y, x)').free()





nltk.download_gui()

nltk.data.show_cfg('grammars/book_grammars/sql0.fcfg')


from nltk import load_parser
cp = load_parser('grammars/book_grammars/sql0.fcfg')
query = 'What cities are located in China'
trees = list(cp.parse(query.split()))
answer = trees[0].label()['SEM']
answer = [s for s in answer if s]
q = ' '.join(answer)
print(q)
# SELECT City FROM city_table WHERE Country="china"

from nltk.sem import chat80
rows = chat80.sql_query('corpora/city_database/city.db', q)
for r in rows: 
  print(r[0], end=" ")



# also peep dis book
# https://github.com/jakevdp/PythonDataScienceHandbook?utm_campaign=Data%2BElixir&utm_medium=email&utm_source=Data_Elixir_107

# here is some good numpy exercises:
# https://github.com/Kyubyong/numpy_exercises?utm_campaign=Data%2BElixir&utm_medium=email&utm_source=Data_Elixir_110

# cheque aut pymc3 for mcmc in python
# pip install git+https://github.com/pymc-devs/pymc3




### PLAYINK AROUND AREA <3 #########################################
# === === === === === === === === === === === === === === === 
import math

my_sqrt = lambda x: math.sqrt(x)
my_sqrt(9)





### THIS NEXT LINE IS 80 CHARS LONG -- USE THIS AS MAX EXECPT MAYBE SINCE
### PYTHON HAS SEMANTIC INDENTING THAT CD BE A PROB... :/
################################################################################
#####################################################################################
# Here is an example of using Rodeo:

# We'll use the popular package called Pandas
# Install it with pip
! pip install pandas

# Import it as 'pd'
import pandas as pd

# Create a dataframe
dat = pd.DataFrame({
    "Animal":["dog","dolphin","chicken","ant","spider"],
    "Legs":[4,0,2,6,8]
})

dat.head()

#####################################################################################
# An example of making a plot:
! pip install ggplot

from ggplot import ggplot, aes, geom_bar

ggplot(dat, aes(x="Animal", weight="Legs")) + geom_bar(fill='blue')


# download salary_data.csv save contents to 
# a local file in the same directory as this notebook
!curl http://www.justinmrao.com/salary_data.csv >> ./salary_data.csv

# peak* at the first row
! head -n 1 salary_data.csv | tr -s "," "\n"

# peak* at the second row
! head -n+2 salary_data.csv | tail -n-1 | tr -s "," "\n"

# Rodeo lets you view plots and charts
import seaborn as sns
%matplotlib inline
X = range(10)
y = range(11,21)

sns.stripplot(X,y)
# sns.scatter(X,y, c='r')


### THE NUMPY AREA ##############################################
# === === === === === === === === === === === === === === === 

"""example taken from Scipy and NumPy by Eli Bressert (p. 6)"""
import numpy as np

def list_times(alist, scalar):
    for i, val in enumerate(alist):
        alist[i] = val * scalar
    return alist


arr = np.arange(1e7)
l   = arr.tolist()

%timeit arr * 1.1
%timeit list_times(l, 1.1)

# this throws syntax error :/
print "len(l)", len(l)
print "len(arr)", len(arr)

# for subsetting
l = [ [1,2], [3,4] ]
arr   = np.array(l)

print "Value in Row One, Column One: %d" % l[0][0]
print "Value in Row One, Column One: %d" % arr[0,0]
print "Value in All Rows, Column Two: %s" % str(arr[::,1])
print "Value in Row Two, Both Columns: %s" % str(arr[1::,])


# You can access elements stored in an ndarray using arr[] sub notation arr[rows, columns].
# Select the entire 10th row like so arr[9, :].  9 indicates "row 10" and : indicates all columns.
zero_to_1000 = np.arange(0,1000)               # create an array of integers from 0 to 1000
zero_to_1000 = zero_to_1000.reshape( (500,2) ) # reshape into 2 dimensions (500 Rows x 2 Cols2)
zero_to_1000[:100, 1]                          # select the 2nd columns from the top 100 rows


# also index-based and comprehension-based subsetting is poss waoww :o
numbers = np.random.uniform(size=100)
numbers = numbers.reshape((50,2))
mask = (numbers >= 0.7) & (numbers <= 0.9)
mask[:10]


### THE SCIPY AREA ##############################################
# === === === === === === === === === === === === === === === 
import scipy


### THE MATPLOTLIB AREA #########################################
# === === === === === === === === === === === === === === === 
import pylab as pl
import matplotlib.pyplot as plt

# Make some data to plot
x = np.linspace(start=0, stop=2*np.pi, num=50)
y1 = np.sin(x)
y2 = np.cos(x)

# for line plot
fig, ax = plt.subplots(1, figsize=(12,6))
ax.plot(x, y1, label='sin')
ax.plot(x, y2, label='cos')

ax.legend(fontsize=16)
ax.axes.xaxis.set_label_text("X Axis", fontdict={"size":22})
ax.axes.yaxis.set_label_text("y Axis", fontdict={"size":22})
ax.title.set_text("This is a graph\n")
ax.title.set_fontsize(28)
fig.show()
print


# for histogram
d = np.random.randn(100) * 100.
m = d.mean()
s = d.std()
m_y = 1.5

fig = plt.figure(figsize=(10,5))

ax = plt.subplot(111)
ax.hist(d, 15)
ax.plot(m, m_y, "ko")
ax.plot([m - s, m + s], [m_y] * 2, "k--");


# for grid-like display of multiple plots
x = np.arange(0, 100)
y = np.random.rand(100)  # 100 random numbers
fig, ((ax1, ax2), (ax3, ax4)) = plt.subplots(2,2,figsize=(10,10))
ax1.plot(x, y)
ax2.hist(y)
ax3.scatter(x, y)
ax4.boxplot(y)
plt.show()


### AAAAAND THE PANDAS AREA! ####################################
# === === === === === === === === === === === === === === === 

import pandas as pd
df = pd.DataFrame({
    "A": range(10), 
    "B": np.random.random(size=10)
})

df.B.corr(df.A)


### THE SCIKIT-LEARN AREA #######################################
# === === === === === === === === === === === === === === === 

# NOTE: DONT KNOW IF SCI-KIT LEARN IS INSTALLED - CHEQUE OUT LATER

import scipy.misc
import matplotlib.pyplot as plt

face = scipy.misc.face()
plt.gray()
plt.imshow(face)
plt.show()


"""
===============================================================
A demo of structured Ward hierarchical clustering on raccoon image
===============================================================

Compute the segmentation of a 2D image with Ward hierarchical
clustering. The clustering is spatially constrained in order
for each segmented region to be in one piece.
"""

# Author : Vincent Michel, 2010
#          Alexandre Gramfort, 2011
# License: BSD 3 clause

print(__doc__)

import time as time

import numpy as np
import scipy as sp

import matplotlib.pyplot as plt

from sklearn.feature_extraction.image import grid_to_graph
from sklearn.cluster import AgglomerativeClustering
from sklearn.utils.testing import SkipTest
from sklearn.utils.fixes import sp_version

if sp_version < (0, 12):
    raise SkipTest("Skipping because SciPy version earlier than 0.12.0 and "
                   "thus does not include the scipy.misc.face() image.")


###############################################################################
# Generate data
try:
    face = sp.face(gray=True)
except AttributeError:
    # Newer versions of scipy have face in misc
    from scipy import misc
    face = misc.face(gray=True)

# Resize it to 10% of the original size to speed up the processing
face = sp.misc.imresize(face, 0.10) / 255.

X = np.reshape(face, (-1, 1))

###############################################################################
# Define the structure A of the data. Pixels connected to their neighbors.
connectivity = grid_to_graph(*face.shape)

###############################################################################
# Compute clustering
print("Compute structured hierarchical clustering...")
st = time.time()
n_clusters = 15  # number of regions
ward = AgglomerativeClustering(n_clusters=n_clusters, linkage='ward',
                               connectivity=connectivity)
ward.fit(X)
label = np.reshape(ward.labels_, face.shape)
print("Elapsed time: ", time.time() - st)
print("Number of pixels: ", label.size)
print("Number of clusters: ", np.unique(label).size)

###############################################################################
# Plot the results on an image
plt.figure(figsize=(5, 5))
plt.imshow(face, cmap=plt.cm.gray)
for l in range(n_clusters):
    plt.contour(label == l, contours=1,
                colors=[plt.cm.spectral(l / float(n_clusters)), ])
plt.xticks(())
plt.yticks(())
plt.show()




### THE STATSMODEL AREA ##########################################
# === === === === === === === === === === === === === === === 

# statistics & econometrics package with useful tools for parameter estimation & statistical testing
# Features include:
# linear regression models
# GLMs
# time series modeling
# integration with pandas


import pandas as pd
import statsmodels.api as sm
import pylab as pl
import numpy as np
 
# read the data in
df = pd.read_csv("http://www.ats.ucla.edu/stat/data/binary.csv")
 
# rename the 'rank' column because there is also a DataFrame method called 'rank'
df.columns = ["admit", "gre", "gpa", "prestige"]
dummy_ranks = pd.get_dummies(df['prestige'], prefix='prestige')
dummy_ranks.head()

# create a clean data frame for the regression
cols_to_keep = ['admit', 'gre', 'gpa']
data = df[cols_to_keep].join(dummy_ranks.ix[:, 'prestige_2':])
data.head()

# manually add the intercept
data['intercept'] = 1.0

train_cols = data.columns[1:]
# Index([gre, gpa, prestige_2, prestige_3, prestige_4], dtype=object)
 
logit = sm.Logit(data['admit'], data[train_cols])
 
# fit the model
result = logit.fit()

print result.summary()
