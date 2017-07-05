
# coding: utf-8

# this is a jupyter nb, figgering out if it makes sense as an ide substitute. 
# 
# assuming i can write in *markdown* + make a list of advantages:
# 
# - so many ppl use it that there must be a reason why
# - output is inline like an rmd doc
# - actually meant to be used interactively
# - seems like the checkpoints cd be useful...
# - ...
# 
# and disadvantages (compared to sublime or rodeo, e.g.):
# 
# - not rly modular (working in browser but python running locally)
# - kinda cornfusing at first
# - no code completion(?) (shd investigate if thar is)
# - dk if is easy to import other modules or use a multi-file proj dir
# - how to easily see number of chars on line??
# - ...
# 
# okee that's it. make a code chunk now.

# In[1]:

boosh = [1,2,3,4,5]
for x in range(len(boosh)):
    print("the {}-th 'boosh'".format(str(boosh[x])))


# In[2]:

# another code "chunk" 
print("blaowwie")


# ### a sec head

# w some more markdown, this time w a [link](http://lefft.xyz). 
# 
# in next chunk will make a plot w fake data (see [documentation](https://docs.python.org/3/library/random.html) for `random` module).

# In[3]:

# going to make a plot in this chunk
# first import the random module for making fake data
import random as rnd
import numpy as np

# going to make 50 random points to plot
num_points = 50

# initialize containers for the random data
xs = np.zeros(num_points)
ys = np.zeros(num_points)

# loop over indices of xs/ys, fill up the containers
for i in range(num_points):
    xs[i] = rnd.randint(a=0, b=1000)
    ys[i] = rnd.uniform(a=0, b=1)

# now peep a few of each
print("\nfirst five x's, pre-sort: {}".format(xs[:5]), 
      "\nfirst five y's, pre-sort: {}".format(ys[:5]))

# now sort them so the plot looks reasonable
xs.sort()
ys.sort()

# and confirm that we sorted it
print("\nfirst five x's, post-sort: {}".format(xs[:5]), 
      "\nfirst five y's, post-sort: {}".format(ys[:5]))


# now make a scatterplot w `matplotlib` (see documentation [here](https://matplotlib.org/api/pyplot_api.html)):

# In[4]:

get_ipython().magic('matplotlib notebook')
import matplotlib.pyplot as plt

# now build the plot (don't think we need numpy here)
# plt.plot(xs, ys) # gives a line plot
plt.scatter(xs, ys)


# In[5]:

from random import choice

# now add a random grouping var and color the points by it
# NOTE: random.choices() is only in >=3.6 (which i thot i had?!)

# initialize container
groups = ["missing" for i in range(num_points)]

# fill the container w random groups
for i in range(num_points):
    groups[i] = choice(["group_A", "group_B"])

# display the first five choices
print("groups of the first five elements: {}".format(groups[:5]))


# In[6]:

# want to remake the plot, but this time with coloring for group
# for now best solution is just to overlay two plots, each w diff color

# NOTE to get the elements instead: print([i for i in groups if i=="group_A"])

# get the indices for "group_A"
idx_A = [i for i in range(len(groups)) if groups[i]=="group_A"]

# get the indices for "group_B"
idx_B = [i for i in range(len(groups)) if groups[i]=="group_B"]

# print first few of each
print("a few A indices: {} \na few B indices: {}".format(idx_A[:3], idx_B[:3]))

# check that they're disjoint
print("\nshould be the empty set: {}".format(set(idx_A).intersection(set(idx_B))))


# In[7]:

# now make the color-coded scatterplot by overlaying two plots
plotA = plt.scatter(xs[idx_A], ys[idx_A], marker="+", c="red", label="group_A")
plotB = plt.scatter(xs[idx_B], ys[idx_B], marker="x", c="blue", label="group_B")

plt.legend(handles=[plotA, plotB], loc=2, scatterpoints=1)

plt.show()


# some linxe for reference: 
# 
# - [sampling w replacement](https://stackoverflow.com/questions/43281886/get-a-random-sample-with-replacement) so question
# - [scatter examples from plotly](https://plot.ly/matplotlib/scatter/)
# - [list comprehension booleans/indices](https://stackoverflow.com/questions/38338427/list-comprehension-putting-booleans-inside-of-list-instead-of-integers)
# - [colored legends (w subplots methinks)](https://matplotlib.org/examples/lines_bars_and_markers/scatter_with_legend.html)
# - [point shapes ("markers")](https://matplotlib.org/api/markers_api.html)
# - 
# 
# and now donezo :p

# In[8]:

# the end <333


# In[ ]:


import pandas as pd

file_name = "http://www.ats.ucla.edu/stat/data/binary.csv"

df = pd.read_csv(file_name)

df.head()


