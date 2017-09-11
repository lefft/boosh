# lots of others too
from sklearn.datasets import load_iris 
# also there is RandomForestRegressor and RandomTreesEmbedding
from sklearn.ensemble import RandomForestClassifier

import pandas as pd
import numpy as np

iris = load_iris()
# type(iris) ~~~> sklearn.datasets.base.Bunch
# iris.data # array of features (not incl species labels)
# iris.feature_names # names of features
# iris.DESCR # metadata
# iris.target # species classification/labels
# iris.target_names # names of species




dat = pd.DataFrame(iris.data, columns=iris.feature_names)
dat.head(5)

# randomly select 80% of the rows as training set
dat['train'] = np.random.uniform(0, 1, len(dat)) <= .8

dat['species'] = pd.Categorical.from_codes(
  iris.target, categories=iris.target_names, ordered=False
)
dat.head(3)

train = dat[dat['train']==True]
test = dat[dat['train']==False]

feat = dat.columns[:4]

rando_clf = RandomForestClassifier(n_jobs=2) 

# figure out this syntax -- maybe has to do w n_jobs=2 arg in clf??
y, _ = pd.factorize(train['species'])


rando_clf.fit(train[feat], y)

predictions = iris.target_names[rando_clf.predict(test[feat])]
pd.crosstab(test['species'], predictions, rownames=['actual'], colnames=['predictions'])

