# TIM -- START BY PUTTING THE FUNCS IN A MODULE, STRIP OFF IF NAME MAIN, 
#        MAKE TOP LEVEL, THEN IMPORT THE MOD + RUN TEH ROUTINEZO ~<33 
import pandas as pd
import os
from nltk.corpus import stopwords
import nltk.data
import logging
import numpy as np  # Make sure that numpy is imported
from gensim.models import Word2Vec
from sklearn.ensemble import RandomForestClassifier
from KaggleWord2VecUtility import KagW2V
from Word2Vec_AverageVectors_util import * 



# ****** Read the two training sets and the test set
data_loc = 'data'
train = 'labeledTrainData.tsv'
train_ul = 'unlabeledTrainData.tsv'
test = 'testData.tsv'

train = pd.read_csv(
  os.path.join(data_loc, train), header=0, delimiter="\t", quoting=3)
test = pd.read_csv(
  os.path.join(data_loc, test), header=0, delimiter="\t", quoting=3)
unlabeled_train = pd.read_csv(
  os.path.join(data_loc, train_ul), header=0,  delimiter="\t", quoting=3)


print('read:\n > {} labeled train reviews, '.format(train.review.size), 
      '\n > {} labeled test reviews, and '.format(test.review.size), 
      '\n > {} unlabeled reviews'.format(unlabeled_train.review.size))


# Load the punkt tokenizer
tokenizer = nltk.data.load('tokenizers/punkt/english.pickle')


# ****** Split the labeled and unlabeled training sets into clean sentences
sentences = []  # Initialize an empty list of sentences

print("Parsing sentences from training set")
for review in train["review"]:
  sentences += KagW2V.review_to_sentences(review, tokenizer)

print("Parsing sentences from unlabeled set")
for review in unlabeled_train["review"]:
  sentences += KagW2V.review_to_sentences(review, tokenizer)


# ****** Set parameters and train the word2vec model
#
# Import the built-in logging module and configure it so that Word2Vec
# creates nice output messages
logging.basicConfig(
  format='%(asctime)s : %(levelname)s : %(message)s', level=logging.INFO)

# Set values for various parameters
num_features = 300    # Word vector dimensionality
min_word_count = 40   # Minimum word count
num_workers = 4       # Number of threads to run in parallel
context = 10          # Context window size
downsampling = 1e-3   # Downsample setting for frequent words

# Initialize and train the model (this will take some time)
print("Training Word2Vec model...")
model = Word2Vec(sentences, workers=num_workers,
                 size=num_features, min_count = min_word_count, 
                 window = context, sample = downsampling, seed=1)


# If you don't plan to train the model any further, calling
# init_sims will make the model much more memory-efficient.
model.init_sims(replace=True)

# It can be helpful to create a meaningful model name and
# save the model for later use. You can load it later using Word2Vec.load()
model_name = "300features_40minwords_10context"
model.save(model_name)

model.doesnt_match("man woman child kitchen".split())
model.doesnt_match("france england germany berlin".split())
model.doesnt_match("paris berlin london austria".split())
model.most_similar("man")
model.most_similar("queen")
model.most_similar("awful")

# ****** Create average vectors for the training and test sets
print("Creating average feature vecs for training reviews")

trainDataVecs = getAvgFeatureVecs(getCleanReviews(train), model, num_features)

print("Creating average feature vecs for test reviews")

testDataVecs = getAvgFeatureVecs(getCleanReviews(test), model, num_features)

# ****** Fit a random forest to the training set, then make predictions
# Fit a random forest to the training data, using 100 trees
forest = RandomForestClassifier(n_estimators = 100)

print("Fitting a random forest to labeled training data...")
forest = forest.fit(trainDataVecs, train["sentiment"])

# Test & extract results
result = forest.predict(testDataVecs)

# Write the test results
output = pd.DataFrame(data={"id":test["id"], "sentiment":result})
output.to_csv("Word2Vec_AverageVectors.csv", index=False, quoting=3)
print("Wrote Word2Vec_AverageVectors.csv")


