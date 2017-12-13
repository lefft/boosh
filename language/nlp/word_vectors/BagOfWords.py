### PART 1 ## ~~ ## ~~ ## ~~ ## ~~ ## ~~ ## ~~ ## ~~ ## ~~ ## ~~ ## ~~ ## ~~ 
import os
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.ensemble import RandomForestClassifier
from KaggleWord2VecUtility import KagW2V
import pandas as pd
import numpy as np

train = 'labeledTrainData.tsv'
test = 'testData.tsv'
data_loc = 'data'
bow_out = 'bow_out.csv'

# read in test and train data 
train = pd.read_csv(
  os.path.join(data_loc, train), header=0, delimiter="\t", quoting=3)

test = pd.read_csv(
  os.path.join(data_loc, test), header=0, delimiter="\t", quoting=3)

print('first review:', train["review"][0])
# nltk.download()  # Download text data sets, including stop words




# Initialize an empty list to hold the clean reviews
clean_train_reviews = []

# Loop over each review; create an index i that goes from 0 to the length
# of the movie review list
# "Cleaning and parsing the training set movie reviews...\n"
for i in range( 0, len(train["review"])): # <-- was `xrange()`
  clean_train_reviews.append(
    " ".join(KagW2V.review_to_wordlist(train["review"][i], True)))
# NOTE: WARNING ABOUT HTML PARSING (ine 3 of the file <stdin>)
# he code that caused this warning is on line 3 of the file <stdin>. 
# To get rid of this warning, change code that looks like this: 
# BeautifulSoup(YOUR_MARKUP}) to [up(...KUP, "lxml")] 



# ****** Create a bag of words from the training set
# "Creating the bag of words...\n"

# Initialize the "CountVectorizer" object, which is scikit-learn's
# bag of words tool.
vectorizer = CountVectorizer(analyzer = "word",
                             tokenizer = None,
                             preprocessor = None,
                             stop_words = None,
                             max_features = 5000)


# fit_transform() does two functions: First, it fits the model and learns the 
# vocabulary; second, it transforms our training data into feature vectors. 
# The input to fit_transform should be a list of strings.
train_data_features = vectorizer.fit_transform(clean_train_reviews)

# Numpy arrays are easy to work with, so convert the result to an array
np.asarray(train_data_features)




# ******* Train a random forest using the bag of words
# "Training the random forest (this may take a while)..."

# Initialize a Random Forest classifier with 100 trees
forest = RandomForestClassifier(n_estimators = 100)

# Fit the forest to the training set, using the bag of words as
# features and the sentiment labels as the response variable
# 
# This may take a few minutes to run
forest = forest.fit(train_data_features, train["sentiment"])

# Create an empty list and append the clean reviews one by one
clean_test_reviews = []

# "Cleaning and parsing the test set movie reviews...\n"
for i in range(0,len(test["review"])):
  clean_test_reviews.append(
    " ".join(KagW2V.review_to_wordlist(test["review"][i], True)))


# Get a bag of words for the test set, and convert to a numpy array
test_data_features = vectorizer.transform(clean_test_reviews)
np.asarray(test_data_features)




# Use the random forest to make sentiment label predictions
# "Predicting test labels...\n"
result = forest.predict(test_data_features)

# Copy results to a pandas df with an "id" column and a "sentiment" column
output = pd.DataFrame( data={"id":test["id"], "sentiment":result} )


test['review'][0]
type(test)
test.head(5)
output.head(5)
test.shape
output.shape

# Use pandas to write the comma-separated output file
output.to_csv(os.path.join(data_loc, bow_out), index=False, quoting=3)


