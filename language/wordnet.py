# scratch 
from nltk.corpus import wordnet as wn
p = wn.synset('panda.n.01')
hyper = lambda s: s.hypernyms()
prn = lambda l: [*map(lambda h: print(" >> {}".format(h)), l)]

pl = list(p.closure(hyper))
sl = list(wn.synset('dog.n.01').closure(hyper))
prn(sl)


prn(list(wn.synset('queen.n.01').closure(hyper)))
