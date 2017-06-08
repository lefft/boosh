





# Cons3.py, Python simulation example: keep tossing a coin until get 3
# heads in a row; let X be the number of tosses needed; find P(X > 6),
# E(X)

# usage: python Cons3.py

import random # load the library

r = random.Random(98765) # sets random number generator seed
sumx = 0 # sum of all X values
count = 0 # number of times X > 6

# rep = 0,1,...,9999
for rep in range(10000): 
  x = 0
  consechds = 0
  
  while True:
    u = r.uniform(0.0,1.0) # generate random number in (0,1)
    
    if u < 0.5: # heads
      consechds += 1
      
    else: # tails
    consechds = 0
    
    x += 1
if consechds == 3: break
if x > 6: count += 1
sumx += x
print ’probability more than 6 tosses are needed =’, count/10000.0
print ’mean number of tosses to get 3 consecutive heads =’, sumx/10000.0