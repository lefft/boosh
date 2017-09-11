

def findNumber(arr, k):
    for x in range(len(arr)):
      if arr[x]==k: 
        return "yes"
    return "no"

findNumber([1,2,3,4,5], 6)


def oddNumbers(l, r):
  out = []
  for x in range(l, r+1):
    if x % 2 == 1:
      out.append(x)
  return out

oddNumbers(3, 7)


import numpy as np

for x in range(3):
  print(x)


