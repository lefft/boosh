import numpy
vul2 = nx.closeness_vitality(g)
mvul2 = list(vul2.items())
avul2 = numpy.array(mvul2)
x = avul2[:, 1]
mbet = list(bet.items())
abet = numpy.array(mbet)
y = abet[:, 1]
from scipy import stats 
slope, intercept, r_value, p_value, std_err = stats.linregress(x,y)
r2 = r_value * r_value
print r2