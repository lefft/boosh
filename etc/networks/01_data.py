### TIM - WHEN U COME BACK TO THIS, START AT PART 4 (LINE 62)
# PART 1 HERE -- "01_data.py"
import networkx as nx
edges = [
  (0,43), (0,56), (1,2), (1,6), (2,5), (2,14), (3,4), (3,17),
  (4,5), (4,16), (5,15), (5,14), (6,7), (6,14), (7,8), (7,14), 
  (8,9), (8,11), (10,11), (10,24), (11,12), (11,24), (11,25), (12,13), 
  (12,23), (13,14), (13,22), (14,21), (15,21), (15,18), (16,17), (16,18), 
  (17,19), (18,20), (19,20), (20,21), (20,35), (20,36), (21,33), (22,26), 
  (22,27), (23,25), (23,26), (24,25), (25,30), (26,31), (27,32), (27,33), 
  (28,29), (28,30), (29,30), (29,43), (30,31), (32,42), (33,34), (35,38), 
  (36,37), (36,38), (37,39), (38,39), (38,41), (38,40), (39,40), (39,47), 
  (40,46), (34,41), (42,43), (42,44), (43,44), (44,55), (45,55), (45,52), 
  (46,51), (47,48), (47,50), (48,49), (48,50), (49,50), (50,51), (51,52), 
  (52,53), (53,54), (53,55), (55,56), (34,42), (34,45), (35,36)
]

cities = [
  "Bayonne","Calais", "Lille", "Strasbourg", "Metz", "Reims", 
  "Amiens", "Rouen", "Caen", "Cherbourg", "Brest", "Rennes", "Le Mans", 
  "Chartres", "Paris", "Troyes", "Nancy", "Mulhouse", "Langres", 
  "Besancon", "Dijon", "Sens", "Orleans", "Angers", "Vannes", "Nantes", 
  "Tours", "Vierzon", "La Rochelle", "Saintes", "Niort", "Poitiers", 
  "Limoges", "Vichy", "Clermont-Fd", "Macon", "Bourg-en-Bresse", "Geneve", 
  "Lyon", "Grenoble", "Valence", "St-Etienne", "Brive", "Bordeaux", "Montauban", 
  "Millau", "Avignon", "Digne-les-bains", "Nice", "Toulon", "Marseille", 
  "Nimes", "Montpellier", "Narbonne", "Perpignan", "Toulouse", "Pau"
]

giraffe = nx.Graph()
giraffe.add_edges_from(edges)

giraffe.edges()


# PART 2 HERE -- "02_centrality.py"
import operator
deg = nx.degree(giraffe)
bet = nx.betweenness_centrality(giraffe)
tri = sorted(bet.items(), reverse = True, key = operator.itemgetter(1))
for i in range(len(tri)):
    print(" " + str(i + 1) + " " + cities[ int(tri[i][0]) ])


# PART 3 HERE -- "03_vulnerability.py"
aspl = nx.average_shortest_path_length(giraffe)
vul = []
for i in range(len(giraffe.nodes())):
    if (i != 8 and i !=53): # exceptions
        g2 = nx.Graph()
        g2.add_edges_from(edges)
        g2.remove_node(i)
        vul = vul + [(i, nx.average_shortest_path_length(g2) - aspl)]

trivul = sorted(vul, reverse = True, key = operator.itemgetter(1))

for i in range(len(trivul)):
    print(" " + str(i + 1) + " " + cities[ int(trivul[i][0]) ])


### TIM - WHEN U COME BACK TO THIS, START AT PART 4 (LINE 62)
# PART 4 HERE -- "04_closeness_vitality.py"
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


### NOW YOU CAN FIDDLE AROUND ONCE YOU GET ALL THE ABOVE PARTS :p
### NOW YOU CAN FIDDLE AROUND ONCE YOU GET ALL THE ABOVE PARTS :p
### NOW YOU CAN FIDDLE AROUND ONCE YOU GET ALL THE ABOVE PARTS :p





