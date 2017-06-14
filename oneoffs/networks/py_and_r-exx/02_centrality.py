import operator
deg = nx.degree(g)
bet = nx.betweenness_centrality(g)
tri = sorted(bet.items(), reverse = True, key = operator.itemgetter(1))
for i in range(len(tri)):
    print(" " + str(i + 1) + " " + villes[ int(tri[i][0]) ])