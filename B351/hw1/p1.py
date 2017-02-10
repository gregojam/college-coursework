#Extra Credit

class MyGraph:
    OUTSIDE = 'U'
    BATTERY = 45
    
    def __init__(self):
        self.adj = {}
    
    #build() : Edges -> Void
    #Builds the adjacency dictionary with values
    #   being a list of adjacent vertices in the same
    #   order they were encountered in edges set.
    def build(self, edges):
        for x in edges:
            if x[0] not in self.adj:
                self.adj[x[0]] = [x[1]]
            else:
                self.adj[x[0]].append(x[1])
            if x[1] in self.adj:
                self.adj[x[1]].append(x[0])
            else:
                self.adj[x[1]] = [x[0]]
    
    #depthSearch() : vertice, vertice? -> Void
    #If no start vertice is given, starts at OUTSIDE.
    def depthFirst(self, goal, start = OUTSIDE):
        #Performs a depth first search from a start
        #   vertice to a goal vertice, and prints the
        #   first path found.
        if start not in self.adj:
            print "START NOT IN GRAPH"
        elif goal not in self.adj:
            print "GOAL NOT IN GRAPH"
        else:
            time, battery, batUsed = 0, 100, 0
            path = [start]
            indexes = {start : 0}
            u = start
            while goal not in path:
                if indexes[u] < len(self.adj[u]):
                    v = self.adj[u][indexes[u]]
                    if v not in path:
                        path.append(v)
                        indexes[v] = 0
                        indexes[u] += 1
                        u = v
                    else:
                        indexes[u] += 1
                else:
                    path.remove(u)
                    u = path[len(path)-1]
                
            #returns to start
            i = 1
            n = len(path) - 1
            while u != start:
                u = path[n-i]
                path.append(u)
                i += 1

            #calculates time of path
            outside = False
            x = 0
            while x < len(path)-1:
                if path[x+1] == self.OUTSIDE:
                    outside = True
                elif outside:
                    time += 13
                    outside = False
                else:
                    time += 6
                x += 1
            
            #calculates battery life left to two
            x = 0
            while x < (time * 100 / self.BATTERY):
                if battery <= 0:
                    batUsed +=1
                    battery += 100
                battery -= 1
                x += 1
  
            print(path)
            print("Time: " + str(time))
            print("Battery Left: " + str(battery) + "%")
            if batUsed > 0:
                print("Battery Changes: " + str(batUsed))
    
    def printThis(self):
        print(self.adj)

#Used for testing
#
#edges = [
#    ['A', 'B'],['B','H'],['B','J'],['C','I'],
#    ['C','U'],['D','F'],['D','K'],['D','L'],['E','F'],
#    ['G','H'],['G','J'],['G','K'],['L','U']
#    ]
#
#mg = MyGraph()
#mg.build(edges)
#
#mg.depthFirst('A')

myGraph = MyGraph()
myGraph.printThis()