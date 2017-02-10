import networkx as nx
import matplotlib.pyplot as plt
class MyNode(object):

    def __init__(self, nodeID):
        self.node = nodeID
        self.color = 0

    def color(self):
        self.color = 1

    def uncolor(self):
        self.uncolor = 0

    def get_color(self):
        return color

    def get_nodeID(self):
        return nodeID


class MyStack:

    def __init__(self):
        self.stack = []

    def empty(self):
        return self.stack == []

    def pop(self):
        if self.empty():
            return None
        else:
            item = self.stack.pop()
            return item

    def push(self, item):
        self.stack.append(item)

class MyGraph(object):

    def __init__(self, adj_list=None):
        if adj_list == None:
            adj_list = {}
        else:
            self.adj_list = adj_list

    def add_node(self, node):
        if node not in self.adj_list.keys():
            self.adj_list[node] = []

    def add_edge(self, node1, node2):
        """ You provide code here
        """

    def get_edges(self):
        edge_list = [(x,y) for x in self.adj_list.keys() for y in self.adj_list[x]]
        return edge_list

    def get_nodes(self):
        node_list = [i for i in self.adj_list.keys()]
        return node_list
    
    def DFS(self, node):
        visited = []
        reachable = [node]
        s = MyStack()
        s.push(node)
        while not s.empty():
            v = s.pop()
            if v not in reachable:
                reachable.append(v)
            if v not in visited:
                visited.append(v)
                for w in self.adj_list[v]:
                    s.push(w)
        return reachable

    def reach(self, node1, node2):
        return (node1 in self.DFS(node2))
          
    def draw(self):
        myG = nx.Graph()
        myG.add_edges_from(self.get_edges())
        edgeless = []
        for v in self.adj_list.keys():
            if self.adj_list[v] == []:
                edgeless.append(v)
        if edgeless != []:
            myG.add_nodes_from(edgeless)
        mylabel = {}
        for i in self.get_nodes():
             mylabel[i] = i
       
        pos = nx.spring_layout(myG)
        
        nx.draw_networkx_nodes(myG,pos, nodelist=self.get_nodes(), node_color='b', node_size=500, alpha=0.9)
        nx.draw_networkx_edges(myG,pos, width = 2, edge_list = None, edge_color='r')
        plt.axis('off')
        nx.draw_networkx_labels(myG,pos, mylabel, font_size=16, font_color='w')
        plt.show()   
        plt.close()

#mg = {1 : [2], 2: [3,1], 3: [1]}

#f = MyGraph(mg)
#f.draw()

mg2 = {1 : [2], 2: [1,3,4,5], 3: [2,4,5], 4 : [2,3,5,9], 5 : [2,3,4,9], 6 : [7,8], 7 : [6], 8 : [6], 9:[4,5]}

f2 = MyGraph(mg2)


f2.draw()

print(f2.get_edges())
print(f2.get_nodes())
for i in range(1,9):
    print(" Reachable from {0} is {1}".format(i, f2.DFS(i)))


print(f2.reach(1,8))
print(f2.reach(1,9))

#f2.draw()

#f.add_edge(4,1)

#print(f.get_edges())
#print(f.get_nodes())

#f.draw()
#G = nx.Graph()
#G.add_edges_from([(1,2),(2,3),(3,4),(5,3),(1,5)])

#label = {1 : 1, 2 : 2, 3: 3,4:4,5:5}


# Need to create a layout when doing
# separate calls to draw nodes and edges

#pos = nx.spring_layout(G)

#nx.draw_networkx_nodes(G,pos, nodelist=[1,2,3,5], node_color='b', node_size=500, alpha=0.9)
#nx.draw_networkx_edges(G,pos, width = 2, edge_list = None, edge_color='r')
#plt.axis('off')
#nx.draw_networkx_labels(G,pos, label, font_size=16, font_color='w')
#plt.show()