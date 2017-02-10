import copy

class MyRobot:
    def __init__(self, f):
        file = open(f, 'r')
        self.map = []
        for line in file:
            newLine = []
            newLine.extend(line[:len(line)-1])
            self.map.append(newLine)
        file.close()
        self.width = len(self.map[0])
        self.height = len(self.map)
       
    def findStart(self):
        for i in range(self.width):
            if not int(self.map[self.height-1][i]):
                return (self.height-1, i)
    
    def findExits(self):
        exits = []
        for i in range(self.width):
            if not int(self.map[0][i]):
                exits.append( (0, i) )
        for i in range(self.height-1):
            if not int(self.map[i][0]):
                exits.append( (i, 0) )
            elif not int(self.map[i][self.width-1]):
                exits.append( (i, self.width-1) )            
        return exits
    
    def neighbors(self, point, m):
        neighbors = []
        y = point[0]
        x = point[1]
        if x > 0 and not int(m[y][x-1]):
            neighbors.append( (y, x-1) )
        if x < self.width-1 and not int(m[y][x+1]):
            neighbors.append( (y, x+1) )
        if y > 0 and not int(m[y-1][x]):
            neighbors.append( (y - 1 ,x) )
        if y < self.height-1 and not int(m[y+1][x]):
            neighbors.append( (y+1, x) )
        return neighbors
    
    def allNeighbors(self, point, m):
        neighbors = []
        y = point[0]
        x = point[1]
        if x > 0:
            neighbors.append( (y, x-1) )
        if x < self.width-1:
            neighbors.append( (y, x+1) )
        if y > 0:
            neighbors.append( (y - 1 ,x) )
        if y < self.height-1:
            neighbors.append( (y+1, x) )
        return neighbors
    
    def findPathFrom(self, start, exit):
        tmpMap = copy.deepcopy(self.map)
        point = exit
        frontier = []
        frontier.extend(self.neighbors(exit, tmpMap))
        tmpMap[exit[0]][exit[1]] = 0
        count = 1
        while len(frontier) != 0:
            newFront = []
            while len(frontier) != 0:
                child = frontier.pop()
                tmpMap[child[0]][child[1]] = count
                neighs = self.neighbors(child, tmpMap)
                for ch in neighs:
                    if isinstance(tmpMap[ch[0]][ch[1]], str):
                        tmpMap[ch[0]][ch[1]] = count+1
                        newFront.append(ch)
            frontier.extend(newFront)
            count += 1
        
        count = tmpMap[start[0]][start[1]]
        if isinstance(count, str):
            return []
        path = [start]
        point = start
        while count:
            neighs = self.allNeighbors(point, tmpMap)
            for n in neighs:
                if tmpMap[n[0]][n[1]] < count:
                    count = tmpMap[n[0]][n[1]]
                    path.append(n)
                    point = n
                
        ###Testing###
#        self.printMap(tmpMap)
#        print
#        print(path)
#        print
#        print
        
        return path
    
    def getPath(self):
        start = self.findStart()
        exits = self.findExits()
        if len(exits):
            paths = []
            for exit in exits:
                paths.append(self.findPathFrom(start, exit))
            optimal = paths[0]
            for path in paths:
                if len(path) == 0:
                    continue
                if len(path) < len(optimal):
                    optimal = path
            if len(optimal):
                return (True, optimal)
            return False
        else:
            return False
        
    def getInstructions(self):
        ins = { 1: 'rotate -90', 2: 'rotate 90', 3: 'advance'}
        path = self.getPath();
        heading = 0  # 0 = N, 1 = E, 2 = S, 3 = W
        if not path:
            return False
        instr = ['facing North at {}'.format(path[1][0])]
        for i in range(len(path[1]) - 1):
            curr = path[1][i]
            next = path[1][i+1]
            if next[0] < curr[0]: #up
                while heading != 0:
                    if heading == 1:
                        instr.append(ins[2])
                        heading = 0
                    else:
                        instr.append(ins[1])
                        heading = (heading + 1) % 4
                instr.append(ins[3])
            elif next[0] > curr[0]: #down
                while heading != 2:
                    if heading == 3:
                        instr.append(ins[2])
                        heading = 2
                    else:
                        instr.append(ins[1])
                        heading = (heading +1) % 4
                instr.append(ins[3])
            elif next[1] < curr[1]: #left
                while heading != 3:
                    if heading == 0:
                        instr.append(ins[2])
                        heading = 3
                    else:
                        instr.append(ins[1])
                        heading = (heading + 1) % 4
                instr.append(ins[3])
            else: #right
                while heading != 1:
                    if heading == 2:
                        instr.append(ins[2])
                        heading = 2
                    else:
                        instr.append(ins[1])
                        heading = (heading + 1) % 4
                instr.append(ins[3])
        return (True, instr)
            
    def printMap(self, m):
        for line in m:
                print line
    

#robot = MyRobot('testTrue.txt')
#path = robot.getPath()
#assert path[0]
#assert len(path[1]) == 7
#instr = robot.getInstructions()
#assert instr[0]
#
#robot = MyRobot('testFalse.txt')
#assert not robot.getPath()
#assert not robot.getInstructions()

if __name__ == '__main__':
    file = input("Enter file to read as \"[your file]\":  ")
    robot = MyRobot(file)
    robot.printMap(robot.map)
    print robot.getInstructions()

### Testing ###
#robot.printMap(robot.map)
#print robot.findStart()
#print
#print robot.findExits()
#print
#print robot.neighbors( (0,0), robot.map)
#print robot.neighbors( (robot.height-1, robot.width-1), robot.map )
#print robot.neighbors( (robot.height-1, 4), robot.map )
#print robot.neighbors( (robot.height / 2, robot.width / 2), robot.map )
#print