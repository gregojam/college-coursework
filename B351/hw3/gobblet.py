import copy
import time
import random


ALPHA = 100000
BETA = 100000
CHOICE = ()
START = 0

"""
A Player is a [ListOf [ListOf Int]]

Inner lists represent each of player's
stacks.
"""
#Player 1's stacks
p1 = [
[4,3,2,1,0],
[4,3,2,1,0],
[4,3,2,1,0]
]

#Player 2's stacks
p2 = [
[9,8,7,6,0],
[9,8,7,6,0],
[9,8,7,6,0]
]

"""
A Board is a  [ListOf [ListOf [ListOf Int]]]

Inner most lists hold all gobblets on a
particular space.
"""
board = [
[[0],[0],[0],[0]],
[[0],[0],[0],[0]],
[[0],[0],[0],[0]],
[[0],[0],[0],[0]]
]

legal = {'A':0, 'B':1, 'C':2, 'D':3}
stacks = {'E':0, 'F':1, 'G':2}


"""
Prints the topmost piece on each space
of a board in a 4x4 square.

Board Player Player -> None
"""
def printBoard(board, plyr1, plyr2):
    label = ('A', 'B', 'C', 'D')
    print "    {}   {}  {}   {}".format(*label)
    
    i = 0
    for row in board:
        print("{}  {}   {}   {}   {}".format(label[i],
        row[0][0], row[1][0], row[2][0], row[3][0]))
        i += 1
        
    print "#########"
    print"      Stacks"
    print"  p1        p2"
    print"E F G    E F G"
    print("{} {} {}    {} {} {}".format(plyr1[0][0],
        plyr1[1][0], plyr1[2][0], plyr2[0][0], plyr2[1][0],
        plyr2[2][0]
        )
    )


"""
Prints the rules of the game.

None -> None
"""
def printRules():
    print
    print
    print "~RULES:"
    print(
        "In turns, players can either put "
        "a new gobblet on the board, move "
        "one of their gobblets already on "
        "the board to any empty space, or "
        "gobble up any smaller size "
        "gobblet with a bigger gobblet "
        "already in play."
    )
    print
    print(
        "If you put a new gobblet in play, "
        "you must place it on an empty "
        "square. However, there is one "
        "exception to this rule: if your "
        "opponent already has 3 gobblets "
        "in a row on the board, you may "
        "gobble up 1 of the 3 pieces in the "
        "line with a gobblet taken directly "
        "from one of your external stacks."
    )
    print
    print(
        "Once a player has touched a "
        "gobblet that is on the board, the "
        "player must play it. If the player "
        "touches a gobblet that cannot "
        "be played, they automatically "
        "lose the game."
    )
    print
    print(
        "The game is declared a draw if "
        "either player encounters an identical "        "board within 2 of their turns."
    )
    print
    print(
        "Player 1's pieces are 1-4, with bigger "
        "numbers being bigger gobblets. "
        "Player 2's pieces are 6-9. "
        "A 0 means there are no pieces present."
    )
    print


"""
Checks if given space belongs to given
player's-pieces.

(Int, Int) [Tupleof Int] Board? -> Bool
"""
def checkOne(a, good, board=board):
    if a[0] < 0 or a[0] > 3 or a[1] < 0 or a[1] > 3:
        return False
    if board[a[0]][a[1]][0] in good:
        return True
    return False


"""
Returns number of differnet num-in-a-row's
a given space is in for a given player.

Int (Int, Int) Player Board? -> Int
"""
def inARow(num, space, player, board=board):
    if player == p1:
        good = (1,2,3,4)
    else:
        good = (6,7,8,9)
        
    numOf = 0

    x = space[0]
    y = space[1]
    
    #Check verticals
    truth = []
    a = [(0,y),(1,y),(2,y),(3,y)]
    for s  in a:
        if checkOne(s, good, board):
            truth.append(1)
    if len(truth) >=num:
        numOf += 1
    
    #Check horizontals
    truth = []
    a = [(x,0),(x,1),(x,2),(x,3)]
    for s  in a:
        if checkOne(s, good, board):
            truth.append(1)
    if len(truth) >=num:
        numOf += 1
    
    #Check diagonal \
    truth = []
    a = [(0,0),(1,1),(2,2),(3,3)]
    if (x,y) in a:
        for s  in a:
            if checkOne(s, good, board):
                truth.append(1)
        if len(truth) >=num:
            numOf += 1
    
    #Check diagonal /
    truth = []
    a = [(0,3),(1,2),(2,1),(3,0)]
    if (x,y) in a:
        for s  in a:
            if checkOne(s, good, board):
                truth.append(1)
        if len(truth) >=num:
            numOf += 1
    
    return numOf


"""
Returns whether or not a given board is a
winning board, and, if so, who won.
Diagonals are only checked over the two
diagonal lines that can win.

Board? -> (Bool, Int)
"""
def boardWon(board=board):
    #Check Horizontals
    for row in board:
        if row[0][0] == 0:
            continue
        elif row[0][0] in (1,2,3,4):
            player = 0
            good = (1,2,3,4)
        else:
            player = 1
            good = (6,7,8,9)
        
        if(
            row[1][0] in good and
            row[2][0] in good and
            row[3][0] in good
        ):
            return (True, player)
        
    #Check Verticals
    for col in range(4):
        if board[0][col][0] == 0:
            continue
        elif board[0][col][0] in (1,2,3,4):
            player = 0
            good = (1,2,3,4)
        else:
            player = 1
            good = (6,7,8,9)
        
        if(
            board[1][col][0] in good and
            board[2][col][0] in good and
            board[3][col][0] in good
        ):
            return (True, player)
        
    #Check diagonal \
    if board[0][0][0] != 0:
        if board[0][0][0] in (1,2,3,4):
            player = 0
            good = (1,2,3,4)
        else:
            player = 1
            good = (6,7,8,9)
        if(
            board[1][1][0] in good and
            board[2][2][0] in good and
            board[3][3][0] in good
        ):
            return (True, player)
    
    #Check diagonal /
    if board[0][3][0] != 0:
        if board[0][3][0] in (1,2,3,4):
            player = 0
            good = (1,2,3,4)
        else:
            player = 1
            good = (6,7,8,9)
        if(
            board[1][2][0] in good and
            board[2][1][0] in good and
            board[3][0][0] in good
        ):
            return (True, player)
        
    return (False, None)


"""
Prompts the user to select a peice to move.
Ensures the chosen piece is legal.

Player -> [TupleOf Str]
"""
def choosePiece(player):
    if player == p1:
        good = (1,2,3,4)
        plyr = 1
    else:
        good = (6,7,8,9)
        plyr = 2

    printBoard(board, p1, p2)
    print
    print "Player {}'s turn!".format(plyr)
    print "Select a piece to move."
    
    needLegal = True
    while needLegal:
        print (
            "Enter row and colum, "
            "separated by a single space "
            "to move a board piece. "
            "Enter stack letter to place a "
            "piece from that stack."
        )
        
        piece = raw_input(">")
        piece = tuple(piece.split(" "))
        if len(piece) == 2:
            piece = (piece[0].upper(), piece[1].upper())
            if piece[0] in legal and piece[1] in legal: #Check iff on board
                peese = (legal[piece[0]], legal[piece[1]])
                peese = board[peese[0]][peese[1]][0]
                if peese in good:
                    needLegal = False
                elif peese == 0:
                    print
                    printBoard(board, p1, p2)
                    print
                    print "There's no piece there..."
                else:
                    print
                    printBoard(board, p1, p2)
                    print
                    print "That's not your piece!"
            else:
                print
                printBoard(board, p1, p2)
                print
                print "You entered illegal input."
        elif len(piece) == 1:
            piece = (piece[0].upper())
            if piece[0] in stacks:
                peese = stacks[piece]
                peese = player[peese][0]
                if peese > 0:
                    needLegal = False
                else:
                    print
                    printBoard(board, p1, p2)
                    print
                    print "That stack is empty..."
            else:
                print
                printBoard(board, p1, p2)
                print
                print "You entered illegal input."

    print
    return piece


"""
Returns whether or not a move is legal.

[TupleOf Str] Player [TupleOf Str] Board? Bool? -> (Bool, (Int, Int) )
"""
def checkMove(piece, player, move, board=board, prints=True):
    if len(piece) == 2:
        piece = (legal[piece[0]], legal[piece[1]])
        peese = board[piece[0]][piece[1]][0]
    else:
        peese = player[stacks[piece[0]]][0]
    if peese > 5:
        peese -= 5
    
    if(len(move) == 2): #Check legal input format
        move = (move[0].upper(), move[1].upper())
        if move[0] in legal and move[1] in legal: #Check iff on board
            move = (legal[move[0]], legal[move[1]])
            rawTar = board[move[0]][move[1]][0]
            if player == p1:
                op = p2
                bad = (6,7,8,9)
            else:
                op = p1
                bad = (1,2,3,4)
            target = rawTar
            if target > 5:
                target -= 5
            if move == piece: #Check for actually moving
                if prints:
                    print
                    printBoard(board, p1, p2)
                    print
                    print "That wouldn't be a move..."
            elif target >= peese: #Check iff gobblet fits
                if prints:
                    print
                    printBoard(board, p1, p2)
                    print
                    print "You can't move that piece there."
            elif rawTar in bad and len(piece) ==1: #Check for legal cover from stack
                if not inARow(3, move, op):
                    if prints:
                        print
                        printBoard(board, p1, p2)
                        print
                        print "You can't move that piece there!"
                else: #We're good! Move along.
                    return (True, move)
            else: #We're good! Move along.
                return (True, move)
        else:
            if prints:
                print
                printBoard(board, p1 ,p2)
                print
                print "You entered illegal input."
    else:
        if prints:
            print
            printBoard(board, p1, p2)
            print
            print "You entered illegal input."
    return (False, move)


"""
Prompts user to enter move.
Ensures legal move before returning.

[TupleOf Str] Player -> (Int, Int)
"""
def getMove(piece, player):
    printBoard(board, p1, p2)
    print
    print "Place this piece where?"
    
    isLegal = (False, None)
    while not isLegal[0]:
        print(
            "Enter row and column, "
            "separated by single space."
        )
        move = raw_input(">")
        move = tuple(move.split(" "))

        isLegal = checkMove(piece, player, move)

    print
    return isLegal[1]


"""
Returns all of the child boards of a given
board, moving a given piece for a given
player.

[TupleOf Str] Player Board Player Player -> [ListOf (Board, Player, Player)]
"""
def generateChildren(piece, player, board, p1, p2):
    children = []
    
    tmpP1 = copy.deepcopy(p1)
    tmpP2 = copy.deepcopy(p2)
    
    if len(piece) ==2:
        p = (legal[piece[0]], legal[piece[1]])
        peese = board[p[0]][p[1]][0]
    else:
        if player == p1:
            peese = tmpP1[stacks[piece[0]]].pop(0)
        else:
            peese = tmpP2[stacks[piece[0]]].pop(0)
        
    for row in legal:
        for col in legal:
            tmpBoard = copy.deepcopy(board)
            if checkMove(piece, player,  (row, col), tmpBoard, False)[0]:
                if len(piece) == 2:
                    tmpBoard[p[0]][p[1]].pop(0)
                tmpBoard[legal[row]][legal[col]].insert(0, peese)
                children.append((tmpBoard,tmpP1,tmpP2))
            
    return children


"""
Heuristic used by AI to score individual boards.
Each piece is scored based on its weight
(i.e. 1, 2, 3, or 4) and multiplied by 2 or 3, if
it sits within a num-in-a-row. If it sits within a
num-in-a-row of the opponent, is scored as
if it the num-in-a-row is for this player.
Winning boards automatically receive a score
of 1000.

Board Player -> Int
"""
def h(board, player):
    winStatus = boardWon(board)
    if winStatus[0]:
        if winStatus[1] == player:
            return 1000
        else:
            return -1000
        
    score = 0
    
    if player == p1:
        op = p2
        good = (1,2,3,4)
        bad = (6,7,8,9)
    else:
        op = p1
        good = (6,7,8,9)
        bad = (1,2,3,4)
        
    for row in range(4):
        for col in range(4):
            if board[row][col][0] in bad:
                if 4 in bad:
                    num3 = inARow(3, (row,col), op)
                    num2 = inARow(2, (row,col), op)
                    if num3:
                        score -= board[row][col][0] * 3 * num3
                    elif num2:
                        score -= board[row][col][0] * 2 * num2
                    else:
                        score -= board[row][col][0]
                    num3 = inARow(3, (row,col), player)
                    num2 = inARow(2, (row,col), player)
                    if num3:
                        score -= board[row][col][0] * 3 * num3
                    elif num2:
                        score -= board[row][col][0] * 2 * num2
                else:
                    num3 = inARow(3, (row,col), op)
                    num2 = inARow(2, (row,col), op)
                    if num3:
                        score -= (board[row][col][0]-5) * 3 * num3
                    elif num2:
                        score -= (board[row][col][0]-5) * 2 * num2
                    else:
                        score -= (board[row][col][0]-5)
                    num3 = inARow(3, (row,col), player)
                    num2 = inARow(2, (row,col), player)
                    if num3:
                        score -= (board[row][col][0]-5) * 3 * num3
                    elif num2:
                        score -= (board[row][col][0]-5) * 2 * num2
            elif board[row][col][0] in good:
                if 4 in good:
                    num3 = inARow(3, (row,col), player)
                    num2 = inARow(2, (row,col), player)
                    if num3:
                        score += board[row][col][0] * 3 * num3
                    elif num2:
                        score += board[row][col][0] * 2 * num2
                    else:
                        score += board[row][col][0]
                    num3 = inARow(3, (row,col), op)
                    num2 = inARow(2, (row,col), op)
                    if num3:
                        score += board[row][col][0] * 3 * num3
                    elif num2:
                        score += board[row][col][0] * 2 * num2
                else:
                    num3 = inARow(3, (row,col), player)
                    num2 = inARow(2, (row,col), player)
                    if num3:
                        score += (board[row][col][0]-5) * 3 * num3
                    elif num2:
                        score += (board[row][col][0]-5) * 2 * num2
                    else:
                        score += (board[row][col][0]-5)
                    num3 = inARow(3, (row,col), op)
                    num2 = inARow(2, (row,col), op)
                    if num3:
                        score += (board[row][col][0]-5) * 3 * num3
                    elif num2:
                        score += (board[row][col][0]-5) * 2 * num2
                        
    return score


"""
Perfoms minimax with alphaBeta pruning
on a set of boards.
Returns a tuple of a Board and its score

[ListOf (Boards, Int)] Int Int -> (Board, Int)
"""
def search(player, children, depth, myTime, choosing = True):
    global ALPHA
    global BETA
    global CHOICE
    
    if player == p1:
        good = (1,2,3,4)
        bad = (6,7,8,9)
    else:
        good = (6,7,8,9)
        bad = (1,2,3,4)
    
    if depth == 0:
        choice = (None, -100000)
        for ch in children:
            score = h(ch[0], player)
            if score > choice[1]:
                if choosing:
                    CHOICE = ch[0]
                choice = (ch[0], score)
            elif score == choice[1]:
                if choosing:
                    CHOICE = random.choice( [ch[0], CHOICE] )
            if player == p1 and score > ALPHA:
                break
            if player == p2 and score > BETA:
                break
    else:
        choice = (None, 100000)
        for ch in children:
            winStatus = boardWon(ch[0])
            if winStatus[0]:
                if( 
                    (player == p1 and winStatus[1] == 0) or
                    (player == p2 and winStatus[1] == 1)
                ):
                    return (ch[0], 1000)
            
            if player == p1:
                opponent = p2
                plyr = ch[1]
                op = ch[2]
            else:
                opponent = p1
                plyr = ch[2]
                op = ch[1]
                
            pieces = []
            for x in legal:
                for y in legal:
                    if ch[0][legal[x]][legal[y]][0] in bad:
                        pieces.append((x,y))
            for s in stacks:
                if op[stacks[s]] != []:
                    pieces.append((s,))
                    
            childs = []
            for piece in pieces:
                childs.extend(generateChildren(piece, op, ch[0], ch[1], ch[2]))
                     
            for c in childs:
                if time.time() - START > myTime:
                    return choice
                score = search(opponent, childs, depth-1, myTime, False)
                if score[1] < choice[1]:
                    if choosing:
                        CHOICE = ch[0]
                    choice = (ch[0], score[1])
                    if player == p1:
                        beta = score[1]
                    else:
                        alpha = score[1]
                elif score[1] == choice[1]:
                    if choosing:
                        CHOICE = random.choice( [ch[0], CHOICE] )
                    choice = (ch[0], score[1])
                if player == p1 and score[1] > beta:
                    if choice[0] == None:
                        if choosing:
                            CHOICE = ch[0]
                        choice = (ch[0], score[1])
                    break
                elif player == p2 and score[1] > alpha:
                    if choice[0] == None:
                        if choosing:
                            CHOICE = ch[0]
                        choice = (ch[0], score[1])
                    break

    return choice




"""
Determines which piece from the current
board was moved, and to where it was
moved to reach a given board.

Board -> [ [Str], (Int, Int) ]
"""
def extractMove(target, player):
    if player == p1:
        good = (1,2,3,4)
    else:
        good = (6,7,8,9)
        
    move = [None, None]
    for row in range(4):
        for col in range(4):
            tar = target[row][col][0]
            bor = board[row][col][0]
            if tar != bor:
                if bor not in good:
                    move[1] = (row, col)
                    if move[0] == None:
                        move[0] = (tar,)
                else:
                    if tar in good and tar > bor:
                        move[1] = (row, col)
                        if move[0] == None:
                            move[0] = (tar,)
                    else:
                        move[0] = (row, col)
                        
    skcats = {0:'E', 1:'F', 2:'G'}
    if len(move[0]) == 1:
        for i in range(3):
            if player[i][0] == move[0][0]:
                move[0] = (skcats[i],)
                break
                        
    lagel = {0:'A', 1:'B', 2:'C', 3:'D'}
    if len(move[0]) == 2:
        move[0] = (lagel[move[0][0]], lagel[move[0][1]])
                        
    return tuple(move)


"""
Returns computer player's move.

Player -> ( [TupleOf Str] (Int, Int) )
"""
def compMove(player, depth, myTime):
    global ALPHA
    global BETA
    global CHOICE
    global START
    alpha = 1000000
    beta = 1000000
    CHOICE = ()
    
    print
    printBoard(board, p1, p2)
    
    if player == p1:
        good = (1,2,3,4)
    else:
        good = (6,7,8,9)
        
        
    pieces = []
    for x in legal:
        for y in legal:
            if board[legal[x]][legal[y]][0] in good:
                pieces.append((x,y))
    for s in stacks:
        if player[stacks[s]][0] != 0:
            pieces.append(s)
            
    children = []
    for piece in pieces:
        children.extend( generateChildren(piece, player, board, p1, p2) )
    
    START = time.time()
    target = search(player, children, depth-1, myTime)
    
    if player == p1:
        plyr = 1
    else:
        plyr = 2
 
    move = extractMove(CHOICE, player)
    lagel = {0:'A', 1:'B', 2:'C', 3:'D'}
    m = (lagel[move[1][0]], lagel[move[1][1]])
    
    print
    print "Player {} moved from {} to {}.".format(plyr, move[0], m)
    print
    
    return move


"""
Runs the game.

None -> None
"""
def gobby(plyrs, level= 0, myTime= 0):
    printRules()
    
    p1LastThree = []
    p2LastThree = []
    players = (p1, p2)
    player = 0
    if plyrs == 'h2':
        ai = ()
    elif plyrs == 'hr':
        if level not in (0,1,2):
            raise ValueError("Second argument for gobby() must be 0, 1, or 2 when AI is playing.")
        if myTime < 1:
            raise ValueError("Third argument for gobby() must be >= 1 when AI is playing.")
        ai = (1,)
    elif plyrs == 'rh':
        if level not in (0,1,2):
            raise ValueError("Second argument for gobby() must be 0, 1, or 2 when AI is playing.")
        if time < 1:
            raise ValueError("Third argument for gobby() must be >= 1 when AI is playing.")
        ai = (0,)
    elif plyrs == 'r2':
        ai = (0, 1)
    else:
        raise ValueError("First argument for gobby() must be one of: 'h2', 'hr', 'rh', 'r2'")
    
    myTime = myTime * 60
    gameOn = True
    while gameOn:
        if player in ai:
            move = compMove(players[player],  level + 1, myTime)
            piece = move[0]
            move = move[1]
        else:
            piece = choosePiece(players[player])
            children = generateChildren(piece, players[player], board, p1, p2)
            if children == []:
                print("Chosen piece is unmovable. " \
                    "Player {} wins!".format(1 + (player + 1) % 2))
                break
            else:
                move = getMove(piece, players[player])

        if len(piece) == 2:
            piece = (legal[piece[0]], legal[piece[1]])
            peese = board[piece[0]][piece[1]].pop(0)
        else:
            peese = players[player][stacks[piece[0]]].pop(0)
        board[move[0]][move[1]].insert(0,peese)
        
        winStatus = boardWon()
        if winStatus[0]:
            print "Player {} wins!".format(winStatus[1] + 1)
            gameOn = False
        
        if players[player] == p1:
            if board in p1LastThree:
                print "This game is a draw!"
                gameOn = False
                
            p1LastThree.append(copy.deepcopy(board))
            if len(p1LastThree) > 3:
                p1LastThree.pop(0)
                
        else:
            if board in p2LastThree:
                print "This game is a draw!"
                gameOn = False
                
            p2LastThree.append(copy.deepcopy(board))
            if len(p2LastThree) > 3:
                p2LastThree.pop(0)
            
        player = (player + 1) % 2
        
    printBoard(board, p1, p2)
    print
        


### Run The Game ###
gobby('r2', 2, 2.5)

