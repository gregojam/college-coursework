from checkHand import bestHand, runBetterThan
from math import floor
import random

#############################################################################
"""
These functions help provide a very basic game simulation for testing purposes.
"""
### Our Deck ###
DECK = []
def shuffle():
    global DECK
    DECK = [x for x in range(52)]
    random.shuffle(DECK)

### Deal a given number of cards ###
def deal(num):
    turned = []
    for _ in range(num):
        turned.append(DECK.pop())
    return turned
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#




#############################################################################
"""
These are dependent on how we were to receive data from the game script.
Change what these return to test this script.
"""
### Gets whether game is still going ###
def getGameStatus():
    return False

### Gets my player name
def getName():
    return 'me'

### Gets how many chips I have ###
def getChips():
    return 200

NUMPLAYERS = 5
### Gets number of players still active this hand ###
def getPlayers():
    return NUMPLAYERS

### Gets the maximum bet allowed ###
def getMaxBet():
    return 1000

### Gets what has happened so far this round ###
def getWhatsHappened():
    global NUMPLAYERS
    choices = ('raise', 'raise', 'fold', 'call', 'call', 'check', 'check')
    happenings = []
    for _ in range(getPlayers()-1):
        happening = random.choice(choices)
        if happening == 'raise':
            happenings.append(('bob', 'raise', random.randrange(1, 21)))
        elif happening == 'fold':
            NUMPLAYERS -= 1
            happenings.append(('bob', 'fold'))
        else:
            happenings.append(('bob', happening))
            
    return happenings
#^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^#


### Returns a random integer with a bell-curve distribution.
###
### Note: returned value is in the closed interval [low, high], if both are
###     provided. Otherwise, the returned value is in the closed interval
###     [0, low].
def bellRand(low, high=None):
    if high == None:
        oddAdjust = low % 2
        x = random.randrange(low // 2 + 1)
        y = random.randrange(low // 2 + 1 + oddAdjust)
    else:
        oddAdjust = (high-low) % 2
        x = random.randrange(low, high // 2 + 1 + low)
        y = random.randrange(low, high // 2 + 1 + low + oddAdjust)
    return x + y

### Decides what I want to do
def takeTurn(whatsHappened, table, hand, myBest, chips):
    # Get number of players still in hand
    opponentsLeft = getPlayers() - 1
    
    canBet = True
    callVal = 0
    for happening in whatsHappened:
        if happening[1] == 'raise':
            if happening[0] == myName:
                canBet = False
            else:
                callVal += happening[2]
        
    
    # Find probability of taking or splitting pot
    winProb = runBetterThan(table, hand, myBest)
    
    # Calculate maximum bet I'm willing to make
    maxWilling = floor(winProb**opponentsLeft * chips)
    
    bet = 0
    if canBet:
        # Place bet randomly, up to my maxWilling
        if maxWilling > callVal:
            bet = bellRand(1, maxWilling - callVal)
    
    # Get the maximum bet allowed, and ensure I don't exceed it
    maxAllowed = getMaxBet()
    if bet > maxAllowed:
        bet = maxAllowed
       
    if callVal > maxWilling:
        return myName, 'fold'
    elif bet > 0:
        return myName, 'raise', bet
    elif callVal > 0:
        return myName, 'call'
    else:
        return myName, 'check'
    
    
    

# Get my danged ol' player name
myName = getName()

# Get my danged ol' chips
chips = getChips()

gameOn = True
while(gameOn):      
    # Shuffle Deck
    shuffle()
    table = []
    
    folded = False
    
    # Deal our hand
    hand = deal(2)  
    print("In hand : ", hand)
    print()
    
    # Simulate opponents' first turns
    whatsHappened = getWhatsHappened()
    print("whatsHappened = ", whatsHappened)
    
    # Take my first turn
    whatIDid = takeTurn(whatsHappened,table, hand, None, chips//2)
    print("whatIDid = ", whatIDid)
    whatsHappened.append(whatIDid)
    
    # Count what was bet
    bet = 0
    for happening in whatsHappened:
        if happening[1] == 'raise':
            bet += happening[2]
            
    # Bookkeeping
    if whatIDid[1] == 'fold':
        folded = True
    else:
        chips -= bet
    
    # Simulate opponents' second turns
    whatsHappened.extend(getWhatsHappened())
    print("whatsHappened = ", whatsHappened)
    
    # Take my second turn

    if not folded:
        whatIDid = takeTurn(whatsHappened, table, hand, None, chips//2)
        print("whatIDid = ", whatIDid)
        
        # Count what was bet
        bet = 0
        for happening in whatsHappened:
            if happening[1] == 'raise':
                bet += happening[2]
                
        # Bookkeeping
        if whatIDid[1] == 'fold':
            folded = True
        else:
            chips -= bet
            
    print()
    
    # Deal the flop
    table = deal(3)
    myBest = bestHand(hand + table)
    print("On Table : ", table)
    print("Best: ", myBest)
    
    # Simulate opponents' first turns
    whatsHappened = getWhatsHappened()
    print("whatsHappened = ", whatsHappened)
    
    if not folded:
        # Take my first turn
        whatIDid = takeTurn(whatsHappened,table, hand, myBest, chips)
        print("whatIDid = ", whatIDid)
        whatsHappened.append(whatIDid)
    
    # Count what was bet
    bet = 0
    for happening in whatsHappened:
        if happening[1] == 'raise':
            bet += happening[2]
            
    # Bookkeeping
    if whatIDid[1] == 'fold':
        folded = True
    else:
        chips -= bet
    
    # Simulate opponents' second turns
    whatsHappened.extend(getWhatsHappened())
    print("whatsHappened = ", whatsHappened)
    
    # Take my second turn
    if not folded:
        whatIDid = takeTurn(whatsHappened, table, hand, myBest, chips)
        print("whatIDid = ", whatIDid)
        
        # Count what was bet
        bet = 0
        for happening in whatsHappened:
            if happening[1] == 'raise':
                bet += happening[2]
                
        # Bookkeeping
        if whatIDid[1] == 'fold':
            folded = True
        else:
            chips -= bet
    
    print()
    
    # Deal the Turn
    table.extend(deal(1))
    myBest = bestHand(hand + table)
    print("On Table : ", table)
    print("Best: ", myBest)
    
    # Simulate opponents' first turns
    whatsHappened = getWhatsHappened()
    print("whatsHappened = ", whatsHappened)
    
    if not folded:
        # Take my first turn
        whatIDid = takeTurn(whatsHappened,table, hand, myBest, chips)
        print("whatIDid = ", whatIDid)
        whatsHappened.append(whatIDid)
    
    # Count what was bet
    bet = 0
    for happening in whatsHappened:
        if happening[1] == 'raise':
            bet += happening[2]
            
    # Bookkeeping
    if whatIDid[1] == 'fold':
        folded = True
    else:
        chips -= bet
    
    # Simulate opponents' second turns
    whatsHappened.extend(getWhatsHappened())
    print("whatsHappened = ", whatsHappened)
    
    # Take my second turn
    if not folded:
        whatIDid = takeTurn(whatsHappened, table, hand, myBest, chips)
        print("whatIDid = ", whatIDid)
        
        # Count what was bet
        bet = 0
        for happening in whatsHappened:
            if happening[1] == 'raise':
                bet += happening[2]
                
        # Bookkeeping
        if whatIDid[1] == 'fold':
            folded = True
        else:
            chips -= bet
    
    print()
    
    # Deal the River
    table.extend(deal(1))
    myBest = bestHand(hand + table)
    print("On Table : ", table)
    print("Best: ", myBest)
    
    # Simulate opponents' first turns
    whatsHappened = getWhatsHappened()
    print("whatsHappened = ", whatsHappened)
    
    if not folded:
        # Take my first turn
        whatIDid = takeTurn(whatsHappened,table, hand, myBest, chips)
        print("whatIDid = ", whatIDid)
        whatsHappened.append(whatIDid)
    
    # Count what was bet
    bet = 0
    for happening in whatsHappened:
        if happening[1] == 'raise':
            bet += happening[2]
            
    # Bookkeeping
    if whatIDid[1] == 'fold':
        folded = True
    else:
        chips -= bet
    
    # Simulate opponents' second turns
    whatsHappened.extend(getWhatsHappened())
    print("whatsHappened = ", whatsHappened)
    
    # Take my second turn
    if not folded:
        whatIDid = takeTurn(whatsHappened, table, hand, myBest, chips)
        print("whatIDid = ", whatIDid)
        
        # Count what was bet
        bet = 0
        for happening in whatsHappened:
            if happening[1] == 'raise':
                bet += happening[2]
                
        # Bookkeeping
        if whatIDid[1] == 'fold':
            folded = True
        else:
            chips -= bet
    
    # Update game status
    gameOn = getGameStatus()
