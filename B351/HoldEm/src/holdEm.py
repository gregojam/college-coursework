from checkHand import bestHand, runBetterThan
from math import floor
import random


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

### Gets whether game is still going ###
def getGameStatus():
    return False

### Gets how many chips I have
def getChips():
    return 200

def bellRand(high):
    oddAdjust = (high+1) % 2
    x = random.randrange((high+1) // 2)
    y = random.randrange((high+1) // 2 + oddAdjust)
    return x + y

#############################################################################
for i in range(13):
    rank = i +2
    if rank == 11:
        rank = "J"
    elif rank == 12:
        rank = "Q"
    elif rank == 13:
        rank = "K"
    elif rank == 14:
        rank = "A"
        
    print("{}, {}, {}, {} = {}".format(i, i+13, i+26, i+39, rank))
print()
print()
#############################################################################


chips = None
gameOn = True
while(gameOn):
    # Get our danged ol' chips
    if chips == None:
        chips = getChips()
        
    # Shuffle Deck
    shuffle()
      
    # Deal our hand
    hand = deal(2)    
    print("In hand : " + str(hand))
    print()
    
    # Deal the flop
    table = deal(3)
    myBest = bestHand(hand + table)
    print("On Table : " + str(table))
    print("Best: ", myBest)
    
    # Find probability of taking or splitting pot
    winProb = runBetterThan(table, hand, myBest)
    print(winProb)
    
    maxBet = floor(winProb * chips)
    print("Max = ", maxBet)
    
    bet = bellRand(maxBet)
    chips -= bet
    print("Bet = " + str(bet))
    
    print()
    
    # Deal the Turn
    table.extend(deal(1))
    myBest = bestHand(hand + table)
    print("On Table : " + str(table))
    print("Best: " + str(myBest))
    
    # Find probability of taking or splitting pot
    winProb = runBetterThan(table, hand, myBest)
    print(winProb)
    
    maxBet = floor(winProb * chips) - bet
    print("Max = ", maxBet)
    
    if maxBet > 0:
        bet = bellRand(maxBet)
    else:
        bet = 0
    chips -= bet
    print("Bet = ", bet)
    
    print()
    
    # Deal the River
    table.extend(deal(1))
    myBest = bestHand(hand + table)
    print("On Table : " + str(table))
    print("Best: ", myBest)
    
    # Find probability of taking or splitting pot
    winProb = runBetterThan(table, hand, myBest)
    print(winProb)
    
    maxBet = floor(winProb * chips) - bet
    print("Max = ", maxBet)
    
    if maxBet > 0:
        bet = bellRand(maxBet)
    else:
        bet = 0
    chips -= bet
    print("Bet = ", bet)
    
    

# ###########################################################################
#     hand = [8, 11]
#     table = [12, 9, 10, 15]
#     beatBy = 0
#     myBest = bestHand(hand + table)
#     
#     winProb = runBetterThan(table, hand, myBest)
#     print(winProb)
# ###########################################################################
    
    gameOn = getGameStatus()
