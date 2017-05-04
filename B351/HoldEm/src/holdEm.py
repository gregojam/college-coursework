import checkHand
import random

DECK = []


### Our Deck ###
def shuffle():
    global DECK
    DECK = [x for x in range(52)]
    random.shuffle(DECK)

### Deal a given number of cards ###
def deal(num):
    turned = []
    for i in range(num):
        turned.append(DECK.pop())
    return turned


def getGameStatus():
    return False


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

gameOn = True
while(gameOn):
    # Shuffle Deck
    shuffle()
    
    
    # Deal our hand
    hand = deal(2)    
    print("In hand : " + str(hand))
    print()
    
    # Deal the flop
    table = deal(3)
    myBest = checkHand.bestHand(hand + table)
    print("On Table : " + str(table))
    print("Best: " + str(myBest))
    print()
    
    # Deal the Turn
    table.extend(deal(1))
    myBest = checkHand.bestHand(hand + table)
    print("On Table : " + str(table))
    print("Best: " + str(myBest))
    print()
    
    # Deal the River
    table.extend(deal(1))
    myBest = checkHand.bestHand(hand + table)
    print("On Table : " + str(table))
    print("Best: " + str(myBest))
    

    

    
    hand = [8, 11]
    table = [12, 9, 10, 15]
    beatBy = 0
    myBest = checkHand.bestHand(hand + table)
    
    (beatBy, totalCount) = checkHand.runBetterThan(table, hand, myBest)

                            
    print(beatBy)
    print(totalCount)
    print(beatBy / totalCount)
    
    maxBet = 1 - beatBy / totalCount
    print(maxBet)

    
    gameOn = getGameStatus()

