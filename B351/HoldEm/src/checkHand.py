def isRoyalFlush(hand):
    if isStraightFlush(hand):
        return hand[4] % 13 == 12
    return False

def isStraightFlush(hand):
    if hand[0] == -1:
        return False
    suit = None
    for i in range(5):
        if suit == None:
            suit = hand[i] // 13
        if hand[i] != 8 +i or hand[i] // 13 != suit:
            return False
    return True

def is4OfAKind(hand):
    return len({hand[4]%13, hand[3]%13, hand[2]%13, hand[1]%13}) == 1

def isFullHouse(hand):
    return len({hand[4]%13, hand[3]%13, hand[2]%13}) == 1 and hand[1]%13 ==  hand[0]%13

def isFlush(hand):
    suit = hand[0] // 13
    for x in hand:
        if x // 13 != suit:
            return False
    return True

def isStraight(hand):
    for i in range(1, 5):
        if hand[i] %13 != hand[i-1]%13 + 1:
            return False
    return True

def is3OfAKind(hand):
    return len({hand[4]%13, hand[3]%13, hand[2]%13}) == 1

def is2Pair(hand):
    return hand[4]%13 == hand[3]%13 and hand[2]%13 == hand[1]%13

def isPair(hand):
    return hand[4]%13 == hand[3]%13

def betterThan(hand1, hand2):
    h1Check = isStraightFlush(hand1)
    h2Check = isStraightFlush(hand2)
    if h1Check:
        if h2Check:
            return hand1[4]%13 > hand2[4]%13
        return True
    elif h2Check:
        return False
    
    h1Check = is4OfAKind(hand1)
    h2Check = is4OfAKind(hand2)
    if h1Check:
        if h2Check:
            return hand1[4]%13 > hand2[4]%13
        return True
    elif h2Check:
        return False
    
    h1Check = isFullHouse(hand1)
    h2Check = isFullHouse(hand2)
    if h1Check:
        if h2Check:
            if hand1[4] %13 == hand2[4]%13:
                return hand1[3]%13 > hand2[3]%13
            return hand1[4]%13 > hand2[4]%13
        return True
    elif h2Check:
        return False
    
    h1Check = isFlush(hand1)
    h2Check = isFlush(hand2)
    if h1Check:
        if h2Check:
            return hand1[4]%13 > hand2[4]%13
        return True
    elif h2Check:
        return False
    
    h1Check = isStraight(hand1)
    h2Check = isStraight(hand2)
    if h1Check:
        if h2Check:
            return hand1[4]%13 > hand2[4]%13
        return True
    elif h2Check:
        return False
    
    h1Check = is3OfAKind(hand1)
    h2Check = is3OfAKind(hand2)
    if h1Check:
        if h2Check:
            return hand1[4]%13 > hand2[4]%13
        return True
    elif h2Check:
        return False
    
    h1Check = is2Pair(hand1)
    h2Check = is2Pair(hand2)
    if h1Check:
        if h2Check:
            return hand1[4]%13 > hand2[4]%13
        return True
    elif h2Check:
        return False
    
    h1Check = isPair(hand1)
    h2Check = isPair(hand2)
    if h1Check:
        if h2Check:
            return hand1[4]%13 > hand2[4]%13
        return True
    elif h2Check:
        return False
    
    for i in range(4, -1, -1):
        if hand1[i] > hand2[i]:
            return True
        if hand1[i] < hand2[i]:
            return False

def runBetterThan(table, hand, myBest):
    cards = [x for x in range(52)]
    for card in hand + table:
        cards.remove(card)
    
    beatBy = 0
    if len(table) == 3:
        total = 178365
        for i in range(len(cards)):
            for j in range(i, len(cards)):
                for k in range(j, len(cards)):
                    for m in range(k, len(cards)):
                        if len({i, j, k, m}) == 4:
                            thisBest = bestHand(table + [i, j, k, m])
                            if(betterThan(thisBest, myBest)):
                                beatBy += 1
    elif len(table) == 4:
        total = 15180
        for i in range(len(cards)):
            for j in range(i, len(cards)):
                for k in range(j, len(cards)):
                    if len({i, j, k}) == 3:
                        thisBest = bestHand(table + [i, j, k])
                        if(betterThan(thisBest, myBest)):
                            beatBy += 1
    else:
        total = 990
        for i in range(len(cards)):
            for j in range(i, len(cards)):
                if len({i, j}) == 2:
                    thisBest = bestHand(table + [i, j])
                    if(betterThan(thisBest, myBest)):
                        beatBy += 1
    
    return 1 - beatBy / total
    

def rankDict(hand):
    tmpHand = []
    for i in range(len(hand)):
        tmpHand.append(hand[i] % 13)
        
    rDict= {}
    for i in range(len(hand)):
        if tmpHand[i] in rDict:
            rDict[tmpHand[i]].append(hand[i])
        else:
            rDict[tmpHand[i]] = [hand[i]]
    return rDict
            
def bestOfAKind(hand):
    rDict = rankDict(hand)
    ranks = sorted(rDict, key=lambda x: (len(rDict[x]), x), reverse= True)

    tmpHand = []
    for rank in ranks:
        for card in rDict[rank]:
            tmpHand.insert(0, card)
            if len(tmpHand) == 5:
                return tmpHand
        

def bestStraight(hand):
    rHand = {}
    for card in hand:
        rHand[card % 13] = card
    
    best = [-1, -1, -1, -1, -1]
    for card in hand:
        run = [card]
        for j in range(1, 5):
            if card + j in rHand:
                run.append(rHand[card + j])
        if len(run) == 5 and run[4] > best[4]:
            best = run
    return best

def bestFlush(hand):
    tmpHand = []
    for i in range(len(hand)):
        tmpHand.append(hand[i] % 13)
        
    suits = [[], [], [], []]
    for h in hand:
        suits[h//13].append(h)
    
    best = [-1 ,-1, -1 ,-1, -1]
    bestIsStraight = False
    for suit in suits:
        if len(suit) >= 5:
            straight = bestStraight(suit)
            if straight[0] == -1:
                suitIsStraight = False
                suit = sorted(suit)[-5:]
            else:
                suitIsStraight = True
                suit = straight
            if best[0] == -1 or suit[4] % 13 > best[4]%13:
                if suitIsStraight or not bestIsStraight:
                    bestIsStraight = suitIsStraight
                    best = suit
        
    return best

def bestHand(hand):
    flush= bestFlush(hand)
    if isRoyalFlush(flush) or isStraightFlush(flush):
        return flush
    
    ofAKind = bestOfAKind(hand)
    if is4OfAKind(ofAKind) or isFullHouse(ofAKind):
        return ofAKind
    
    if(flush[0] != -1):
        return flush
    
    straight = bestStraight(hand)
    if(straight[0] != -1):
        return straight

    return ofAKind
        

if __name__ == "__main__":
    class Card:
        def __init__(self, rank, suit):
            self.face = ranks[rank] + suits[suit]

    suits = {0 : "C", 1 : "D", 2 : "H", 3 : "S"}
    ranks = {0 : '2', 1: '3', 2: '4', 3:'5', 4:'6', 5:'7', 6:'8', 7:'9', 8:'10', 9:'J', 10:'Q', 11:'K', 12:'A'}
    deck = {}
    for i in range(52):
        deck[i] = Card(i%13, i//13)

    hand = [0, 14, 43, 3, 15]

    convertedHand = []
    rHand = []
    for card in hand:
        convertedHand.append(deck[card].face)
        rHand.append(card %13)
    
    best = bestHand(hand)
    
    print("hand = " + str(hand))
    print("convertedHand = " + str(convertedHand))
    print("rHand = " + str(rHand))
    print("rDict = " + str(rankDict(hand)))
    print("bestOfAKind = " + str(bestOfAKind(hand)))
    print("bestStraight = " + str(bestStraight(hand)))
    print("bestFlush = " + str(bestFlush(hand)))
    print("bestHand = " + str(best))
    
    print()
    print(isRoyalFlush(best))
    print(isStraightFlush(best))
    print(is4OfAKind(best))
    print(isFullHouse(best))
    print(isFlush(best))
    print(isStraight(best))
    print(is3OfAKind(best))
    print(is2Pair(best))
    print(isPair(best))
    
    print()
    print(betterThan(bestHand([26, 39, 14, 15, 16]), bestHand([0, 13, 1, 2, 3])))
    