twoChart = {
    (12,12,False):0, (12,11,True):3, (12,10,True):5, (12,9,True):7, (12,8,True):11, (12,7,True):18,
    (12,6,True):23, (12,5,True):29, (12,4,True):33, (12,3,True):27, (12,2,True):31, (12,1,True):32, (12,0,True):38,
    (12,11,False):10, (12,10,False):17, (12,9,False):26, (12,8,False):41, (12,7,False):75, (12,6,False):90,
    (12,5,False):101, (12,4,False):112, (12,3,False):100, (12,2,False):103, (12,1,False):108, (12,0,False):116,
    
    (11,11,False):1, (11,10,True):6, (11,9,True):8, (11,8,True):13, (11,7,True):21, (11,6,True):36,
    (11,5,True):43, (11,4,True):52, (11,3,True):54, (11,2,True):57, (11,1,True):59, (11,0,True):58,
    (11,10,False):19, (11,9,False):30, (11,8,False):44, (11,7,False):80, (11,6,False):111,
    (11,5,False):121, (11,4,False):124, (11,3,False):127, (11,2,False):131, (11,1,False):132, (11,0,False):134,
    
    (10,10,False):2, (10,9,True):12, (10,8,True):14, (10,7,True):24, (10,6,True):42,
    (10,5,True):60, (10,4,True):65, (10,3,True):68, (10,2,True):70, (10,1,True):71, (10,0,True):74,
    (10,9,False):34, (10,8,False):48, (10,7,False):82, (10,6,False):114, (10,5,False):130,
    (10,4,False):136, (10,3,False):140, (10,2,False):142, (10,1,False):143, (10,0,False):145,
    
    (9,9,False):4, (9,8,True):15, (9,7,True):25, (9,6,True):40, (9,5,True):63,
    (9,4,True):78, (9,3,True):81, (9,2,True):85, (9,1,True):86, (9,0,True):88,
    (9,8,False):46, (9,7,False):79, (9,6,False):107, (9,5,False):128,
    (9,4,False):146, (9,3,False):148, (9,2,False):151, (9,1,False):152, (9,0,False):154,
    
    (8,8,False):9, (8,7,True):22, (8,6,True):37, (8,5,True):56,
    (8,4,True):73, (8,3,True):92, (8,2,True):94, (8,1,True):95, (8,0,True):97,
    (8,7,False):72, (8,6,False):99, (8,5,False):123, (8,4,False):139,
    (8,3,False):156, (8,2,False):157, (8,1,False):159, (8,0,False):161,
    
    (7,7,False):16, (7,6,True):39, (7,5,True):53, (7,4,True):67,
    (7,3,True):87, (7,2,True):105, (7,1,True):106, (7,0,True):110,
    (7,6,False):98, (7,5,False):118, (7,4,False):133,
    (7,3,False):149, (7,2,False):163, (7,1,False):164, (7,0,False):165,
    
    (6,6,False):20, (6,5,True):47, (6,4,True):61,
    (6,3,True):77, (6,2,True):93, (6,1,True):115, (6,0,True):117,
    (6,5,False):113, (6,4,False):125, (6,3,False):138,
    (6,2,False):155, (6,1,False):166, (6,0,False):167,
    
    (5,5,False):28, (5,4,True):55, (5,3,True):66, (5,2,True):84, (5,1,True):102, (5,0,True):119,
    (5,4,False):120, (5,3,False):129, (5,2,False):144, (5,1,False):160, (5,0,False):168,
    
    (4,4,False):35, (4,3,True):62, (4,2,True):69, (4,1,True):89, (4,0,True):109,
    (4,3,False):122, (4,2,False):135, (4,1,False):147, (4,0,False):162,
    
    (3,3,False):45, (3,2,True):64, (3,1,True):76, (3,0,True):91,
    (3,2,False):126, (3,1,False):137, (3,0,False):150,
    
    (2,2,False):49, (2,1,True):83, (2,0,True):96,
    (2,1,False):141, (2,0,False):153,
    
    (1,1,False):51, (1,0,True):104,
    (1,0,False):158,
    
    (0,0,False):50
    }

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
    if len(table) == 0:
        a = hand[0]
        b = hand[1]
        if a%13 < b%13:
            tmp = a
            a = b
            b = tmp
        total = 169
        beatBy = twoChart[(a%13, b%13, a//13 == b//13)]       
    elif len(table) == 3:
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

    hand = [10, 9, 24, 20, 36]

    convertedHand = []
    rHand = []
    for card in hand:
        convertedHand.append(deck[card].face)
        rHand.append(card %13)
    
    best = bestHand(hand)
    
    print("hand = ", hand)
    print("convertedHand = ", convertedHand)
    print("rHand = ", rHand)
    print("rDict = ", rankDict(hand))
    print("bestOfAKind = ", bestOfAKind(hand))
    print("bestStraight = ", bestStraight(hand))
    print("bestFlush = ", bestFlush(hand))
    print("bestHand = ", best)
    
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
    
    
    # Check twoChart is valid
    assert len(set(twoChart.values()))
    vals = twoChart.values()
    for x in range(169):
        assert x in vals
    ks = twoChart.keys()
    for x in range(12, -1, -1):
        assert (x, x, False) in ks
        for y in range(x-1, 0, -1):
            assert (x, y, True) in ks
            assert (x, y, False) in ks
    