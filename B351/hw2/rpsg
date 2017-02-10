import random as rn

#values of rock, paper, scissors
r,p,s = 0,1,2
#dictionary e.g., rock beats scissors
ws = {r:s, p:r, s:p}
#dictionary of play choices
plays = {0: "rock", 1: "paper", 2: "scissors"}

compChips = 100
humanChips = 100

gamehistory = []

print
print("Welcome to Gambler's Rock, Paper, Scissors!")
print("You and the Computer each have $100 to start.")
print("Each player can choose tobet up to the amount of")
print("money the player with the lowest amount of money")
print("has. Then both players must bet an amount of money")
print("equal to the larger of the two chosen bets.")
print("Good luck!")
print

while compChips > 0 and humanChips > 0:
    maxBet = min(compChips, humanChips)
    humanBet = 0
    while humanBet > maxBet or humanBet < 1:
        humanBet = int(input("Set your bet choice. Minimum bet is $1. Maximum bet is ${}. $".format(maxBet)))
    compBet = rn.randint(1, maxBet)
    bet = max(humanBet, compBet)
    print("Computer's bet choice was ${}.".format(compBet))
    print("Bet is set at ${}.".format(bet))
    human = -1
    while human < 0 or human > 2:
        human = int(input("r=0,p=1,s=2 "))
    comp = rn.randrange(0,3,1)

    print
    print("You: {0}, Comp: {1}".format(plays[human], plays[comp]))

    if ws[comp] == human:
        print("Computer won this round.\n")
        compChips += bet
        humanChips -= bet
    elif ws[human] == comp:
        print("You won this round.\n")
        humanChips += bet
        compChips -= bet
    else:
        print("It's a tie.\n")
        
    print("You have ${}. The Computer has ${}.".format(humanChips, compChips))
        
if humanChips:
    print("Computer wins!")
else:
    print("You win!")


