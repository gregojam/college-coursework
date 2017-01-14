######V2.7######

from itertools import product
from copy import deepcopy
import random

# board is either small_board or big_board_status
def easy(board, choosing_board):
    """
    Weak AI for easy difficulty: Randomly plays any legal move.
    """
    choices = []
    for (i,j) in product(range(3), range(3)):
        if (i,j) not in board:
            choices.append((i,j))
    return random.choice(choices)


def medium(board, big_board_status, player, choosing_board, is_medium):
    """
    Decent AI for medium difficulty: Uses Uniform Monte Carlo
    simulation to score small board move. Very strong play on small
    boards, due to fairly small state space of Tic-Tac-Toe. No outlook on
    how a small board move affects opponent's next play.
    """
    
    scores = {}
    if choosing_board:
        small_board = big_scores(board, big_board_status, player, True)
        if is_medium:
            return small_board
        else:
            return medium(board[small_board], big_board_status, player, False, False)

    else:
        children = generateChildren(board, player)
        for (i,j) in children:
            scores[(i,j)] = small_scores(children[(i,j)], player)
            
        if is_medium:
            if player == 1:
                return max(scores, key = scores.get)
            else:
                return min(scores, key = scores.get)
        else:
            return scores
        

def hard(big_board, big_board_status, last_move, player, choosing_board, just_helping):
    """
    Strong AI for hard difficulty: Uses Uniform Monte Carlo
    simulation to score small board move and compares that to
    the scores said move makes available for opponent's next play.
    Very strong play on small boards while considering how a small
    board move might affect opponent's next play.
    """
    
    if choosing_board:
        return big_scores(big_board, big_board_status, player, False)    
    else:
        choices = medium(big_board[last_move], big_board_status, player, choosing_board, False)
        if len(choices) == 1:
            return choices.keys()[0]        
        else:
            for choice in choices:
                temp_small = deepcopy(big_board[choice])
                temp_big = deepcopy(big_board_status)
                temp_small[choice] = player
                if end_game(temp_small)[0]:
                    temp_big[last_move] = player
                    any_win = end_game(temp_big)
                    if any_win[0]:
                        if any_win[1] == player:
                            if just_helping:
                                return player * 10000
                            else:
                                return choice
                        elif any_win[1] == -player:
                            if just_helping:
                                return -player * 10000
                            else:
                                del choices[choice]
                        else:
                            continue
                if end_game(big_board[choice])[0]:
                    next_scores = medium(big_board, big_board_status, -player, True, False)
                else:
                    next_scores = medium(big_board[choice], big_board_status, -player, False, False)
                if player == 1:
                    choices[choice] -= abs(next_scores[min(next_scores, key = next_scores.get)])
                else:
                    choices[choice] += abs(next_scores[max(next_scores, key = next_scores.get)])
            if just_helping:
                return choices
            else:
                if player == 1:
                    return max(choices, key = choices.get)
                else:
                    return min(choices, key = choices.get)

       
def small_scores(board, player):
    """
    Small board scoring for AI's.
    """
    if end_game(board)[0]:
        score = player * 200 + player
    else:
        score = 0
        for i in range(100):
            temp = deepcopy(board)
            while not end_game(temp)[0]:
                player = -player
                temp[easy(temp, False)] = player
            score += end_game(temp)[1]
    return score


def big_scores(board, big_board_status, player, is_medium):
    """
    Big board scoring for AI's.
    """
    
    scores = {}
    for (i,j) in product(range(3), range(3)):
        if end_game(board[(i,j)])[0]:
            pass
        else:
            if is_medium:
                scores[(i,j)] = medium(board[(i,j)], big_board_status, player, False, False)
            else:
                scores[(i,j)] = hard(board, big_board_status, (i,j), player, False, False)

    if player == 1:
        return max(scores, key = scores.get)
    else:
        return min(scores, key = scores.get)


def AI(ai, big_board, big_board_status, last_move, player, choosing_board):
    """
    AI selector for in-game use.
    """
    
    if ai.upper() == "E":
        if choosing_board:
            return easy(big_board_status, choosing_board)
        else:
            return easy(big_board[last_move], choosing_board)
    if ai.upper() == "M":
        if choosing_board:
            return medium(big_board, big_board_status, player, choosing_board, True)
        else:
            return medium(big_board[last_move], big_board_status, player, choosing_board, True)
    else:
        return hard(big_board, big_board_status, last_move, player, choosing_board, False)

def chooseDifficulty(second_ai):
    """"
    Returns difficulty choice for AI
    """
    
    # Available difficulty choices
    difficulties = ("E", "M", "H")
    while True:
        if second_ai:
            print "Please select difficulty for second AI:"
        else:
            print "\nPlease select a difficulty for AI:"
        difficulty = raw_input("(E)asy, (M)edium, or (H)ard?\n")
        if difficulty.upper() in difficulties:
            break
        else:
            print "{0} is not a valid input.\n".format(difficulty)
    return difficulty


def generateChildren(small_board, player):
    """
    Generate possible children boards from small_board
    """
    
    children = {}
    for (i,j) in product(range(3), range(3)):
        child = deepcopy(small_board)

        # Ensure move is available, then make move and place in children dictionary
        if small_board.get((i,j), 0) == 0:
            child[(i,j)] = player
            children[(i,j)] = child
            
    return children


def ask(board):
    """ Return a correct move else raise a ValueError. """

    # Tuple of two elements, both between and including 0 and 2
    authorized_moves = product(range(3), range(3))
    
    pair = raw_input("(a pair of numbers between 1 and 3 separated by a comma):\n")
    pair = pair.split(",")
    try:
        pair = (int(pair[0]) - 1, int(pair[1]) - 1) # Pair between 0 and 2; player will see between 1 and 3
    except:
        raise ValueError("{0} is not a valid input. Please enter a valid input.".format(",".join(pair)))
    if pair not in authorized_moves:
        raise ValueError("({0}, {1}) is out of bounds.".format(pair[0] + 1, pair[1] + 1))
    elif pair in board:
        raise ValueError("({0}, {1}) was already played.".format(pair[0] + 1, pair[1] + 1))
    else:
        return pair


def draw_small(small_board, numbers = False):
    """
    Returns a list of strings representing each small_board lines.
    If number is True, coordinate are added at the beginning of each line.
    Loop left to right starting in the top left corner.
    """
    
    lines = []
    for j in range(2, -1, -1):
        draw_line = ""
        if numbers:
            draw_line += str(j + 1) + " "
        for i in range(3):
            cell = small_board.get((i,j), 0)
            if cell == 0:
                draw_line += "-"
            elif cell == 1:
                draw_line += "X"
            elif cell == -1:
                draw_line += "O"
            draw_line += " "
        lines.append(draw_line)
    if numbers:
        lines.append("  1 2 3")
        
    return lines


def draw_big(big_board, big_board_status):
    """
    Draw the big_board directly calling print statements.
    The drawing is made 3 lines at a time.
    The following signs are used to help the players:
    if player 1 wins one of the small boards, a big X is drawn instead of the actual small board.
    if player 2 wins one of the small boards, a big O is drawn instead of the actual small board.
    if a small board is a draw, it will be shown as a board filled with "#".
    """
    
    print("  _______________________")
    print(" |       |       |       |")
    for j in range(2, -1, -1):
        line0 = "| "
        line1 = "| "
        line2 = "| "
        for i in range(3):
            # Drawing sugar in case a board is won are stale
            if (i,j) in big_board_status:
                if big_board_status[(i,j)] == 0:
                    line0 += "# # # | "
                    line1 += "# # # | "
                    line2 += "# # # | "
                elif big_board_status[(i,j)] == 1:
                    line0 += "X   X | "
                    line1 += "  X   | "
                    line2 += "X   X | "
                elif big_board_status[(i,j)] == -1:
                    line0 += " OOO  | "
                    line1 += "O   O | "
                    line2 += " OOO  | "
            else:
                line0 += draw_small(big_board[(i,j)])[0] + "| "
                line1 += draw_small(big_board[(i,j)])[1] + "| "
                line2 += draw_small(big_board[(i,j)])[2] + "| "
        print(" " + line0)
        print(str(j + 1) + line1)
        print(" " + line2)
        if j == 0:
            print(" |_______|_______|_______|")
            print("     1       2       3")
        else:
            print(" |_______|_______|_______|")
            print(" |       |       |       |")


def rows(board):
    """
    Generator used in end_game, return first horizontal
    then vertical and finaly the diagonal rows.
    """
    
    for k in range(3):
        row_k = [board.get((i, k), 0) for i in range(3)]
        yield row_k
    for k in range(3):
        col_k = [board.get((k,j), 0) for j in range(3)]
        yield col_k
    dia1 = [board.get((i,i), 0) for i in range(3)]
    yield dia1
    dia2 = [board.get((i,2 - i), 0) for i in range(3)]
    yield dia2


def end_game(board):
    """
    Check if the game has ended. Return a pair (bool, int) where bool is
    True if game has ended and int is the winner's number, 0 if it is a draw.
    """

    # Because unmarked board spaces do not exist in board, winner is
    ## not possible if length is not at least 3, no one has won
    if len(board) < 3:
        return (False, 0)
    else:
        for row in rows(board):
            if 0 in row:
                continue
            elif sum(row) == 3:
                return (True, 1)
            elif sum(row) == -3:
                return (True, -1)
        else:
            
            # Draw condition: the board is full and no one won
            if len(board) == 9:
                return (True, 0)
            else:
                return (False, 0)


def playgame(first_play):
    # Dictionary of dictionary index by pair representing each small_board
    big_board = {(i,j): {} for (i,j) in product(range(3), range(3))}
    
    # Dictionary: no key if the game is still going on,
    ## 0 if game has ended in a draw and 1 or 2 if player 1 or 2 won the sub board (i,j)
    ### "start" is utilized in easy() function
    big_board_status = {}

    # Start as player 1, other player is player -1 for easy scoring
    player = 1
    
    last_move = None

    # Player tiles: first element is empty to make player number correlate with tile index
    playerTiles = [[], "X", "O"]

    if first_play:
        # Welcome Screen
        print("Welcome to Tic-tac-toe-ception!")
        print("1. Each turn you make a move in one of the small boards.")
        print("2. When you get three in a row on a small board, you win that board.")
        print("3. To win the game, you need to win three small boards in a row.")
        print("4. Each player's next small board is determined by the coordinate\n" \
              "   of the previous player's move.\n")

    # Choose number of human players and set AI difficulty
    while True:
        print "Please enter number of human players:"
        num = raw_input("0, 1, or 2?\n")
        if num == "2":
            players = ([], "human", "human")
            break
        elif num == "1":
            difficulty = chooseDifficulty(False)
            players = ([], "human", difficulty)
            break
        elif num == "0":
            difficulty1 = chooseDifficulty(False)
            difficulty2 = chooseDifficulty(True)
            players = ([], difficulty1, difficulty2)
            print "\nX is set to {0}.".format(players[1])
            print "O is set to {0}.".format(players[2])
            break
            
    # Game Loop
    while True:
        # Draw
        draw_big(big_board, big_board_status)
        
        # Show current player
        print("\n       {0} is playing.".format(playerTiles[player]))

        # Show previous players move, if not first move
        if last_move != None:
            print "\nYou're opponent played at " \
                  "({0}, {1}) on board ({2}, {3})".format(last_move[0] + 1, last_move[1] + 1,
                                                          last_board[0] + 1, last_board[1] + 1)

        # Ask for a small board if first turn or player sent to an already won board
        if last_move in big_board_status or last_move == None:
            try:
                print "\nEnter the coordinates of the small board where you will play next:"
                if players[player] == "human":
                    last_move = ask(big_board_status)
                else:
                    last_move = AI(players[player], big_board,
                                   big_board_status, last_move, player, True)
                    print ("{0},{1}".format(last_move[0] + 1, last_move[1] + 1))
            except ValueError as e:
                print(e)
                continue

        # Update small_board
        small_board = big_board[last_move]

        # Show which small board is being played
        print("\nYou are playing in board ({0}, {1}): \n".format(last_move[0] + 1, last_move[1] + 1))

        # Draw small board
        print("\n".join(draw_small(small_board, True)))
        print("")
        
        # Ask for a move
        try:
            print "Enter the coordinates for your next move:\n",
            if players[player] == "human":
                move = ask(small_board)
            else:              
                move = AI(players[player], big_board,
                          big_board_status, last_move, player, False)
                print ("{0},{1}".format(move[0] + 1, move[1] + 1))
        except ValueError as e:
            print(e)
            continue

        # Show player change
        print("\n=====================Player Change=======================\n")
        
        # Update small_board
        small_board[move] = player
        
        # Check for won small board, show if true
        (end, winner) = end_game(small_board)
        if end:
            print("    {0} won board ({1}, {2}).".format(playerTiles[player],
                                                             last_move[0] + 1, last_move[1] + 1))

            # Update big board status
            big_board_status[last_move] = winner
            
            # Check for end game, show if true
            (end, winner) = end_game(big_board_status)
            if end:
                draw_big(big_board, big_board_status)
                if winner == 0:
                    print("Draw -_-")
                    break
                else:
                    print("\n          {0} wins!".format(playerTiles[player]))
                    break
                
        # Switch players, save last move for printing, update last_move
        player = -player
        last_board = last_move
        last_move = move

if __name__ == "__main__":
    playgame(True)
    while(True):
        again = raw_input("\n\nPlay again? (Y)es or (N)o:\n")
        if again.upper() == "Y":
            print "\n\n\n"
            playgame(False)
        elif again.upper() == "N":
            print "\nGoodbye\n\n\n\n\n\n"
            break
        else:
            print "Invalid input.\n"
        
