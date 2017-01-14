#####V.3.4#####

matrix = []

keepGoing = True

print("Enter each coefficient then the constant, one at a time, in the row.\n"
      "Enter 'n' to go to the next row.")
while keepGoing:
    matVal = input()
    if matVal == "n":
        keepGoing = False

    elif matVal.isdigit():
        matrix.append(matVal)

print(matrix)
print("There are {} values in your matrix.".format(len(matrix)))
