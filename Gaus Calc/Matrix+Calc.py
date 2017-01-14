#####V3.4#####

"""Matrix Creation"""

matrix = [] #create empty, 1D array for matrix
row = []    #create empty array for row values


print("Enter the augmented matrix, one value at\n"
      "a time and row-by-row, for the system of\n"
      "linear equations you are trying to solve.\n"
      "Enter 'n' to move to the next row and 'f'\n"
      "when you are finished forming your matrix.\n")

rowNum = 1 #create row number counter
colNum = 1 #create column number counter
keepGoing = True
while keepGoing:
    matVal = input("Enter value for row {}, column {}.  ".format(rowNum, colNum))

    if matVal == "n":
        matrix.append(row)  #add row to matrix
        rowNum += 1         #making matrix 2D, move to
        colNum = 1          #next row formation, reset
        row = []            #colNum, and clear row           

    elif matVal == "f":
        matrix.append(row) #add final 'row' to 'matrix'
        keepGoing = False #exit matrix formation

    else:
        try:
            row.append(float(matVal))
            colNum += 1

        except:
            print("Not a valid character. Try again.")

checkMat = matrix

for row in range(len(checkMat)):
    for col in range(len(checkMat[row])):
        if matrix[row][col] == int(matrix[row][col]):
           matrix[row][col] = int(matrix[row][col])

print("\nYour matrix is:\n")
for row in range(len(matrix)):
    print(matrix[row])
##################################################################
"""Gaussian Elimination"""

for row in range(len(matrix)):  #steps through rows
    for count in range(1, len(matrix)):
        try:
            multiple = matrix[row][row]
            otherMult = matrix[row + count][row]

            if multiple != 0 and otherMult != 0:
                for col in range(len(matrix[row])): #steps through columns within rows
                    matrix[row + count][col] = (matrix[row][col] * otherMult -
                                                matrix[row + count][col] * multiple)
        except:
            "hi"

    for count in range(1, len(matrix)):
        try:
            multiple = matrix[row][row]
            otherMult = matrix[row - count][row]

            if multiple != 0 and otherMult != 0:
                for col in range(len(matrix[row])):
                    matrix[row - count][col] = (matrix[row][col] * otherMult -
                                                matrix[row - count][col] * multiple)

        except:
            "hi"

for row in range(len(matrix)):
    divisor = matrix[row][row]
    for col in range(len(matrix[row])):
        try:
            matrix[row][col] /= divisor

        except:
            "hi"

        if matrix[row][col] == int(matrix[row][col]):
           matrix[row][col] = int(matrix[row][col])
        
print("\nYour matrix, in reduced row echelon form, is:\n")
for row in range(len(matrix)):
    print(matrix[row])
