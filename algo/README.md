B403 structure


Compile myStructure.c as follows:

$ gcc myStructure.c -o myStructure


Running the resulting executable brings up a help menu, with all options
available, and a command line. Files created as a result of reading
instructions from a (properly formatted) input file, will be written to
a file with a .res extension (e.g. reading Input.txt will write Input.res).

~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Also included is a file checker. It parses two files (formatted the same as
the .res files), and checks that they are equivalent to one another.

To use the provided file checker, compile checker.c as follows:

$ gcc checker.c -o checker

To use the file checker, run the checker executable, then type to two files
you wish to compare, separated by a space. The program will print "success1"
if the first file opens correctly, "success2" if the second file opens
correctly, and then "true" or "false" accordingly.
