////////////////////////////
// James Gregory
// gregojam@indiana.edu
//
// N-Puzzle
//
// 10-13-17
///////////////////////////

My app idea was to implement a simple 8-puzzle.

The app would have four views. The first view would be the game board.
The second view would be a settings menu, allowing the user to start a
new game, change user profiles, change the colors of the board, tiles 
and text. The third view will be used
to create new user profiles. The fourth view will be used to load user
profiles. The first two views will be accessible via Tab Bar. The third
view will be accessed through buttons on the second view.

The color settings will be persistent, and saved within user
profiles, which will also be persistent.


I chose to save persistent data in the file directory, as it allowed me
to save an arbitrary number of String:CustomClass pairs in a single file.
This is ideal, as it allows me to maintain the dictionary structure in
its entirety, so I don't have to manually rebuild the dictionary each time
the app launches. The dictionary structure itself is ideal for its O(n)
lookup time and *O(n)* add time.

UIKit instances used:
    UIButton ~ general gameplay, color selection, menu traversal.
    UILabel ~ read-only text, displayed when certain conditions are met.
    UITextField ~ user-entered username and password.
    


All tasks completed

Added a button that starts a new game when pressed, added some warning
messages to screens where user input might be invalid, and added text
that displays when the user has completed a puzzle.
