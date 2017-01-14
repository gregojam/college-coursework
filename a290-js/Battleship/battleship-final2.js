var numShips = 4;
// make board
var board = new Array(7);
for(i = 0; i < 7; i++){
	board[i] = new Array(7);
}

// place ships
for(i = 0; i < numShips; i++){
	var o = Math.floor(Math.random() * 2);
	var tryAgain = true;
	while(tryAgain){
		var x = Math.floor(Math.random() * (5 + 2*((o+1)%2)));
		var y = Math.floor(Math.random() * (5 + 2*o));
		if(board[x][y] == undefined &&
		   board[x+o][y+(o+1)%2] == undefined &&
		   board[x+2*o][y+2*((o+1)%2)] == undefined){
			board[x][y] = 1;
			board[x+o][y+(o+1)%2] = 1;
			board[x+2*o][y+2*((o+1)%2)] = 1;
			tryAgain = false;
		}
		else; // try again!
	}
}
		
var guess;
var hits = 0;
var guesses = 0;

while(hits < 3*numShips){
	guess = prompt("Ready, aim, fire!" + 
			" (enter two numbers from 0-6, " + 
			"separated by whitespace): ");
	guess = guess.match(/^\s*([0-6])\s+([0-6])\s*$/);
	if(guess == null)
		alert("Please enter a valid cell number!");
	else{
		guesses++;
		if(board[guess[1]][guess[2]] == 1){
			alert("HIT!");
			board[guess[1]][guess[2]] = 0;
			hits++
		}
		else if(board[guess[1]][guess[2]] == 0)
			alert("ALREADY HIT");
		else
			alert("MISS");
	}
}
alert("You took " + guesses + " guess to sink the battleships, " +
	"which means your shooting accuracy was " +
	(100 * (3 / guesses)) + "%");
