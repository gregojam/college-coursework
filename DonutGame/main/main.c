#include <stm32f30x.h>
#include <f3d_lcd_sd.h>
#include <f3d_uart.h>
#include <f3d_i2c.h>
#include <f3d_nunchuk.h>
#include <f3d_delay.h>
#include <f3d_accel.h>
#include <f3d_user_btn.h>
#include "level.h"
#include "audio.h"
#include <diskio.h>
#include <stdio.h>
#include <stdlib.h>
#include <bmp.h>

levelInfo level;
int player[2];
int *portals;
char standingOn;
int keysHeld, donutHeld, lvl, wasReset;
nunchuk_t nunValues;
FATFS Fatfs;		/* File system object */
FIL Fil;		/* File object */
BYTE Buff[128];		/* File read buffer */


void initPortals(){
	int i;
	for(i = 0; i < level.numPortals; i++){
		portals[i*4] = 60;
	}
}

void addPortal(char set, int h, int v){
	int index = (int)set - 48;
	if(portals[index*4] == 60){
		portals[index*4] = h;
		portals[index*4+1] = v;
	}
	else{
		portals[index*4+2] = h;
		portals[index*4+3] = v;
	}
}

void teleport(int h, int v){
	int i = (int)level.map[h][v] - 48;
	f3d_lcd_drawObject(player[0], player[1], standingOn);
	if(portals[i*4] == v && portals[i*4+1] == h){
		f3d_lcd_drawObject(portals[i*4+2],portals[i*4+3], '@');
		player[0] = portals[i*4+2];
		player[1] = portals[i*4+3];
	}
	else if(portals[i*4+2] == v && portals[i*4+3] == h){
		f3d_lcd_drawObject(portals[i*4],portals[i*4+1], '@');
		player[0] = portals[i*4];
		player[1] = portals[i*4+1];
	}
	else;
		//f3d_lcd_drawObject(25, 20, GREEN);
}

char findReplacement(int h, int v){
			char obj = level.map[h][v];
			if(obj == 'x')
				return 'x';
			else if(obj == 'k'){
				keysHeld++;
				level.map[h][v] = ' ';
				return ' ';
			}
			else if(obj == 'd'){
				keysHeld--;
				level.map[h][v] = ' ';
				return ' ';
			}
			else if(obj == 'o') {
				donutHeld++;
				return 'o';
			}
			else if(obj == ' ')
				return ' ';
			else
				return '!';
}

void story() {
	int height;
	int screenHeight = 127;
	int numLines = 7;
	f3d_lcd_fillScreen(GREY);
	if(lvl == 0){
		f3d_lcd_drawString(5,4, "A Long time ago, in a ~classroom very, very near, ~"
					"our hero set out on a ~journey to find the ~greatest dozen "
					"of donuts ~the world had ever known. ~There was but one ~problem... "
					"the Donut God ~was displeased with our ~hero's quest! To hinder ~his "
					"progress, the Donut ~God scattered the donuts", BLACK, GREY);
		delay(11000);
		f3d_lcd_fillScreen(GREY);
		f3d_lcd_drawString(5, 4, "far and wide, and placed ~them in inconvenient to ~reach places. He "
					 "would ~have placed them in ~impossible to reach ~places, buuuut he didn't ~"
					 "really care that much.", BLACK, GREY);

		delay(8000);
		f3d_lcd_fillScreen(GREY);
		f3d_lcd_drawObject(1,2,'@');
		f3d_lcd_drawObject(1,6,'o');
		f3d_lcd_drawObject(1,10,'k');
		f3d_lcd_drawObject(1,14,'d');
		f3d_lcd_drawObject(1,18,'x');
		f3d_lcd_drawObject(1,22,'!');
		f3d_lcd_drawString(12,2*4,"PLAYER",BLACK,GREY);
		f3d_lcd_drawString(12,6*4,"DONUT",BLACK,GREY);
		f3d_lcd_drawString(12,10*4,"KEY",BLACK,GREY);
		f3d_lcd_drawString(12,14*4,"DOOR",BLACK,GREY);
		f3d_lcd_drawString(12,18*4,"WALL",BLACK,GREY);
		f3d_lcd_drawString(12,22*4,"PORTAL",BLACK,GREY);
		f3d_lcd_drawString(12,26*4, "User Button Resets level", BLACK, GREY);
		f3d_lcd_drawString(12,30*4, "GOAL:GET THE DONUT", BLACK, GREY);
		delay(8000);
		f3d_lcd_fillScreen(GREY);
		f3d_lcd_drawString(59, 49, "Level 1", PURPLE, GREY);
		f3d_lcd_drawString(32, 59, "More Than A Donut", BROWN, GREY);
	}
	else if(lvl == 1){
		f3d_lcd_drawString(59, 49, "Level 2", PURPLE, GREY);
		f3d_lcd_drawString(32, 59, "More Than A Level", BROWN, GREY);
	}
	else if(lvl == 2){
		f3d_lcd_drawString(59, 49, "Level 3", PURPLE, GREY);
		f3d_lcd_drawString(27, 59, "More Than A Corner", BROWN, GREY);
	}
	else if(lvl == 3){
		f3d_lcd_drawString(59, 49, "Level 4", PURPLE, GREY);
		f3d_lcd_drawString(20, 59, "More Than A Joystick", BROWN, GREY);
	}
	else if(lvl == 4){
		f3d_lcd_drawString(59, 49, "Level 5", PURPLE, GREY);
		f3d_lcd_drawString(37, 59, "More Than A Door", BROWN, GREY);
	}
	else if(lvl == 5){
		f3d_lcd_drawString(59, 49, "Level 6", PURPLE, GREY);
		f3d_lcd_drawString(39, 59, "More Than A Key", BROWN, GREY);
	}
	else if(lvl == 6){
		f3d_lcd_drawString(59, 49, "Level 7", PURPLE, GREY);
		f3d_lcd_drawString(32, 59, "More Than A Start", BROWN, GREY);
	}
	else if(lvl == 7){
		f3d_lcd_drawString(59, 49, "Level 8", PURPLE, GREY);
		f3d_lcd_drawString(32, 59, "More Than A Frown", BROWN, GREY);
	}
	else if(lvl == 8){
		f3d_lcd_drawString(59, 49, "Level 9", PURPLE, GREY);
		f3d_lcd_drawString(22, 59, "More Than A Hallway", BROWN, GREY);
	}
	else if(lvl == 9){
		f3d_lcd_drawString(59, 49, "Level 10", PURPLE, GREY);
		f3d_lcd_drawString(27, 59, "More Than A Portal", BROWN, GREY);
	}
	else if(lvl == 10){
		f3d_lcd_drawString(59, 49, "Level 11", PURPLE, GREY);
		f3d_lcd_drawString(39, 59, "More Than A Gym", BROWN, GREY);
	}
	else if(lvl == 11){
		f3d_lcd_drawString(59, 49, "Level 12", PURPLE, GREY);
		f3d_lcd_drawString(32, 59, "More Than A Shake", BROWN, GREY);
	}
	else
		f3d_lcd_drawString(5, 4, "As our hero collected the ~12th and final donut "
					 "for ~his dozen. The Donut God ~appeared before him, "
					 "and ~said \"Meh... I guess you ~can keep the donuts.\" "
					 "To ~which our hero replied ~\"They're not for me. ~"
					 "They're for my lab!\"", BLACK, GREY);

	delay(3000);
	if(lvl == 12)
		delay(7000);
}

void loadLevel() {	
	f3d_lcd_fillScreen(GREY);

	switch(lvl){
		case 0:
			level = LEVEL_1;
			break;
		case 1:
			level = LEVEL_2;
			break;
		case 2:
			level = LEVEL_3;
			break;
		case 3:
			level = LEVEL_4;
			break;
		case 4:
			level = LEVEL_5;
			break;
		case 5:
			level = LEVEL_6;
			break;
		case 6:
			level = LEVEL_7;
			break;
		case 7:
			level = LEVEL_8;
			break;
		case 8:
			level = LEVEL_9;
			break;
		case 9:
			level = LEVEL_10;
			break;
		case 10:
			level = LEVEL_11;
			break;
		case 11:
			level = LEVEL_12;
	}

	player[0] = level.playerStart[0];
	player[1] = level.playerStart[1];
	portals = malloc(4 * level.numPortals * sizeof(int));
	standingOn = ' ';
	initPortals();
	f3d_lcd_drawObject(player[0], player[1], '@');

	/***************LEVEL 7*********************/
	if(lvl == 6 && wasReset){
		level.map[23][7] = ' ';
		level.map[23][20] = 'o';
	}
	/*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*/

	// (v)ertical and (h)orizontal from view of play
	int v, h;
	for(v = 0; v < 32; v++){
		for(h = 0; h < 40; h++){
			if(level.map[v][h] == 'x' || level.map[v][h] == 'd' ||
				 level.map[v][h] == 'k' || level.map[v][h] == 'o')
				f3d_lcd_drawObject(h,v,level.map[v][h]);
			else if(level.map[v][h] == ' ' || level.map[v][h] == '@')
				continue;
			else {
				addPortal(level.map[v][h], h, v); 
				f3d_lcd_drawObject(h, v, level.map[v][h]);
			}
		}
	}
}

void drawPicture(void) {
	char *picture = "sean.bmp";
	f_mount(0, &Fatfs);		/* Register volume work area (never fails) */
	FRESULT rc;			/* Result code */
  	DIR dir;			/* Directory object */
  	FILINFO fno;			/* File information object */
  	UINT bw, br;
  	unsigned int retval;
  	int x, y;
	uint16_t colors[128];
  	struct bmpfile_magic magic;
  	struct bmpfile_header header;
  	BITMAPINFOHEADER info;
  	struct bmppixel pixel[128];

	rc = f_close(&Fil);
    rc = f_open(&Fil, picture, FA_READ);
    rc = f_read(&Fil, &magic, sizeof(magic), &br);	/* Read a chunk of file */
    rc = f_read(&Fil, &header, sizeof(header), &br);
    rc = f_read(&Fil, &info, sizeof(info), &br);

	f3d_lcd_setAddrWindow (16, 0, 143, 127, MADRIGHT);
	   for(y = 127;y >= 0; y--) {
	    rc = f_read(&Fil, (void *) &pixel, sizeof pixel, &br);
	    if (rc || !br) break;
	    for (x = 127; x >= 0; x--){
			colors[x] = ((pixel[x].b >> 3) << 11) | ((pixel[x].g >> 2) << 5) | (pixel[x].r >> 3);
	    }
      if(y < 143)
		f3d_lcd_pushColor(colors, 128);
    }
}


int main(void){
	f3d_uart_init();
	f3d_lcd_init();
	f3d_i2c1_init();
	f3d_nunchuk_init();
	f3d_delay_init();
	f3d_accel_init();
	f3d_user_btn_init();

	lvl = 0;
	story();
	loadLevel();
	
	int slowDown = 0;
	keysHeld = 0;
	donutHeld = 0;
	float accData[3];
	int numShakes = 20;
	float lastShake;
	int donutThere = 0;
	int shakeTimer = 5;
	int zPressed = 0;
	wasReset = 0;
	int resetPressed = 0;
	
	while(1){
		f3d_nunchuk_read(&nunValues);//getting nunchuk values

		/************RESET LEVEL********************/
		if(user_btn_read() && !resetPressed){
			wasReset = 1;			
			keysHeld = 0;
			donutHeld = 0;
			donutThere = 0;
			resetPressed = 1;
			free(portals);
			loadLevel();	


		}
		else if(user_btn_read())
			continue;
		else
			resetPressed = 0;
		/*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*/

		/*********LEVEL 4***************************/
		if(lvl == 3){
			if(!donutThere){
				addPortal('1', portals[2], portals[3]);
				donutThere = 1;
			}
			if((int)nunValues.z && !zPressed){
				if(level.map[22][20] == '0')
					level.map[22][20] = '1';
				else
					level.map[22][20] = '0';
				zPressed = 1;
			}
			else if((int)nunValues.z)
				continue;
			else
				zPressed = 0;
		}
		/*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*/

		/********************LEVEL 5****************/
		if(lvl == 4)
			keysHeld = 1;
		/*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*/


		/************LEVEL 12***********************/
		if(lvl == 11 && donutThere == 0) {
			f3d_accel_read(accData);
			if(lastShake < 0){
				if(accData[0] > 0){
					shakeTimer = 5;
					numShakes--;
				}
			}
			else if(lastShake > 0){
				if(accData[0] < 0){
					shakeTimer = 5;
					numShakes--;
				}
			}
			else
				shakeTimer--;

			if(shakeTimer <= 0)
				numShakes = 20;

			if(numShakes <= 0){
				level.map[12][12] = 'o';
				f3d_lcd_drawObject(12, 12, 'o');
				donutThere = 1;
			}

			lastShake = accData[0];
		}
		/*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*/

		/*********************END GAME**************/
		if(lvl == 12){
			free(portals);
			drawPicture();
			break;
		}
		
		/***********NEXT LEVEL********/
		if(donutHeld == 1) {
			lvl++;
			playSound(0);
			free(portals);
			story();
			loadLevel();
			keysHeld = 0;
			donutHeld = 0;
			donutThere = 0;
			wasReset = 0;
		}
		/*^^^^^^^^^^^^^^^^^^^^^^^^^^^*/

		/*************RECORDING MOVEMENT****************************************/
		if(nunValues.jy == '\xFF' && slowDown > 50){ //move up
			if(level.map[player[1]-1][player[0]] == 'x'
				|| (level.map[player[1]-1][player[0]] == 'd' && keysHeld == 0)
				|| player[1] < 1); // wall
			else{
				char temp = findReplacement(player[1]-1, player[0]);
				if(temp == '!')
					teleport(player[1]-1, player[0]);
				else{
					f3d_lcd_drawObject(player[0], player[1], standingOn);
					f3d_lcd_drawObject(player[0],--player[1], '@');
				}
				standingOn = temp;
				slowDown = 0;
			}
		}
		else if(nunValues.jy == '\x00' && slowDown > 50){ //move down
			if(level.map[player[1]+1][player[0]] == 'x'
				|| (level.map[player[1]+1][player[0]] == 'd' && keysHeld == 0)
				|| player[1] > 30); // wall
			else{
				char temp = findReplacement(player[1]+1, player[0]);
				if(temp == '!')
					teleport(player[1]+1, player[0]);
				else{
					f3d_lcd_drawObject(player[0], player[1], standingOn);
					f3d_lcd_drawObject(player[0],++player[1], '@');
				}
				standingOn = temp;
				slowDown = 0;
			}
		}
		if(nunValues.jx == '\xFF' && slowDown > 50){ //move right
			if(level.map[player[1]][player[0]+1] == 'x'
				|| (level.map[player[1]][player[0]+1] == 'd' && keysHeld == 0)
				|| player[0] > 38); // wall
			else{
				char temp = findReplacement(player[1], player[0]+1);
				if(temp == '!'){
					teleport(player[1], player[0]+1);
				}
				else{
					f3d_lcd_drawObject(player[0], player[1], standingOn);
					f3d_lcd_drawObject(++player[0],player[1], '@');
				}
				standingOn = temp;
				slowDown = 0;
			}
		}
		else if(nunValues.jx == '\x00' && slowDown > 50){ //move left
			if(level.map[player[1]][player[0]-1] == 'x'
				|| (level.map[player[1]][player[0]-1] == 'd' && keysHeld == 0)
				|| player[0] < 1); // wall
			else{
				char temp = findReplacement(player[1], player[0]-1);
				if(temp == '!')
					teleport(player[1], player[0]-1);
				else{
					f3d_lcd_drawObject(player[0], player[1], standingOn);
					f3d_lcd_drawObject(--player[0],player[1], '@');
				}
				standingOn = temp;
				slowDown = 0;
			}
		}
		/*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*/

		if(slowDown <= 50) slowDown++; //slowing down player movement
	}

}

#ifdef USE_FULL_ASSERT
void assert_failed(uint8_t* file, uint32_t line) {
/* Infinite loop */
/* Use GDB to find out why we're here */
  while (1);
}
#endif

/* main.c ends here */
