#include <stm32f30x.h>  // Pull in include files for F30x standard drivers
#include <f3d_led.h>     // Pull in include file for the local drivers
#include <f3d_uart.h>
#include <f3d_gyro.h>
#include <f3d_lcd_sd.h>
#include <f3d_i2c.h>
#include <f3d_accel.h>
#include <f3d_mag.h>
#include <f3d_nunchuk.h>
#include <f3d_rtc.h>
#include <f3d_systick.h>
#include <ff.h>
#include <diskio.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <f3d_user_btn.h>
#include <f3d_timer2.h>
#include <bmp.h>
#include "level.h"
//#include "audio.h"
/////////////////////////////////////////////////////

#define TIMER 20000
#define AUDIOBUFSIZE 128

extern uint8_t Audiobuf[AUDIOBUFSIZE];
extern int audioplayerHalf;
extern int audioplayerWhole;

FATFS Fatfs;        /* File system object */
FIL fid;            /* File object */
BYTE Buff[512];     /* File read buffer */
int ret;

struct ckhd {
    uint32_t ckID;
    uint32_t cksize;
};

struct fmtck {
    uint16_t wFormatTag;
    uint16_t nChannels;
    uint32_t nSamplesPerSec;
    uint32_t nAvgBytesPerSec;
    uint16_t nBlockAlign;
    uint16_t wBitsPerSample;
};

void readckhd(FIL *fid, struct ckhd *hd, uint32_t ckID) {
    f_read(fid, hd, sizeof(struct ckhd), &ret);
    if (ret != sizeof(struct ckhd))
    exit(-1);
    if (ckID && (ckID != hd->ckID))
    exit(-1);
}

void die (FRESULT rc) {
    printf("Failed with rc=%u.\n", rc);
    while (1);
}

void play(struct ckhd hd){
      audioplayerStart();
      while (hd.cksize && !user_btn_read()) { // interrupt on user button press
	int next = hd.cksize > AUDIOBUFSIZE/2 ? AUDIOBUFSIZE/2 : hd.cksize;
	if (audioplayerHalf) {
	  if (next < AUDIOBUFSIZE/2)
	    bzero(Audiobuf, AUDIOBUFSIZE/2);
	  f_read(&fid, Audiobuf, next, &ret);
	  hd.cksize -= ret;
	  audioplayerHalf = 0;
	}
	if (audioplayerWhole) {
	  if (next < AUDIOBUFSIZE/2)
	    bzero(&Audiobuf[AUDIOBUFSIZE/2], AUDIOBUFSIZE/2);
	  f_read(&fid, &Audiobuf[AUDIOBUFSIZE/2], next, &ret);
	  hd.cksize -= ret;
	  audioplayerWhole = 0;
	}
      }
      audioplayerStop();
}

/////////////////////////////////////////////////////////

levelInfo level;
levelInfo game[13];
int *player;
int *portals;
char standingOn;
int keysHeld, donutHeld, currLevel;
nunchuk_t nunValues;
FATFS Fatfs;		/* File system object */
FIL Fil;		/* File object */
//BYTE Buff[128];		/* File read buffer */


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
			else if(obj == ' ' || obj == '@')
				return ' ';
			else
				return '!';
}

void intro(void) {
	int height;
	int screenHeight = 127;
	int numLines = 7;
	for(height = numLines*16+screenHeight; height > -7; height -= 2) {
		f3d_lcd_fillScreen(GREY);
		/*f3d_lcd_drawString(12,height-11*16,"HELLO",BLACK,GREY);
		f3d_lcd_drawString(12,height-10*16,"HELLO 2",BLACK,GREY);
		f3d_lcd_drawString(12,height-9*16,"HELLO",BLACK,GREY);
		f3d_lcd_drawString(12,height-8*16,"HELLO 2",BLACK,GREY);
		f3d_lcd_drawString(12,height-7*16,"HELLO",BLACK,GREY);*/
		f3d_lcd_drawString(12,height-6*16,"HELLO 2",BLACK,GREY);
		f3d_lcd_drawString(12,height-5*16,"HELLO",BLACK,GREY);
		f3d_lcd_drawString(12,height-4*16,"HELLO 2",BLACK,GREY);
		f3d_lcd_drawString(12,height-3*16,"HELLO",BLACK,GREY);
		f3d_lcd_drawString(12,height-2*16,"HELLO 2",BLACK,GREY);
		f3d_lcd_drawString(12,height-1*16,"HELLO",BLACK,GREY);
		f3d_lcd_drawString(12,height,"HELLO 2",BLACK,GREY);
		delay(100);
	}


	delay(1000);
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
	f3d_lcd_drawString(12,26*4, "GOAL:GET THE DONUT", BLACK, GREY);
	delay(6000);
}

void loadLevel(int lvl) {
	level = game[lvl];
	player = level.playerStart;
	portals = malloc(4 * level.numPortals * sizeof(int));
	standingOn = ' ';
	initPortals();
	
	f3d_lcd_fillScreen(GREY);
	f3d_lcd_drawObject(player[0], player[1], '@');

	// (v)ertical and (h)orizontal from view of play
	int v, h;
	for(v = 0; v < 32; v++){
		for(h = 0; h < 40; h++){
			if(level.map[v][h] == 'x' || level.map[v][h] == 'd' || level.map[v][h] == 'k' || level.map[v][h] == 'o')
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
  	int columns;
  	int row;
  	struct bmpfile_magic magic;
  	struct bmpfile_header header;
  	BITMAPINFOHEADER info;
  	struct bmppixel pixel;


	rc = f_close(&Fil);
    rc = f_open(&Fil, picture, FA_READ);
    rc = f_read(&Fil, &magic, sizeof(magic), &br);	/* Read a chunk of file */
    rc = f_read(&Fil, &header, sizeof(header), &br);
    rc = f_read(&Fil, &info, sizeof(info), &br);
    f3d_lcd_setAddrWindow(0,0,128,160,MADLEFT);
    for(columns = 0; columns < 128; columns++) {
    	uint16_t lcdArray[128];
    	int i = 0;
    	for(row = 128; row > 0; row--) {
    		rc = f_read(&Fil, (void *) &pixel, sizeof(pixel), &br);
    	 	uint16_t red = (pixel.r >> 3);
    		uint16_t green = (pixel.g >> 2);
    		uint16_t blue = (pixel.b >> 3);
    		uint16_t color = (blue << 11) | (green << 5) | red;
    		lcdArray[i] = color;
    		i++;
    		//f3d_lcd_drawPixel(row,columns,color);
     	}
        f3d_lcd_pushColor(lcdArray,129);
	}
}



int main(void){
	setvbuf(stdin, NULL, _IONBF, 0);
	setvbuf(stdout, NULL, _IONBF, 0);
	setvbuf(stderr, NULL, _IONBF, 0);

	f3d_uart_init();
	f3d_lcd_init();
	f3d_i2c1_init();
	f3d_nunchuk_init();
	f3d_delay_init();
	f3d_accel_init();
	
	game[0] = LEVEL_1;
	game[1] = LEVEL_2;
	game[2] = LEVEL_3;
	game[3] = LEVEL_4;
	currLevel = 0;
	//intro();
	loadLevel(currLevel);

	printf("yay");

	playSound(0);
	
	int slowDown = 0;
	keysHeld = 0;
	donutHeld = 0;
	float accData[3];
	int numShakes = 20;
	float lastShake;
	int donutThere = 0;
	int shakeTimer = 5;  

	//////////////////////////////
	FRESULT rc;			/* Result code */
	DIR dir;			/* Directory object */
	FILINFO fno;			/* File information object */
	UINT bw, br;
	unsigned int retval;
	int bytesread;
	char* daFile[3] = {"thermo.wav", "odyssey.wav", "1.wav"};
	/////////////////////////////

	while(1){
		f3d_nunchuk_read(&nunValues);//getting nunchuk values

		/************LEVEL 13***********************/
		if(currLevel == 3 && donutThere == 0) {
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
		
		/***********NEXT LEVEL********/
		if(donutHeld == 1) {
			//playSound(0,1);
			currLevel++;
			loadLevel(currLevel);
			donutHeld = 0;
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
