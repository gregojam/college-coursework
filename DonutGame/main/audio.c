#include <stm32f30x.h>  // Pull in include files for F30x standard drivers
#include <f3d_rtc.h>
#include <f3d_systick.h>
#include <ff.h>
#include <diskio.h>
#include <stdio.h>
#include <stdlib.h>
#include <strings.h>
#include <f3d_timer2.h>
#include "audio.h"

#define TIMER 20000
#define AUDIOBUFSIZE 128

extern uint8_t Audiobuf[AUDIOBUFSIZE];
extern int audioplayerHalf;
extern int audioplayerWhole;

FATFS Fatfs;        /* File system object */
FIL fid;            /* File object */
BYTE Buff[512];     /* File read buffer */
int ret;

void readckhd(FIL *fid, ckhd *hd, uint32_t ckID) {
    f_read(fid, hd, sizeof(ckhd), &ret);
    if (ret != sizeof(ckhd))
    exit(-1);
    if (ckID && (ckID != hd->ckID))
    exit(-1);
}

void die (FRESULT rc) {
    printf("Failed with rc=%u.\n", rc);
    while (1);
}

void play(ckhd hd){
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

int playSound(int sound) {
	f3d_timer2_init();
  	f3d_dac_init();
	f3d_rtc_init();
  	f3d_systick_init();

	FRESULT rc;			/* Result code */
  	DIR dir;			/* Directory object */
  	FILINFO fno;			/* File information object */
  	UINT bw, br;
  	unsigned int retval;
  	int bytesread;
  	char *daFile = "birthday.wav";
	int played = 0;

	f_mount(0, &Fatfs);/* Register volume work area */

	/***************PLAYING SOUND*******************************/
	rc = f_open(&fid, daFile, FA_READ);
	if (!rc) {
		ckhd hd;
		uint32_t  waveid;
		fmtck fck;
	
		readckhd(&fid, &hd, 'FFIR');
		
		f_read(&fid, &waveid, sizeof(waveid), &ret);
		if ((ret != sizeof(waveid)) || (waveid != 'EVAW'))
			return -1;
	
		readckhd(&fid, &hd, ' tmf');
	
		f_read(&fid, &fck, sizeof(fck), &ret);
	
		// skip over extra info
		
		if (hd.cksize != 16) {
			f_lseek(&fid, hd.cksize - 16);
		}
		
	  	// now skip all non-data chunks !
		
		while(1){
			readckhd(&fid, &hd, 0);
			if (hd.ckID == 'atad')
	  			break;
			f_lseek(&fid, hd.cksize);
	  	}
		
	  	// Play it !
	  	if(!played){
	  		f_read(&fid, Audiobuf, AUDIOBUFSIZE, &ret);
			hd.cksize -= ret;
			play(hd);
			played = 1;
	  	}
	}
	  
	rc = f_close(&fid);
	if (rc) die(rc);
	/*^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^*/
}
