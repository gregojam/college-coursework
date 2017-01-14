#include <stm32f30x.h>  // Pull in include files for F30x standard drivers
#include <f3d_rtc.h>
#include <f3d_systick.h>
#include <ff.h>
#include <diskio.h>
#include <stdio.h>
#include <stdlib.h>
#include <f3d_timer2.h>

#define TIMER 20000
#define AUDIOBUFSIZE 128

extern uint8_t Audiobuf[AUDIOBUFSIZE];
extern int audioplayerHalf;
extern int audioplayerWhole;

typedef struct ckhd {
    uint32_t ckID;
    uint32_t cksize;
}ckhd;

typedef struct fmtck {
    uint16_t wFormatTag;
    uint16_t nChannels;
    uint32_t nSamplesPerSec;
    uint32_t nAvgBytesPerSec;
    uint16_t nBlockAlign;
    uint16_t wBitsPerSample;
}fmtck;

void readckhd(FIL*, ckhd*, uint32_t);
void die (FRESULT);
void play(ckhd);
int playSound(int);
