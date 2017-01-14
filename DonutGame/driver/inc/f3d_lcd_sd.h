// Jiawei Zhang		James Gregory
// zhang435		gregojam

/* f3d_lcd_sd.h --- 
 * 
 * Filename: f3d_lcd_sd.h
 * Description: 
 * Author: Bryce Himebaugh
 * Maintainer: 
 * Created: Thu Oct 24 05:19:07 2013
 * Last-Updated: 
 *           By: 
 *     Update #: 0
 * Keywords: 
 * Compatibility: 
 * 
 */

/* Commentary: 
 * 
 * 
 * 
 */

/* Change log:
 * 
 * 
 */

/* Copyright (c) 2004-2007 The Trustees of Indiana University and 
 * Indiana University Research and Technology Corporation.  
 * 
 * All rights reserved. 
 * 
 * Additional copyrights may follow 
 */

/* Code: */
#include <stm32f30x.h>

#define SPI_SLOW (uint16_t) SPI_BaudRatePrescaler_64
#define SPI_MEDIUM (uint16_t) SPI_BaudRatePrescaler_8
#define SPI_FAST (uint16_t) SPI_BaudRatePrescaler_2

#define LCD_RS_CONTROL()   GPIO_ResetBits(GPIOB, GPIO_Pin_9)
#define LCD_RS_DATA()  GPIO_SetBits(GPIOB, GPIO_Pin_9)
#define GPIO_PIN_DC GPIO_Pin_9 

#define LCD_RESET_ASSERT()   GPIO_ResetBits(GPIOB, GPIO_Pin_10)
#define LCD_RESET_DEASSERT()  GPIO_SetBits(GPIOB, GPIO_Pin_10)
#define GPIO_PIN_RST GPIO_Pin_10

#define LCD_BKL_ON()   GPIO_ResetBits(GPIOB, GPIO_Pin_11)
#define LCD_BKL_OFF()  GPIO_SetBits(GPIOB, GPIO_Pin_11)

#define LCD_CS_ASSERT()   GPIO_ResetBits(GPIOB, GPIO_Pin_12)
#define LCD_CS_DEASSERT()  GPIO_SetBits(GPIOB, GPIO_Pin_12)

// Create these macros needed for the SD card interface in the ff9b code
#define SD_CS_HIGH()  GPIO_SetBits(GPIOB, GPIO_Pin_8)
#define SD_CS_LOW()  GPIO_ResetBits(GPIOB, GPIO_Pin_8)

#define GPIO_PIN_SCE GPIO_Pin_12    

#define ST7735_CASET 0x2A
#define ST7735_RASET 0x2B
#define ST7735_MADCTL 0x36
#define ST7735_COLMOD 0x3A
#define ST7735_RAMWR 0x2C
#define ST7735_RAMRD 0x2E

#define MADVAL(x) (((x) << 5) | 8)

#define MADCTLGRAPHICS 0x6
#define MADLEFT 0x1
#define MADUP 0x2
#define MADRIGHT 0x7
#define MADDOWN 0X4

#define ST7735_width 128
#define ST7735_height 160

#define LCDSPEED SPI_FAST
#define SPILCD SPI2
#define LCD_PORT GPIOB

#define LOW 0
#define HIGH 1
#define LCD_C LOW
#define LCD_D HIGH

#define BLACK 0x0000
#define BLUE 0xF800
#define CYAN 0xFFE0
#define GREEN 0x07E0
#define MAGENTA 0xF81F
#define RED 0x001F
#define WHITE 0xFFFF
#define YELLOW 0x07FF
#define GREY 0xEFBD
#define PURPLE 0xEBBD
#define BROWN 0x03FC
#define DARKGREY 0x62AA
#define LIGHTBROWN 0x053F
#define DARKBROWN 0x02B7

void f3d_lcd_sd_interface_init(void);
void f3d_lcd_init(void);
void f3d_lcd_setAddrWindow(uint16_t x0,uint16_t y0,uint16_t x1,uint16_t y1,uint8_t madctl);
void f3d_lcd_pushColor(uint16_t *color,int cnt);
static void f3d_lcd_writeCmd(uint8_t);
void f3d_lcd_drawPixel(uint8_t, uint8_t, uint16_t);
void f3d_lcd_drawChar(uint8_t, uint8_t, unsigned char, uint16_t, uint16_t);
void f3d_lcd_drawChar2(uint8_t, uint8_t, unsigned char, uint16_t, uint16_t);
void f3d_lcd_drawChar3(uint8_t, uint8_t, unsigned char, uint16_t, uint16_t);
void f3d_lcd_drawString(uint8_t, uint8_t, char *, uint16_t, uint16_t);
void f3d_lcd_fillScreen(uint16_t);
void f3d_lcd_PitchRoll(uint16_t, float, float);
void f3d_lcd_barFill(uint8_t, int, uint16_t);
void f3d_lcd_bar(uint8_t x, uint8_t y, uint8_t xMax, uint8_t yMax, uint16_t color);
void f3d_lcd_drawObject(uint8_t x, uint8_t y, char obj);
int spiReadWrite(SPI_TypeDef *SPIx,uint8_t *rbuf,const uint8_t *tbuf, int cnt, uint16_t speed);
int spiReadWrite16(SPI_TypeDef *SPIx,uint16_t *rbuf,const uint16_t *tbuf, int cnt,  uint16_t speed);
static void LcdWrite(char dc,const char *data,int nbytes);
static void LcdWrite16(char dc,const uint16_t *data,int cnt);
static int xchng_datablock(SPI_TypeDef *SPIx, int half, const void *tbuf, void *rbuf, unsigned count);

/* void f3d_sdcard_readwrite(uint8_t *, uint8_t *, int); */


/* f3d_lcd_sd.h ends here */

