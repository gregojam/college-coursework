//Last Modified By:	James Gregory 	Thomas Torbik
//			gregojam	ttorbik

/* f3d_lcd_sd.c --- 
 * 
 * Filename: f3d_lcd_sd.c
 * Description: 
 * Author: Bryce Himebaugh
 * Maintainer: 
 * Created: Thu Oct 24 05:18:36 2013
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
#include <f3d_lcd_sd.h>
#include <f3d_delay.h>
#include <glcdfont.h>
#include <math.h>
#include <f3d_dac.h>

static uint8_t madctlcurrent = MADVAL(MADCTLGRAPHICS);

void f3d_lcd_sd_interface_init(void) {

	/* vvvvvvvvvvv pin initialization for the LCD goes here vvvvvvvvvv*/ 
	GPIO_InitTypeDef GPIO_InitStructure;
	RCC_AHBPeriphClockCmd(RCC_AHBPeriph_GPIOB, ENABLE);
	GPIO_StructInit(&GPIO_InitStructure);
	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_13 | GPIO_Pin_14 | GPIO_Pin_15; // PB13 | PB14 | PB15
	GPIO_InitStructure.GPIO_Speed = GPIO_Speed_50MHz;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AF; // Mode: AF
	GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
	GPIO_Init(GPIOB,&GPIO_InitStructure); 

	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_8 | GPIO_Pin_9 | GPIO_Pin_10 | GPIO_Pin_11 | GPIO_Pin_12 ; //Pin9 | Pin10 | PB11 | PB12
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_OUT; // Mode: OUT
	GPIO_Init(GPIOB,&GPIO_InitStructure);

	GPIO_PinAFConfig(GPIOB,13,GPIO_AF_5);
	GPIO_PinAFConfig(GPIOB,14,GPIO_AF_5);
	GPIO_PinAFConfig(GPIOB,15,GPIO_AF_5);


	/* ^^^^^^^^^^^ pin initialization for the LCD goes here ^^^^^^^^^^ */

	// Section 4.1 SPI2 configuration
	// Note: you will need to add some code in the last three functions
	RCC_APB1PeriphClockCmd(RCC_APB1Periph_SPI2 , ENABLE);
	SPI_InitTypeDef SPI_InitStructure;
	SPI_InitStructure.SPI_Direction = SPI_Direction_2Lines_FullDuplex;
	SPI_InitStructure.SPI_DataSize = SPI_DataSize_8b;
	SPI_InitStructure.SPI_CPOL = SPI_CPOL_Low;
	SPI_InitStructure.SPI_CPHA = SPI_CPHA_1Edge;
	SPI_InitStructure.SPI_NSS = SPI_NSS_Soft;
	SPI_InitStructure.SPI_BaudRatePrescaler = SPI_BaudRatePrescaler_8;
	SPI_InitStructure.SPI_FirstBit = SPI_FirstBit_MSB;
	SPI_InitStructure.SPI_CRCPolynomial = 7;
	SPI_InitStructure.SPI_Mode = SPI_Mode_Master;
	SPI_Init(SPI2, &SPI_InitStructure);
	SPI_RxFIFOThresholdConfig(SPI2, SPI_RxFIFOThreshold_QF);
	SPI_Cmd(SPI2, ENABLE);

	RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA1, ENABLE);
}


struct lcd_cmdBuf {
  uint8_t command;
  uint8_t delay;
  uint8_t len;
  uint8_t data [16];
};

static const struct lcd_cmdBuf initializers[] = {
  // SWRESET Software reset
  { 0x01, 150, 0, 0},
  // SLPOUT Leave sleep mode
  { 0x11, 150, 0, 0},
  // FRMCTR1, FRMCTR2 Frame Rate configuration -- Normal mode, idle
  // frame rate = fosc / (1 x 2 + 40) * (LINE + 2C + 2D)
  { 0xB1, 0, 3, { 0x01, 0x2C, 0x2D }},
  { 0xB2, 0, 3, { 0x01, 0x2C, 0x2D }},
  // FRMCTR3 Frame Rate configureation -- partial mode
  { 0xB3, 0, 6, { 0x01, 0x2C, 0x2D, 0x01, 0x2C, 0x2D }},
  // INVCTR Display inversion (no inversion)
  { 0xB4, 0, 1, { 0x07 }},
  /* ... */
  /*! llcmdstop2 !*/
  /*! llcmdstart3 !*/
  // PWCTR1 Power control -4.6V, Auto mode
  { 0xC0, 0, 3, { 0xA2, 0x02, 0x84}},
  // PWCTR2 Power control VGH25 2.4C, VGSEL -10, VGH = 3 * AVDD
  { 0xC1, 0, 1, { 0xC5}},
  // PWCTR3 Power control, opamp current smal, boost frequency
  { 0xC2, 0, 2, { 0x0A, 0x00 }},
  // PWCTR4 Power control, BLK/2, opamp current small and medium low
  { 0xC3, 0, 2, { 0x8A, 0x2A}},
  // PWRCTR5, VMCTR1 Power control
  { 0xC4, 0, 2, { 0x8A, 0xEE}},
  { 0xC5, 0, 1, { 0x0E }},
  // INVOFF Don't invert display
  { 0x20, 0, 0, 0},
  // Memory access directions. row address/col address, bottom to top refesh (10.1.27)
  { ST7735_MADCTL, 0, 1, {MADVAL(MADCTLGRAPHICS)}},
  // Color mode 16 bit (10.1.30
  { ST7735_COLMOD, 0, 1, {0x05}},
  // Column address set 0..127
  { ST7735_CASET, 0, 4, {0x00, 0x00, 0x00, 0x7F }},
  // Row address set 0..159
  { ST7735_RASET, 0, 4, {0x00, 0x00, 0x00, 0x9F }},
  // GMCTRP1 Gamma correction
  { 0xE0, 0, 16, {0x02, 0x1C, 0x07, 0x12, 0x37, 0x32, 0x29, 0x2D,
0x29, 0x25, 0x2B, 0x39, 0x00, 0x01, 0x03, 0x10 }},
  // GMCTRP2 Gamma Polarity corrction
  { 0xE1, 0, 16, {0x03, 0x1d, 0x07, 0x06, 0x2E, 0x2C, 0x29, 0x2D,
0x2E, 0x2E, 0x37, 0x3F, 0x00, 0x00, 0x02, 0x10 }},
  // DISPON Display on
  { 0x29, 100, 0, 0},
  // NORON Normal on
  { 0x13, 10, 0, 0},
  // End
  { 0, 0, 0, 0}
};

void f3d_lcd_init(void) {
	const struct lcd_cmdBuf *cmd;

	f3d_lcd_sd_interface_init();    // Setup SPI2 Link and configure GPIO pins
	LCD_BKL_ON();                   // Enable Backlight

	// Make sure that the chip select and reset lines are deasserted
	LCD_CS_DEASSERT();              // Deassert Chip Select

	LCD_RESET_DEASSERT();           
	delay(100);
	LCD_RESET_ASSERT();
	delay(100);
	LCD_RESET_DEASSERT();
	delay(100);

	// Write initialization sequence to the lcd
	for (cmd=initializers; cmd->command; cmd++) {
		LcdWrite(LCD_C,&(cmd->command),1);
		if (cmd->len)
			LcdWrite(LCD_D,cmd->data,cmd->len);
		if (cmd->delay) {
			delay(cmd->delay);
		}
	}

}

static void LcdWrite(char dc,const char *data,int nbytes) {
  GPIO_WriteBit(LCD_PORT,GPIO_PIN_DC,dc); // dc 1 = data , 0 = control
  GPIO_ResetBits(LCD_PORT,GPIO_PIN_SCE);
  spiReadWrite(SPILCD,0,data,nbytes,LCDSPEED);
  GPIO_SetBits(LCD_PORT,GPIO_PIN_SCE);
}

static void LcdWrite16(char dc,const uint16_t *data,int cnt) {
  GPIO_WriteBit(LCD_PORT,GPIO_PIN_DC,dc); 
  GPIO_ResetBits(LCD_PORT,GPIO_PIN_SCE);
  spiReadWrite16(SPILCD,0,data,cnt,LCDSPEED);
  GPIO_SetBits(LCD_PORT,GPIO_PIN_SCE);
}

int spiReadWrite(SPI_TypeDef *SPIx, uint8_t *rbuf, 
     const uint8_t *tbuf, int cnt, uint16_t speed) {
  int i;
  SPIx->CR1 = (SPIx->CR1 & ~SPI_BaudRatePrescaler_256) | speed;
  
  if ((cnt > 4) && !(cnt & 1)) {
    return xchng_datablock(SPIx, 0, tbuf, rbuf , cnt);
  }
  else {
    for (i = 0; i < cnt; i++){
      SPI_SendData8(SPIx, tbuf ? *tbuf++ : 0xff);
      while (SPI_I2S_GetFlagStatus(SPIx, SPI_I2S_FLAG_RXNE) == RESET);
      if (rbuf) {
    *rbuf++ = SPI_ReceiveData8(SPIx);
      } else  {
    SPI_ReceiveData8(SPIx);
      }
    }
    return i;
  }
}

int spiReadWrite16(SPI_TypeDef *SPIx, uint16_t *rbuf, 
           const uint16_t *tbuf, int cnt, uint16_t speed) {
  int i;
  SPIx->CR1 = (SPIx->CR1 & ~SPI_BaudRatePrescaler_256) | speed;
  SPI_DataSizeConfig(SPIx, SPI_DataSize_16b);
  if ((cnt > 4) && !(cnt & 3)) {
    i =  xchng_datablock(SPIx, 1, tbuf, rbuf , cnt);
  }
  else {
    for (i = 0; i < cnt; i++){
      SPI_I2S_SendData16(SPIx, tbuf ? *tbuf++ : 0xffff);
      while (SPI_I2S_GetFlagStatus(SPIx, SPI_I2S_FLAG_RXNE) == RESET);
      if (rbuf) {
    *rbuf++ = SPI_I2S_ReceiveData16(SPIx);
      } else {
    SPI_I2S_ReceiveData16(SPIx);
      }
    }
  }
  SPI_DataSizeConfig(SPIx, SPI_DataSize_8b);
  return i;
}

void f3d_lcd_setAddrWindow ( uint16_t x0 , uint16_t y0 , uint16_t x1 , uint16_t y1 , uint8_t madctl) {
  madctl = MADVAL ( madctl );
  if ( madctl != madctlcurrent ){
    f3d_lcd_writeCmd(ST7735_MADCTL);
    LcdWrite(LCD_D, &madctl, 1);
    madctlcurrent = madctl ;
  }
  f3d_lcd_writeCmd(ST7735_CASET);
  LcdWrite16(LCD_D,&x0,1);
  LcdWrite16(LCD_D,&x1,1);
  f3d_lcd_writeCmd(ST7735_RASET);
  LcdWrite16(LCD_D,&y0,1);
  LcdWrite16(LCD_D,&y1,1);
  f3d_lcd_writeCmd(ST7735_RAMWR);
}


void f3d_lcd_pushColor(uint16_t *color,int cnt) {
  LcdWrite16(LCD_D,color,cnt);
}

static void f3d_lcd_writeCmd(uint8_t c) {
  LcdWrite(LCD_C,&c,1);
}

void f3d_lcd_fillScreen(uint16_t color) {
  uint8_t y;
  uint16_t x[ST7735_width];
  for (y = 0; y < ST7735_width; y++) x[y] = color;
  f3d_lcd_setAddrWindow (0,0,ST7735_width-1,ST7735_height-1,MADCTLGRAPHICS);
  for (y=0;y<ST7735_height; y++) {
    f3d_lcd_pushColor(x,ST7735_width);
  }
}

void f3d_lcd_PitchRoll(uint16_t color, float pitch, float roll) {
  uint8_t y;
  uint16_t x[ST7735_width];
  int line; //current line we're coloring
  int centerH = ST7735_height / 2; // vertical center of screen
  int centerW = ST7735_width / 2; // horizontal center of screen
  for (line = 10; line < ST7735_height - 10; line++){
	  for (y = 0; y < ST7735_width; y++){
		if(((y < centerW && y > centerW - (int)roll) || // between center and roll?
			(y > centerW && y < centerW - (int)roll)) && 
			((line < centerH && line > centerH - (int)pitch) || // between center and pitch?
			(line > centerH && line < centerH - (int)pitch)))
			 x[y] = color;
		else
			x[y] = BLACK;
	  } 
 	 f3d_lcd_setAddrWindow (0,line,ST7735_width-1,ST7735_height-11,MADCTLGRAPHICS);
  	 f3d_lcd_pushColor(x,ST7735_width);
  }
}

void f3d_lcd_barFill(uint8_t y_pos, int axis, uint16_t color) {
  uint8_t y;
  uint16_t x[ST7735_width];
  for (y = 0; y < ST7735_width; y++){
    if(y < axis / 2)//bar length (divide by two for readability)
      x[y] = color;
    else
      x[y] = BLACK;//remove previous bar
  }

  //set bar position
  f3d_lcd_setAddrWindow (0,y_pos,ST7735_width-1,ST7735_height-1,MADCTLGRAPHICS);
  for (y=0;y< 10; y++)
    f3d_lcd_pushColor(x,ST7735_width);
}

void f3d_lcd_drawPixel(uint8_t x, uint8_t y, uint16_t color) {
  if ((x >= ST7735_width) || (y >= ST7735_height)) return;
  f3d_lcd_setAddrWindow(x,y,x+1,y+1,MADCTLGRAPHICS);
  f3d_lcd_pushColor(&color,1);
}

void f3d_lcd_bar(uint8_t x, uint8_t y, uint8_t xMax, uint8_t yMax, uint16_t color) {
  int xTemp, yTemp;
  f3d_lcd_setAddrWindow (x,y,xMax,yMax,MADCTLGRAPHICS);
  for (xTemp = x; xTemp<=xMax; xTemp++) {
    for (yTemp = y; yTemp<=yMax; yTemp++) {
      f3d_lcd_pushColor(&color,1);
    }
  }
}

uint16_t wall[][4] = {{BLACK,DARKGREY,BLACK,DARKGREY},{DARKGREY,BLACK,DARKGREY,BLACK},{BLACK,DARKGREY,BLACK,DARKGREY},{DARKGREY,BLACK,DARKGREY,BLACK}};
uint16_t door[][4] = {{BROWN,BROWN,BROWN,BROWN},{BROWN,BROWN,BLACK,BROWN},{BROWN,BROWN,BLACK,BROWN},{BROWN,BROWN,BROWN,BROWN}};
uint16_t person[][4] = {{BROWN,BROWN,BROWN,BROWN},{BROWN,LIGHTBROWN,LIGHTBROWN,BROWN},{BROWN,BROWN,BROWN,BROWN},{BROWN,BROWN,BROWN,BROWN}};
uint16_t portal[][4] = {{BLUE,BLUE,BLUE,BLUE},{BLUE,CYAN,CYAN,BLUE},{BLUE,CYAN,CYAN,BLUE},{BLUE,BLUE,BLUE,BLUE}};
uint16_t ground[][4] = {{GREY,GREY,GREY,GREY},{GREY,GREY,GREY,GREY},{GREY,GREY,GREY,GREY},{GREY,GREY,GREY,GREY}};
uint16_t donut[][4] = {{MAGENTA,MAGENTA,MAGENTA,MAGENTA},{MAGENTA,YELLOW,MAGENTA,MAGENTA},{MAGENTA,MAGENTA,YELLOW,MAGENTA},{MAGENTA,MAGENTA,MAGENTA,MAGENTA}};
uint16_t key[][4] = {{YELLOW,YELLOW,PURPLE,PURPLE},{YELLOW,YELLOW,YELLOW,PURPLE},{PURPLE,YELLOW,YELLOW,YELLOW},{PURPLE,PURPLE,YELLOW,YELLOW}};

void f3d_lcd_drawObject(uint8_t x, uint8_t y, char obj) {
  int xTemp, yTemp, i;
  xTemp = x*4;
  yTemp = y*4;
  f3d_lcd_setAddrWindow (xTemp,yTemp,xTemp+3,yTemp+3,0x5);
  if(obj == 'x') {
    for(i = 0; i < 4; i++) {
      f3d_lcd_pushColor(wall[i],4);
    }
  }
  else if(obj == 'd') {
    for(i = 0; i < 4; i++) {
      f3d_lcd_pushColor(door[i],4);
    }
  }
  else if(obj == '@') {
    for(i = 0; i < 4; i++) {
      f3d_lcd_pushColor(person[i],4);
    }
  }
  else if(obj == ' ') {
    for(i = 0; i < 4; i++) {
      f3d_lcd_pushColor(ground[i],4);
    }
  }
  else if(obj == 'o') {
    for(i = 0; i < 4; i++) {
      f3d_lcd_pushColor(donut[i],4);
    }
  }
  else if(obj == 'k') {
    for(i = 0; i < 4; i++) {
      f3d_lcd_pushColor(key[i],4);
    }
  }
  else {
    for(i = 0; i < 4; i++) {
      f3d_lcd_pushColor(portal[i],4);
    }
  }
}

void f3d_lcd_drawChar(uint8_t x, uint8_t y, unsigned char c, uint16_t color, uint16_t background_color) {
  int i, j;
  for (i = 0; i < 5; i++) {
    for (j = 0; j < 8; j++){ 
      f3d_lcd_drawPixel(129-y-j,x+i, background_color);
    }
  }
  for (i = 0; i < 5; i++) {
    uint8_t byte = ASCII[c*5 + i];
    for (j = 0; j < 8; j++){
      if (byte & (1)) {
	f3d_lcd_drawPixel(129-y-j,x+i, color);
      }
      byte >>= 1;
    }
  }
}

void f3d_lcd_drawString(uint8_t x, uint8_t y, char *c, uint16_t color, uint16_t background_color) {
  uint8_t start = x;
  while (c && *c) {
    f3d_lcd_drawChar(x, y, *c++, color, background_color);
    x += 6;
    if (*c == '~') {
      *c++;
      y += 10;
      x = start;
    }
  }
}

static int xchng_datablock(SPI_TypeDef *SPIx, int half, const void *tbuf, void *rbuf, unsigned count) {
	DMA_InitTypeDef DMA_InitStructure;
	uint16_t dummy[] = {0xffff};

	DMA_Channel_TypeDef *rxChan;
	DMA_Channel_TypeDef *txChan;
	uint32_t dmaflag;

	if (count & 1)
		return -1;

	if (SPIx == SPI1) {
		rxChan = DMA1_Channel2;
		txChan = DMA1_Channel3;
		dmaflag = DMA1_FLAG_TC2;
	}
	else if (SPIx == SPI2) {
		rxChan = DMA1_Channel4;
		txChan = DMA1_Channel5;
		dmaflag = DMA1_FLAG_TC4;
	}
	else
		return -1;

	DMA_InitStructure.DMA_PeripheralBaseAddr = (uint32_t)(&(SPIx->DR));
	if (half) {
		DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;
		DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;
	}
	else {
		DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_Byte;
		DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_Byte;
	}
	DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;
	DMA_InitStructure.DMA_BufferSize = count;
	DMA_InitStructure.DMA_Mode = DMA_Mode_Normal;
	DMA_InitStructure.DMA_Priority = DMA_Priority_VeryHigh;
	DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;

	DMA_DeInit(rxChan);
	DMA_DeInit(txChan);

	if (rbuf) {
		DMA_InitStructure.DMA_MemoryBaseAddr = (uint32_t)rbuf;
		DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;
	}
	else {
		DMA_InitStructure.DMA_MemoryBaseAddr = (uint32_t) dummy;
		DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Disable;
	}
	DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralSRC;
	DMA_Init(rxChan, &DMA_InitStructure);

	if (tbuf) {
		DMA_InitStructure.DMA_MemoryBaseAddr = (uint32_t)tbuf;
		DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;
	}
	else {
		DMA_InitStructure.DMA_MemoryBaseAddr = (uint32_t) dummy;
		DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Disable;
	}
	DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralDST;
	DMA_Init(txChan, &DMA_InitStructure);

	// Enable channels
	DMA_Cmd(rxChan, ENABLE);
	DMA_Cmd(txChan, ENABLE);

	// Enable SPI TX/RX request
	SPI_I2S_DMACmd(SPIx, SPI_I2S_DMAReq_Rx | SPI_I2S_DMAReq_Tx, ENABLE);

	// Wait for completion
	while (DMA_GetFlagStatus(dmaflag) == RESET) { ; }

	// Disable channels
	DMA_Cmd(rxChan, DISABLE);
	DMA_Cmd(txChan, DISABLE);
	SPI_I2S_DMACmd(SPIx, SPI_I2S_DMAReq_Rx | SPI_I2S_DMAReq_Tx, DISABLE);
	return count;
}

/* f3d_lcd_sd.c ends here */
