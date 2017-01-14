//Brent Hall and James Gregory
#include <f3d_gyro.h>
#include <stm32f30x.h>
void f3d_gyro_interface_init() {
  /**********************************************************************/
  /************** CODE HERE *********************************************/
  //You must configure and initialize the following 4 pins
  GPIO_InitTypeDef GPIO_InitStructure;
  RCC_AHBPeriphClockCmd(RCC_AHBPeriph_GPIOA, ENABLE);
  GPIO_StructInit(&GPIO_InitStructure);
  GPIO_InitStructure.GPIO_Pin = GPIO_Pin_5;
  GPIO_InitStructure.GPIO_Speed = GPIO_Speed_50MHz;
  GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AF;
  GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
  GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
  GPIO_Init(GPIOA,&GPIO_InitStructure);
  GPIO_PinAFConfig(GPIOA,5,GPIO_AF_5); 
  
  GPIO_StructInit(&GPIO_InitStructure);
  GPIO_InitStructure.GPIO_Pin = GPIO_Pin_6;
  GPIO_InitStructure.GPIO_Speed = GPIO_Speed_50MHz;
  GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AF;
  GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
  GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
  GPIO_Init(GPIOA,&GPIO_InitStructure);
  GPIO_PinAFConfig(GPIOA,6,GPIO_AF_5); 
  
  GPIO_StructInit(&GPIO_InitStructure);
  GPIO_InitStructure.GPIO_Pin = GPIO_Pin_7;
  GPIO_InitStructure.GPIO_Speed = GPIO_Speed_50MHz;
  GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AF;
  GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
  GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
  GPIO_Init(GPIOA,&GPIO_InitStructure);
  GPIO_PinAFConfig(GPIOA,7,GPIO_AF_5); 

	
  RCC_AHBPeriphClockCmd(RCC_AHBPeriph_GPIOE, ENABLE);
  GPIO_InitStructure.GPIO_Pin = GPIO_Pin_3;
  GPIO_InitStructure.GPIO_Mode = GPIO_Mode_OUT;
  GPIO_InitStructure.GPIO_OType = GPIO_OType_PP;
  GPIO_InitStructure.GPIO_Speed = GPIO_Speed_50MHz;
  GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
  GPIO_Init(GPIOE, &GPIO_InitStructure);
  GPIO_SetBits(GPIOE, GPIO_Pin_3);

  
  /**********************************************************************/
  SPI_InitTypeDef SPI_InitStructure;
  RCC_APB2PeriphClockCmd(RCC_APB2Periph_SPI1, ENABLE);
  //SPI Initialization and configuration
  SPI_InitStructure.SPI_Direction = SPI_Direction_2Lines_FullDuplex;
  SPI_InitStructure.SPI_DataSize = SPI_DataSize_8b;
  SPI_InitStructure.SPI_CPOL = SPI_CPOL_Low;
  SPI_InitStructure.SPI_CPHA = SPI_CPHA_1Edge;
  SPI_InitStructure.SPI_NSS = SPI_NSS_Soft;
  SPI_InitStructure.SPI_BaudRatePrescaler = SPI_BaudRatePrescaler_8;
  SPI_InitStructure.SPI_FirstBit = SPI_FirstBit_MSB;
  SPI_InitStructure.SPI_CRCPolynomial = 7;
  SPI_InitStructure.SPI_Mode = SPI_Mode_Master;
  SPI_Init(SPI1, &SPI_InitStructure);
  SPI_RxFIFOThresholdConfig(SPI1, SPI_RxFIFOThreshold_QF);
  SPI_Cmd(SPI1, ENABLE);
}

//the init function to be called in your main.c
void f3d_gyro_init(void) {
  //
  //SETTING THE CONTROL REGISTERS 
  f3d_gyro_interface_init();
  // CTRL1 Register 
  // Bit 7:6 Data Rate: Datarate 0
  // Bit 5:4 Bandwidth: Bandwidth 3
  // Bit 3: Power Mode: Active
  // Bit 2:0 Axes Enable: X,Y,Z enabled
  uint8_t ctrl1;
  uint8_t ctrl4;
	
  ctrl1 |= (uint8_t) (((uint8_t)0x00)|		
		      ((uint8_t)0x30)|		
		      ((uint8_t)0x08)|		
		      ((uint8_t)0x07));
  // CTRL4 Register 
  // Bit 7 Block Update: Continuous */
  // Bit 6 Endianess: LSB first  */
  // Bit 5:4 Full Scale: 500 dps */
  ctrl4 |= (uint8_t) (((uint8_t)0x00)|			
		      ((uint8_t)0x00)|				     
		      ((uint8_t)0x10));

  f3d_gyro_write(&ctrl1, 0x20, 1);
  f3d_gyro_write(&ctrl4, 0x23, 1);

}
//to read from it
void f3d_gyro_read(uint8_t* pBuffer, uint8_t ReadAddr, uint16_t NumByteToRead) {
  //are we reading one byte or more than one byte???
  if (NumByteToRead > 1) {
    ReadAddr |= (uint8_t)(0x80 | 0x40); // sets to multibyte mode
  }
  else {
    ReadAddr |= (uint8_t) (0x80); // sets to read mode (first bit 1)
  }
  //setting chip select to low (LETS TALK!)
  GYRO_CS_LOW();
  //sending address byte
  f3d_gyro_sendbyte(ReadAddr);  
  while(NumByteToRead > 0x00) {
    //WE are now sending dummy data so we can read the valuable!
    //remember we must write to read!
    //putting the information in the buffer
    *pBuffer = f3d_gyro_sendbyte(((uint8_t)0x00));
    NumByteToRead--;
    pBuffer++;
  }
  //setting chip select to high (DONE TALKING)
  GYRO_CS_HIGH();//setting chip select to high (DONE TALKING)
}

/*writing function*/
void f3d_gyro_write(uint8_t* pBuffer, uint8_t WriteAddr, uint16_t NumByteToWrite) {
  /****************************************************************************/
  /************** CODE HERE *********************************************/

    WriteAddr |= (uint8_t)(0x40);

  GYRO_CS_LOW();

  f3d_gyro_sendbyte(WriteAddr); 
  while(NumByteToWrite > 0x00) {
    *pBuffer = f3d_gyro_sendbyte(*pBuffer);
    NumByteToWrite--;
    pBuffer++;
  }
	
  GYRO_CS_HIGH();

  /***************************************************************************/
}

/*sends the bytes*/
static uint8_t f3d_gyro_sendbyte(uint8_t byte) {
  while (SPI_I2S_GetFlagStatus(SPI1, SPI_I2S_FLAG_TXE) == RESET); 
    SPI_SendData8(SPI1, byte);
  while (SPI_I2S_GetFlagStatus(SPI1, SPI_I2S_FLAG_RXNE) == RESET);
    return (uint8_t)SPI_ReceiveData8(SPI1);
}
/*gets the data*/
void f3d_gyro_getdata(float *pfData) {
  //
  //
  int16_t RawData[3];
  uint8_t tmpbuffer[6];
  int i;
  //We are going to write some data to the gyro
  f3d_gyro_write(tmpbuffer,0x28,6);
  //Then we are going to read it
  f3d_gyro_read(tmpbuffer,0x28,6);
  //casting the data retreiving from tmpbuffer to raw data
  for(i=0; i<3; i++) {
    RawData[i]=(int16_t)(((uint16_t)tmpbuffer[2*i+1] << 8) + tmpbuffer[2*i]);
  }
  //adjusting the data with the sensitivity
  for(i=0; i<3; i++) {
    pfData[i]=(float)RawData[i]/L3G_Sensitivity_500dps;
  }
}
