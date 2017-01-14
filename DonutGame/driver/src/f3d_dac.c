/* f3d_dac.c --- 
 * 
 * Filename: f3d_dac.c
 * Description: 
 * Author: Bryce Himebaugh
 * Maintainer: 
 * Created: Mon Nov 18 14:09:52 2013
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

#include <f3d_dac.h>
#include <f3d_led.h>

uint8_t Audiobuf[128];
int audioplayerHalf = 0;
int audioplayerWhole = 0;

const uint16_t Sine12bit[32] = {
  2047, 2447, 2831, 3185, 3498, 3750, 3939, 4056, 4095, 4056,
  3939, 3750, 3495, 3185, 2831, 2447, 2047, 1647, 1263, 909, 
  599, 344, 155, 38, 0, 38, 155, 344, 599, 909, 1263, 1647};    
const uint8_t Escalator8bit[6] = {0x0, 0x33, 0x66, 0x99, 0xCC, 0xFF};

void f3d_dac_init(void) {  
  GPIO_InitTypeDef GPIO_InitStructure;
  
  RCC_AHBPeriphClockCmd(RCC_AHBPeriph_DMA2, ENABLE);	
  RCC_APB1PeriphClockCmd(RCC_APB1Periph_DAC, ENABLE);
  
  GPIO_StructInit(&GPIO_InitStructure);  
  //  GPIO_InitStructure.GPIO_Pin =  GPIO_Pin_4|GPIO_Pin_5;
  // Only may pin 4 analog input, pin 5 is used for the gyro clock
  GPIO_InitStructure.GPIO_Pin =  GPIO_Pin_4;
  GPIO_InitStructure.GPIO_Mode = GPIO_Mode_AN;
  GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
  GPIO_Init(GPIOA, &GPIO_InitStructure);
  f3d_dac_channel_setup();
}

void f3d_dac_channel_setup(void) {
  DAC_InitTypeDef DAC_InitStructure;
  DMA_InitTypeDef DMA_InitStructure;
  NVIC_InitTypeDef NVIC_InitStructure;

  DAC_DeInit();
  DAC_StructInit(&DAC_InitStructure);
  DAC_InitStructure.DAC_Trigger = DAC_Trigger_T2_TRGO;
  DAC_InitStructure.DAC_WaveGeneration = DAC_WaveGeneration_None;
  DAC_InitStructure.DAC_LFSRUnmask_TriangleAmplitude = DAC_LFSRUnmask_Bits11_0;
  DAC_InitStructure.DAC_OutputBuffer = DAC_OutputBuffer_Disable;
  DAC_Init(DAC_Channel_2, &DAC_InitStructure); //1

  //  DAC_InitStructure.DAC_OutputBuffer = DAC_OutputBuffer_Enable;
  DAC_Init(DAC_Channel_1, &DAC_InitStructure);
  DAC_Cmd(DAC_Channel_1, ENABLE);
  
  /* DMA_StructInit(&DMA_InitStructure); */
  DMA_InitStructure.DMA_PeripheralBaseAddr = DAC_DHR8R2_ADDRESS;
  // DMA_InitStructure.DMA_PeripheralBaseAddr = &DAC->DHR8R2;
 
  // DMA_InitStructure.DMA_MemoryBaseAddr = (uint32_t)&Sine12bit;
  DMA_InitStructure.DMA_MemoryBaseAddr = (uint32_t)&Audiobuf;
  // DMA_InitStructure.DMA_BufferSize = 32;
  DMA_InitStructure.DMA_BufferSize = sizeof(Audiobuf);

  DMA_InitStructure.DMA_DIR = DMA_DIR_PeripheralDST;
  DMA_InitStructure.DMA_PeripheralInc = DMA_PeripheralInc_Disable;
  DMA_InitStructure.DMA_MemoryInc = DMA_MemoryInc_Enable;
  //  DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_HalfWord;
  DMA_InitStructure.DMA_PeripheralDataSize = DMA_PeripheralDataSize_Byte;
  //  DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_HalfWord;
  DMA_InitStructure.DMA_MemoryDataSize = DMA_MemoryDataSize_Byte;

  DMA_InitStructure.DMA_Mode = DMA_Mode_Circular;
  DMA_InitStructure.DMA_Priority = DMA_Priority_High;
  DMA_InitStructure.DMA_M2M = DMA_M2M_Disable;
  DMA_Init(DMA2_Channel3, &DMA_InitStructure);
  DMA_Cmd(DMA2_Channel3, ENABLE);
  DAC_DMACmd(DAC_Channel_2, ENABLE); // Critical instruction to get the samples to fire

  DMA_InitStructure.DMA_PeripheralBaseAddr = DAC_DHR8R1_ADDRESS;
  DMA_Init(DMA2_Channel4, &DMA_InitStructure);
  DMA_Cmd(DMA2_Channel4, ENABLE);
  DAC_DMACmd(DAC_Channel_1, ENABLE);

  //Enable DMA1 channel IRQ Channel */
  NVIC_InitStructure.NVIC_IRQChannel = DMA2_Channel4_IRQn;
  NVIC_InitStructure.NVIC_IRQChannelPreemptionPriority = 0;
  NVIC_InitStructure.NVIC_IRQChannelSubPriority = 0;
  NVIC_InitStructure.NVIC_IRQChannelCmd = ENABLE;
  NVIC_Init(&NVIC_InitStructure);

  // Enable interrupts for two sources
  DMA_ITConfig(DMA2_Channel4,DMA_IT_HT,ENABLE);
  DMA_ITConfig(DMA2_Channel4,DMA_IT_TC,ENABLE);
}

void DMA2_Channel4_5_IRQHandler(void) {
  static int ht_flag = 0;
  static int tc_flag = 0;
  
  // Half Transfer Flag
  if (DMA_GetFlagStatus(DMA2_FLAG_HT4)==SET) {
    DMA_ClearITPendingBit(DMA2_IT_HT4);
    audioplayerHalf = 1;

    /* if (ht_flag) {  */
    /*   f3d_led_on(2); */
    /* } */
    /* else { */
    /*   f3d_led_off(2); */
    /* } */
    /* ht_flag ^=1; */
  }

  // Full Transfer Flag 
  if (DMA_GetFlagStatus(DMA2_FLAG_TC4)==SET) {
    DMA_ClearITPendingBit(DMA2_IT_TC4);
    audioplayerWhole = 1;
    /* if (tc_flag) {  */
    /*   f3d_led_on(3); */
    /* } */
    /* else { */
    /*   f3d_led_off(3); */
    /* } */
    /* tc_flag ^=1; */
  }
  if (DMA_GetFlagStatus(DMA2_FLAG_TE4)==SET) {
    DMA_ClearITPendingBit(DMA2_IT_TE4);
  }
  if (DMA_GetFlagStatus(DMA2_FLAG_GL4)==SET) {
    DMA_ClearITPendingBit(DMA2_IT_GL4);
  }
}

void audioplayerStart(void) {
  
  // Clear the state
  DMA_ClearITPendingBit(DMA2_IT_TC4);
  DMA_ClearITPendingBit(DMA2_IT_HT4);
  audioplayerHalf = 0;
  audioplayerWhole = 0;

  // Enable everything 
  DMA_Cmd(DMA2_Channel4, ENABLE);
  DAC_Cmd(DAC_Channel_1, ENABLE);
  DAC_DMACmd(DAC_Channel_1, ENABLE);
  TIM_Cmd(TIM2, ENABLE);

}

void audioplayerStop(void) {
  // Disable everything 
  DMA_Cmd(DMA2_Channel4, DISABLE);
  DAC_Cmd(DAC_Channel_1, DISABLE);
  DAC_DMACmd(DAC_Channel_1, DISABLE);
  TIM_Cmd(TIM2, DISABLE);
}




/* f3d_dac.c ends here */
