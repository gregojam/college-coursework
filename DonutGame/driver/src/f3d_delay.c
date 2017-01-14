/* f3d_delay.c --- 
 * 
 * Filename: f3d_delay.c
 * Description: 
 * Author: Bryce Himebaugh
 * Maintainer: 
 * Created: Thu Oct 24 06:34:26 2013
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
#include <f3d_delay.h>

void delay(int count) {
  while (count-- > 0) {
    int i = 10000; 
    while (i-- > 0) {
      asm("nop");/* This stops it optimising code out */
    }
  }
}

void f3d_delay_init(void) {
  TIM_TimeBaseInitTypeDef  TIM_TimeBaseStructure;
  TIM_OCInitTypeDef  TIM_OCInitStructure;
  uint16_t PrescalerValue = 0;

  __IO uint16_t CCR1_Val = 40961;
  
  RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM3, ENABLE);
  //  PrescalerValue = (uint16_t) ((SystemCoreClock) / 72000000) - 1;
  PrescalerValue = (uint16_t) ((SystemCoreClock) / 36000000) - 1;

  
  TIM_TimeBaseStructure.TIM_Period = 65535;
  TIM_TimeBaseStructure.TIM_Prescaler = 0;
  TIM_TimeBaseStructure.TIM_ClockDivision = 0;
  TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;
  
  TIM_TimeBaseInit(TIM3, &TIM_TimeBaseStructure);

  TIM_PrescalerConfig(TIM3, PrescalerValue, TIM_PSCReloadMode_Immediate);

  TIM_OCInitStructure.TIM_OCMode = TIM_OCMode_Timing;
  TIM_OCInitStructure.TIM_OutputState = TIM_OutputState_Enable;
  TIM_OCInitStructure.TIM_Pulse = CCR1_Val;
  TIM_OCInitStructure.TIM_OCPolarity = TIM_OCPolarity_High;
  
  TIM_OC1Init(TIM3, &TIM_OCInitStructure);

  TIM_OC1PreloadConfig(TIM3, TIM_OCPreload_Disable);

  TIM_Cmd(TIM3, ENABLE);
}

void f3d_delay_uS(uint16_t uS_count) {
  if (uS_count >= 1000) {
    uS_count = 1000;
  }
  uS_count *= 36; 
  TIM_Cmd(TIM3, DISABLE);
  TIM_SetCounter(TIM3,0);
  TIM_SetCompare1(TIM3,uS_count);
  TIM_Cmd(TIM3, ENABLE);  
  while (TIM_GetFlagStatus(TIM3,TIM_FLAG_CC1)==RESET);
  TIM_ClearFlag(TIM3,TIM_FLAG_CC1);
  TIM_Cmd(TIM3, DISABLE);  
} 



/* f3d_delay.c ends here */
