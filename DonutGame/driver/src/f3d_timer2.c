/* f3d_timer2.c --- 
 * 
 * Filename: f3d_timer2.c
 * Description: 
 * Author: Bryce Himebaugh
 * Maintainer: 
 * Created: Mon Nov 18 14:08:23 2013
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

#include <f3d_timer2.h> 

void f3d_timer2_init(void) {
   TIM_TimeBaseInitTypeDef TIM_TimeBaseStructure;   
   RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM2, ENABLE);
   TIM_TimeBaseStructInit(&TIM_TimeBaseStructure); 
   //   TIM_TimeBaseStructure.TIM_Period = 0xFF;          
   //   TIM_TimeBaseStructure.TIM_Period = SystemCoreClock/ - 1;          
   TIM_TimeBaseStructure.TIM_Period = 3200;          

   TIM_TimeBaseStructure.TIM_Prescaler = 0x00;       
   TIM_TimeBaseStructure.TIM_ClockDivision = 0x0;    
   TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;  
   TIM_TimeBaseInit(TIM2, &TIM_TimeBaseStructure);
   TIM_SelectOutputTrigger(TIM2, TIM_TRGOSource_Update);
   TIM_Cmd(TIM2, ENABLE);
}

void f3d_timer2_enable(uint32_t samplerate) {
   static TIM_TimeBaseInitTypeDef TIM_TimeBaseStructure;
   uint32_t period;
   RCC_APB1PeriphClockCmd(RCC_APB1Periph_TIM2, ENABLE);
   TIM_TimeBaseStructInit(&TIM_TimeBaseStructure); 
   TIM_TimeBaseStructure.TIM_Prescaler = 0x0;       
   TIM_TimeBaseStructure.TIM_ClockDivision = 0x0;    
   TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;  
   period = (SystemCoreClock/(samplerate<<1)) - 1;
   TIM_TimeBaseStructure.TIM_Period = (uint16_t) period;
   TIM_TimeBaseStructure.TIM_Period = period;          
   TIM_TimeBaseInit(TIM2, &TIM_TimeBaseStructure);
   TIM_Cmd(TIM2, ENABLE);
}

void f3d_timer2_disable(void) {
  TIM_Cmd(TIM2, DISABLE);
}

void f3d_timer2_setperiod(uint16_t period) {
   static TIM_TimeBaseInitTypeDef TIM_TimeBaseStructure;
   TIM_TimeBaseStructInit(&TIM_TimeBaseStructure); 
   TIM_TimeBaseStructure.TIM_Prescaler = 0x0;       
   TIM_TimeBaseStructure.TIM_ClockDivision = 0x0;    
   TIM_TimeBaseStructure.TIM_CounterMode = TIM_CounterMode_Up;  
   if (period < 0x1F) period = 0x1F;
   period = period << 4;
   TIM_TimeBaseStructure.TIM_Period = period;          
   TIM_TimeBaseInit(TIM2, &TIM_TimeBaseStructure);
}


/* f3d_timer2.c ends here */
