/* f3d_rtc.c --- 
 * 
 * Filename: f3d_rtc.c
 * Description: 
 * Author: Bryce Himebaugh
 * Maintainer: 
 * Created: Thu Nov  7 09:10:42 2013
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

#include <f3d_rtc.h>

void f3d_rtc_init(void) {
  RTC_TimeTypeDef  RTC_TimeStructure;
  RTC_DateTypeDef  RTC_DateStructure;
  RTC_InitTypeDef  RTC_InitStructure;
  
  RCC_APB1PeriphClockCmd(RCC_APB1Periph_PWR, ENABLE);
  PWR_BackupAccessCmd(ENABLE);   // Enable access to write to the RTC Registers
  RCC_BackupResetCmd(ENABLE);
  RCC_BackupResetCmd(DISABLE);

  PWR_BackupAccessCmd(ENABLE);

  /* LSI used as RTC source clock*/
  RCC_LSICmd(ENABLE);
  
  /* Wait till LSI is ready */  
  while(RCC_GetFlagStatus(RCC_FLAG_LSIRDY) == RESET);
  
  RCC_RTCCLKConfig(RCC_RTCCLKSource_LSI);
  RCC_RTCCLKCmd(ENABLE);
  RTC_WaitForSynchro(); 
  
  RTC_StructInit(&RTC_InitStructure);   // Set the structure members to their default values
  RTC_InitStructure.RTC_HourFormat = RTC_HourFormat_24;
  RTC_InitStructure.RTC_AsynchPrediv = 88;
  RTC_InitStructure.RTC_SynchPrediv = 470;
  RTC_Init(&RTC_InitStructure); 

  RTC_DateStructure.RTC_Year = 13;
  RTC_DateStructure.RTC_Month = RTC_Month_November;
  RTC_DateStructure.RTC_Date = 7;
  RTC_DateStructure.RTC_WeekDay = RTC_Weekday_Thursday;
  RTC_SetDate(RTC_Format_BCD, &RTC_DateStructure);

  RTC_TimeStructure.RTC_H12     = RTC_H12_AM;
  RTC_TimeStructure.RTC_Hours   = 0x09;
  RTC_TimeStructure.RTC_Minutes = 0x05;
  RTC_TimeStructure.RTC_Seconds = 0x00;  
  RTC_SetTime(RTC_Format_BIN, &RTC_TimeStructure);
}

uint32_t get_fattime(void) {
  // Return a packed 32-bit word 
  // bit31:25 Year from 1980 (0..127)
  // bit24:21 Month (1..12)
  // bit20:16 Day in month(1..31)
  // bit15:11 Hour (0..23)
  // bit10:5 Minute (0..59)
  // bit4:0 Second / 2 (0..29)
  uint32_t compressed_time = 0;
  uint32_t temp = 0;
  RTC_DateTypeDef RTC_DateStructure;
  RTC_TimeTypeDef RTC_TimeStructure;

  RTC_GetDate(RTC_Format_BIN, &RTC_DateStructure); 
  RTC_GetTime(RTC_Format_BIN, &RTC_TimeStructure);
  
  //  printf("%d\n",RTC_DateStructure.RTC_Year);
  compressed_time |= (((2000-1980) + RTC_DateStructure.RTC_Year) << 25);
  compressed_time |= (RTC_DateStructure.RTC_Month << 21);
  compressed_time |= (RTC_DateStructure.RTC_Date << 16);
  compressed_time |= (RTC_TimeStructure.RTC_Hours << 11);
  compressed_time |= (RTC_TimeStructure.RTC_Minutes<<5);
  compressed_time |= RTC_TimeStructure.RTC_Seconds/2;
  return (compressed_time);
}




/* f3d_rtc.c ends here */
