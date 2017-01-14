/* f3d_systick.c --- 
 * 
 * Filename: f3d_systick.c
 * Description: 
 * Author: Bryce Himebaugh
 * Maintainer: 
 * Created: Thu Nov 14 07:57:37 2013
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

#include <f3d_systick.h>
#include <f3d_led.h> 
#include <f3d_user_btn.h>
#include <f3d_uart.h>
#include <queue.h>

volatile int systick_flag = 0;
queue_t txbuf;
int light = 0;
int i = 0;


void f3d_systick_init(void) {
  SysTick_Config(SystemCoreClock/100);
}

void SysTick_Handler(void) {
	putchar(getchar());
	if (!queue_empty(&txbuf)) {
    		flush_uart();
  	}
	if(user_btn_read()){
		if(i > 9){
			f3d_led_off((light-1)%8);
			f3d_led_on(light%8);
			light++;
			i = 0;
		}
		else
			i++;
	}
	else{
		f3d_led_off((light-1)%8);
		f3d_led_on(light%8);
		light++;
		i = 0;
	}
}

/* f3d_systick.c ends here */
