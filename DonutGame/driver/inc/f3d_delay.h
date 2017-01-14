/* f3d_delay.h --- 
 * 
 * Filename: f3d_delay.h
 * Description: 
 * Author: Bryce Himebaugh
 * Maintainer: 
 * Created: Thu Oct 24 06:35:27 2013
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

void delay(int);
void f3d_delay_init(void);
void f3d_delay_uS(uint16_t);

/* f3d_delay.h ends here */
