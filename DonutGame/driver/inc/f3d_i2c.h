/* f3d_i2c.h --- 
 * 
 * Filename: f3d_i2c.h
 * Description: 
 * Author: Bryce Himebaugh
 * Maintainer: 
 * Created: Thu Oct 31 09:11:48 2013
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

void f3d_i2c1_init();
void f3d_i2c1_read(uint8_t device, uint8_t reg, uint8_t* buffer, uint16_t numbytes);
void f3d_i2c1_read_nunchuck (uint8_t device, uint8_t* buffer, uint16_t numbytes);
void f3d_i2c1_write(uint8_t device, uint8_t reg, uint8_t* value);


/* f3d_i2c.h ends here */
