/* f3d_mag.c --- 
 * 
 * Filename: f3d_mag.c
 * Description: 
 * Author: Bryce Himebaugh
 * Maintainer: 
 * Created: Thu Oct 31 09:27:11 2013
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


#include <f3d_i2c.h>
#include <f3d_mag.h>

void f3d_mag_init() {
  // MAG I2C Address = 0x3C 

  uint8_t value = 0; 
 
  value = 0x14;                  //Temp sensor disable,30Hz Output Rate 
  f3d_i2c1_write(0x3C, 0x00,  &value); // Mag (0x3C), CRA (0x00) 

  value = 0xE0;                      // +/- 8.1 Gauss Full Scale
  f3d_i2c1_write(0x3C, 0x01, &value); // Mag (0x3C), CRB (0x01)

  value = 0x00;                      // Continuous Conversion
  f3d_i2c1_write(0x3C, 0x02, &value); // Mag (0x3C), MR  (0x23)
}

void f3d_mag_read(float *mag_data) {
  uint8_t buffer[6];
  int i;
  
  f3d_i2c1_read(0x3C, 0x03, buffer,2);   // Read X Axis
  f3d_i2c1_read(0x3C, 0x07, buffer+2,2); // Read Y Axis
  f3d_i2c1_read(0x3C, 0x05, buffer+4,2); // Read Z Axis (notice that Z is out of order in the chip). 
  
  for (i=0; i<2; i++) {
    mag_data[i]=(float)((int16_t)(((uint16_t)buffer[2*i] << 8) + buffer[2*i+1]))/230;
  }
  mag_data[2]=(float)((int16_t)(((uint16_t)buffer[4] << 8) + buffer[5]))/205;
}





/* f3d_mag.c ends here */

