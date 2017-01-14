/* f3d_accel.c --- 
 * 
 * Filename: f3d_accel.c
 * Description: 
 * Author: Bryce Himebaugh
 * Maintainer: 
 * Created: Thu Oct 31 09:14:43 2013
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
#include <f3d_accel.h>

void f3d_accel_init() {
  // Accelerometer I2C Address = 0x32 

  uint8_t value = 0; 
 
  value = 0x40|0x07;                 // Normal Power Mode, All Axis Enable (X,Y,Z)
  f3d_i2c1_write(0x32, 0x20, &value); // Accel (0x32), Ctrl Reg1 (0x20) 

  value = 0x08;                      // Continuous Update, Little Endian, 2G Sensitivity, High Resolution Mode
  f3d_i2c1_write(0x32, 0x23, &value); // Accel (0x32, Ctrl Reg4  (0x23)


  value = 0x10 | 0x80;               // HPF Cutoff 16, High Pass Filter Normal Mode, AO1/AO1 Disable
  f3d_i2c1_write(0x32, 0x21, &value); // Accel (0x32, Ctrl Reg2  (0x21)
}

void f3d_accel_read(float *accel_data) {
  int16_t raw_data[3];
  uint8_t buffer[6];
  int i;

  f3d_i2c1_read(0x32, 0x28, buffer, 6); // Read 6 bytes starting from accel (0x32), starting at register 0x28. 
  for (i=0; i<3; i++) {
    raw_data[i]=((int16_t)((uint16_t)buffer[2*i+1] << 8) + buffer[2*i])/(uint8_t)16;
    accel_data[i]=(float)raw_data[i]/1000.0;
  }
}

/* f3d_accel.c ends here */

