/* f3d_dac.h --- 
 * 
 * Filename: f3d_dac.h
 * Description: 
 * Author: Bryce Himebaugh
 * Maintainer: 
 * Created: Mon Nov 18 14:13:41 2013
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

#define DAC_DHR12R1_ADDRESS      0x40007408
#define DAC_DHR12L1_ADDRESS      0x4000740C
#define DAC_DHR8R1_ADDRESS       0x40007410

#define DAC_DHR12R2_ADDRESS      0x40007414
#define DAC_DHR12L2_ADDRESS      0x40007418
#define DAC_DHR8R2_ADDRESS       0x4000741C

void f3d_dac_init(void);
void f3d_dac_channel_setup(void);
void audioplayerStart(void);
void audioplayerStop(void);

/* f3d_dac.h ends here */
