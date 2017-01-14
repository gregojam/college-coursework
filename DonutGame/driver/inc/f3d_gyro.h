/* f3d_gyro.h */

/* Code: */
#include <stm32f30x.h>

#define L3G_Sensitivity_250dps   (float) 114.285f
#define L3G_Sensitivity_500dps   (float) 57.1429f
#define L3G_Sensitivity_2000dps   (float) 14.285f

#define GYRO_CS_LOW() GPIO_ResetBits(GPIOE, GPIO_Pin_3) 
#define GYRO_CS_HIGH() GPIO_SetBits(GPIOE, GPIO_Pin_3) 

void f3d_gyro_interface_init();
void f3d_gyro_init(void);
void f3d_gyro_read(uint8_t* pBuffer, uint8_t ReadAddr, uint16_t NumByteToRead);
void f3d_gyro_write(uint8_t*, uint8_t, uint16_t);
static uint8_t f3d_gyro_sendbyte(uint8_t);
void f3d_gyro_getdata(float *pfData);

/* f3d_gyro.h ends here */
