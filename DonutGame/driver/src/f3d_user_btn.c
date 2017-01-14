//Brent Hall
//Partner: Isaac Fisch
/**********************************
 *f3d_user_btn.c
 *contains the init and read functions for the User Button
 *********************************/
#include <stm32f30x.h>
#include <stm32f30x_gpio.h>
#include <stm32f30x_rcc.h>


/*Initialization of the UserButton*/
void f3d_user_btn_init(void){
	GPIO_InitTypeDef GPIO_InitStructure;
	GPIO_StructInit(&GPIO_InitStructure);
	GPIO_InitStructure.GPIO_Pin = GPIO_Pin_0;
	GPIO_InitStructure.GPIO_Mode = GPIO_Mode_IN;
	GPIO_InitStructure.GPIO_PuPd = GPIO_PuPd_NOPULL;
	RCC_AHBPeriphClockCmd(RCC_AHBPeriph_GPIOA, ENABLE);
	GPIO_Init(GPIOA, &GPIO_InitStructure);
}

/*reads the User Button*/
int user_btn_read(void){
	GPIO_ReadInputDataBit(GPIOA,GPIO_Pin_0);
}
