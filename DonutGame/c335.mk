V = 0

# name of executable

ELF=$(notdir $(CURDIR)).elf                    
BIN=$(notdir $(CURDIR)).bin

# Tool path
TOOLROOT=/l/arm2/devtools/bin/

# Library path
LIBROOT=/l/arm2/STM32F3-Discovery_FW_V1.1.0

# Tools
CC_0 = @echo -n "Compiling $< "; $(CC_1)
CC_1 = $(TOOLROOT)/arm-none-eabi-gcc
CC = $(CC_$(V))

GAS_0 = @echo -n "Assembling $< "; $(GAS_1)
GAS_1 = $(TOOLROOT)/arm-none-eabi-gcc
GAS = $(GAS_$(V))


DD_0 = @echo " "; $(DD_1)
DD_1 = $(TOOLROOT)/arm-none-eabi-gcc
DD = $(DD_$(V))

LD_0 = @echo "Linking $@"; $(LD_1)
LD_1=$(TOOLROOT)/arm-none-eabi-gcc
LD = $(LD_$(V))

DL_0 = @echo -n "Downloading"; $(DL_1)
DL_1=$(TOOLROOT)/st-flash 
DL = $(DL_$(V))

OC_0 = @echo "Creating Bin Downloadable File"; $(OC_1)
OC_1=$(TOOLROOT)/arm-none-eabi-objcopy
OC = $(OC_$(V))

GP_0 = @echo " "; $(GP_1)
GP_1=grep
GP = $(GP_$(V))

RM_0 = @echo "Cleaning Directory"; $(RM_1)
RM_1=rm
RM = $(RM_$(V))

AR=$(TOOLROOT)/arm-none-eabi-ar
AS=$(TOOLROOT)/arm-none-eabi-as
OBJCOPY=$(TOOLROOT)/arm-none-eabi-objcopy

# Code Paths

DEVICE=$(LIBROOT)/Libraries/CMSIS/Device/ST/STM32F30x
CORE=$(LIBROOT)/Libraries/CMSIS/Include
PERIPH=$(LIBROOT)/Libraries/STM32F30x_StdPeriph_Driver
SYSTEM_FILE=$(LIBROOT)/Libraries/CMSIS/Device/ST/STM32F30x/Source/Templates
STARTUP_FILE=..
ACCEL_GYRO=$(LIBROOT)/Utilities/STM32F3_Discovery
LOCAL_DRIVERS=../driver
FATFS =./ff9b

# Search path for standard files

vpath %.c $(TEMPLATEROOT)

# Search path for perpheral library

vpath %.c $(CORE)
vpath %.c $(PERIPH)/src
vpath %.c $(DEVICE)

vpath %.c $(SYSTEM_FILE)
vpath %.s $(STARTUP_FILE)

vpath %.c $(ACCEL_GYRO)
vpath %.c $(LOCAL_DRIVERS)/src
vpath %.c $(FATFS)/src

#  Processor specific

LDSCRIPT = ../stm32f3xx.ld
STARTUP = startup_stm32f30x.o system_stm32f30x.o

# Compilation Flags

FULLASSERT = -DUSE_FULL_ASSERT 

# LDLIBS = --specs=nosys.specs -lm 
LDLIBS = -lm 
LDFLAGS+= -T$(LDSCRIPT) -mthumb -mcpu=cortex-m4
CFLAGS+= -mcpu=cortex-m4 -mthumb 
CFLAGS+= -I$(TEMPLATEROOT) -I$(DEVICE) -I$(CORE) -I$(PERIPH)/inc -I.
CFLAGS+= -DUSE_STDPERIPH_DRIVER $(FULLASSERT) 
CFLAGS+= -I$(DEVICE)/Include -I$(CORE)
CFLAGS+= -I$(LIBROOT)/Project/Peripheral_Examples/GPIO_IOToggle
CFLAGS+= -I$(ACCEL_GYRO)
CFLAGS+= -I$(LOCAL_DRIVERS)/inc
CFLAGS+= -I$(FATFS)/src
CFLAGS+= -Wno-multichar

# Build executable 

$(ELF) : $(OBJS)
	$(LD) $(LDFLAGS) -o $@ $(OBJS) $(LDLIBS)

# compile and generate dependency info

%.o: %.c
	$(CC) -c $(CFLAGS) $< -o $@
	$(DD) -MM $(CFLAGS) $< > $*.d

%.o: %.s
	$(GAS) -c $(CFLAGS) $< -o $@
	$(DD) -MM $(CFLAGS) $< > $*.d

%.bin: %.elf
	$(OC) -O binary $< $@

clean:
	$(RM) -f $(OBJS) $(OBJS:.o=.d) $(ELF) startup_stm32f* $(CLEANOTHER) $(BIN) *.d

debug: $(ELF)
	arm-none-eabi-gdb $(ELF)

download: $(BIN)
	$(DL) write $(BIN) 0x8000000 > st-flash.log 2>&1
	$(GP) -o "jolly" st-flash.log | sed 's/jolly/Success/'
	$(GP) -o "Couldn" st-flash.log | sed 's/Couldn/Fail/'

etags:
	find $(PERIPH) -type f -iname "*.[ch]" | xargs etags --append
	find $(DEVICE) -type f -iname "*.[ch]" | xargs etags --append
	find $(CORE) -type f -iname "*.[ch]" | xargs etags --append
	find $(ACCEL_GYRO) -type f -iname "*.[ch]" | xargs etags --append
	find $(LOCAL_DRIVERS) -type f -iname "*.[ch]" | xargs etags --append
	find $(LIBROOT)/Project/Peripheral_Examples/GPIO_IOToggle -type f -iname "*.[ch]" | xargs etags --append
	find . -type f -iname "*.[ch]" | xargs etags --append

all: $(ELF)

# pull in dependencies

-include $(OBJS:.o=.d)





