TARGET=arm-elf
GPRBUILD=gprbuild
GPRFLAGS=--target=$(TARGET)
BUILDDIR=Ada_Drivers_Library/examples/shared/hello_world_blinky/obj/stm32f429disco
CROSS=arm-none-eabi
PATH:=$(HOME)/opt/GNAT/2018-arm-elf/bin:$(PATH)

OBJCOPY=$(CROSS)-objcopy

STFLASH ?= st-flash

PROJ	= blinky
ELF	= $(PROJ).elf
HEX	= $(PROJ).hex
BIN	= $(PROJ).bin

GPR = Ada_Drivers_Library/examples/STM32F429_Discovery/blinky_f429disco.gpr

all:: $(BIN)

report::
	$(MAKE) -C report

$(HEX): $(ELF)
	$(OBJCOPY) -O ihex $(PROJ).elf $(PROJ).hex

$(BIN): $(ELF)
	$(OBJCOPY) -O binary $(PROJ).elf $(PROJ).bin

$(ELF):
	$(GPRBUILD) $(GPRFLAGS) $(GPR)
	cp $(BUILDDIR)/$(PROJ) $(ELF)

flash:: $(BIN)
	$(STFLASH) write $(BIN) 0x8000000

clean::
	$(MAKE) -C report clean
	$(RM) $(ELF) $(HEX) $(OBJ) $(BIN)

.PHONY: all clean flash
