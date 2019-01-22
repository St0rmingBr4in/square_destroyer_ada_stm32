TARGET = arm-elf
GPRBUILD = gprbuild
GPRFLAGS = --target=$(TARGET)
PATH := $(HOME)/opt/GNAT/2018-arm-elf/bin:$(PATH)
CROSS = arm-none-eabi
OBJCOPY = $(CROSS)-objcopy
STFLASH ?= st-flash

GPR = Ada_Drivers_Library/examples/STM32F429_Discovery/blinky_f429disco.gpr
BUILDDIR = Ada_Drivers_Library/examples/shared/hello_world_blinky/obj/stm32f429disco
PROJ	= blinky
ELF	= $(PROJ).elf
HEX	= $(PROJ).hex
BIN	= $(PROJ).bin

TEX = pdflatex -shell-escape -interaction=nonstopmode -file-line-error
GS = gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer -dNOPAUSE -dBATCH -sOutputFile=
VIEWER = zathura

TEX_PATH = tex
TEX_FILES = conclusion.tex glossaire.tex travail.tex docs.tex introduction.tex page_de_garde.tex annexes.tex
PDF = main.pdf

all:: $(BIN) $(PDF)

bin:: $(BIN)

$(HEX): $(ELF)
	$(OBJCOPY) -O ihex $(PROJ).elf $(PROJ).hex

$(BIN): $(ELF)
	$(OBJCOPY) -O binary $(PROJ).elf $(PROJ).bin

$(ELF):
	$(GPRBUILD) $(GPRFLAGS) $(GPR)
	cp $(BUILDDIR)/$(PROJ) $(ELF)

flash:: $(BIN)
	$(STFLASH) write $(BIN) 0x8000000

report: $(PDF)

$(PDF): $(foreach file,$(TEX_FILES),$(TEX_PATH)/$(file))

%.pdf: $(TEX_PATH)/%.tex
	rubber -d $<

report-compress: $(PDF)
	$(GS)"compressed-$(PDF)" $(PDF)

view: $(PDF)
	$(VIEWER) $(PDF)

clean::
	$(RM) $(ELF) $(HEX) $(OBJ) $(BIN) *.log *.aux *.pdf *.toc *.out *.dvi *.ptc *.o

.PHONY: all clean flash view report-compress report bin
