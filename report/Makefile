TEX = pdflatex -shell-escape -interaction=nonstopmode -file-line-error
GS = gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer -dNOPAUSE -dBATCH -sOutputFile=
VIEWER = zathura

TEX_PATH = tex
ADA_TEX_FILES = conclusion_ada.tex glossary.tex work.tex introduction_ada.tex \
						cover.tex appendices.tex common_headers.tex introduction_common.tex
DO178_TEX_FILES = conclusion_do178.tex glossary.tex work.tex introduction_do178.tex \
						cover.tex appendices.tex specs.tex common_headers.tex introduction_common.tex

COMPRESSED_SUFIX=-compress

ADA_NAME = ada_report
ADA_PDF = $(ADA_NAME).pdf
ADA_PDF_COMPRESSED = $(ADA_NAME)$(COMPRESSED_SUFIX).pdf
DO178_NAME = do178_report
DO178_PDF = $(DO178_NAME).pdf
DO178_PDF_COMPRESSED = $(DO178_NAME)$(COMPRESSED_SUFIX).pdf

all:: ada_report do178_report

all_compressed: ada_report_compressed do178_report_compressed

ada_report: $(ADA_PDF)
$(ADA_PDF): $(foreach file,$(ADA_TEX_FILES),$(TEX_PATH)/$(file))
ada_report_compressed: $(ADA_PDF_COMPRESSED)

ada_view: ada_report
	$(VIEWER) $(ADA_PDF)

do178_report: $(DO178_PDF)
$(DO178_PDF): $(foreach file,$(DO178_TEX_FILES),$(TEX_PATH)/$(file))
do178_report_compressed: $(DO178_PDF_COMPRESSED)

do178_view: do178_report
	$(VIEWER) $(DO178_PDF)

%.pdf: $(TEX_PATH)/%.tex
	rubber -d $<

%$(COMPRESSED_SUFIX).pdf: %.pdf
	$(GS)$@ $<

clean::
	$(RM) *.log *.aux *.pdf *.toc *.out *.dvi *.ptc *.o

.PHONY: all clean view report-compress report
