TEX = pdflatex -shell-escape -interaction=nonstopmode -file-line-error
GS = gs -sDEVICE=pdfwrite -dCompatibilityLevel=1.4 -dPDFSETTINGS=/printer -dNOPAUSE -dBATCH -sOutputFile=
VIEWER = zathura

TEX_PATH = tex
TEX_FILES = conclusion.tex glossaire.tex travail.tex docs.tex introduction.tex page_de_garde.tex annexes.tex
PDF = main.pdf

all:: report

report: $(PDF)

$(PDF): $(foreach file,$(TEX_FILES),$(TEX_PATH)/$(file))

%.pdf: $(TEX_PATH)/%.tex
	rubber -d $<

report-compress: report
	$(GS)"compressed-$(PDF)" $(PDF)

view: report
	$(VIEWER) $(PDF)

clean::
	$(RM) *.log *.aux *.pdf *.toc *.out *.dvi *.ptc *.o

.PHONY: all clean view report-compress report
