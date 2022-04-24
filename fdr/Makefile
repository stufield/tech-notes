# ----------------------------------- #
#   Makefile for Analysis Reports
# ----------------------------------- #
MV = mv -f
RM = rm -rf
CP = cp
TOUCH = touch
REXE = R -q --vanilla
RCMD = $(REXE) CMD
RSCRIPT = Rscript --vanilla
PDFLATEX = pdflatex
#########################
.PRECIOUS: Makefile
.PHONY: clean
#########################
%.pdf: %.tex
	$(PDFLATEX) $*
	$(PDFLATEX) $*

%.html: %.Rmd
	$(RSCRIPT) -e "rmarkdown::render('$*.Rmd', quiet = TRUE)"

%.tex: %.Rnw
	$(RSCRIPT) -e "library(knitr); knit('$*.Rnw')"

clean:
	$(MV) *.log *.aux *.toc *.blg *.bbl *.out /tmp

