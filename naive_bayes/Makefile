# ----------------------------------- #
# GNU Make Automatic Variables:
# ----------------------------------- #
# $*: stem of target
# $@: filename of target 
# $%: target member name
# $<: the first prerequisites for target
# $^: prerequisites for target
# $?: prerequisites newer than target
#######################
# Local Variables:
#######################
MV = mv -f
RM = rm -rf
TOUCH = touch
RSCRIPT = Rscript --vanilla
PDFLATEX = pdflatex --interaction=batchmode
#########################
.PRECIOUS: %.tex Makefile
.PHONY: clean
#########################

%.pdf: %.tex
	@$(PDFLATEX) $*
	@$(PDFLATEX) $*

%.html: %.Rmd bayes-data.rds
	$(RSCRIPT) -e "rmarkdown::render('$*.Rmd', quiet = TRUE)"

clean:
	$(RM) *.log *.aux *.toc *.blg *.bbl *.out

