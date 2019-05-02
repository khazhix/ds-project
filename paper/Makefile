PACKAGE=acmart

DOCS = \
	paper.tex \


PDF = ${DOCS:%.tex=%.pdf}

all:  ${PDF}

%.pdf:  %.tex   $(PACKAGE).cls ACM-Reference-Format.bst
	pdflatex $<
	- bibtex $*
	pdflatex $<
	pdflatex $<
	while ( grep -q '^LaTeX Warning: Label(s) may have changed' $*.log) \
	do pdflatex $<; done

paper.pdf: paper-body.tex

clean:
	$(RM) *.log *.aux \
	*.cfg *.glo *.idx *.toc \
	*.ilg *.ind *.out *.lof \
	*.lot *.bbl *.blg *.gls *.cut *.hd \
	*.dvi *.ps  *.thm *.tgz *.zip *.rpi
