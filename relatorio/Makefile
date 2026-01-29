TEX=relatorio.tex
PDF=relatorio.pdf

all: $(PDF)

$(PDF): $(TEX)
	pdflatex $(TEX)
	pdflatex $(TEX)

clean:
	rm -f *.aux *.log *.toc *.out *.lof *.lot *.fls *.fdb_latexmk $(PDF)
