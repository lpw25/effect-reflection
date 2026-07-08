# Builds three versions of the paper and the artifact tarball:
#
#   paper.pdf               full paper, with appendices
#   paper-no-appendix.pdf   paper without the appendices
#   paper-appendix-only.pdf just the appendices
#   artifact.tar.gz         artifact directory (sans build artifacts)
#                           plus paper-appendix-only.pdf
#
# The paper is compiled once in full and then split at the page where
# the appendices start (recorded in paper.aux by paper.tex), so page
# numbers, theorem numbers and cross-references are consistent across
# all three PDFs.

PDFS = paper.pdf paper-no-appendix.pdf appendix.pdf

GS_SPLIT = gs -q -dNOPAUSE -dBATCH -sDEVICE=pdfwrite

# Shell snippet: the page number on which the appendices start.
APPENDIX_PAGE = sed -n 's/\\newlabel{appendix-firstpage}{{}{\([0-9]*\)}.*/\1/p' paper.aux

all: $(PDFS)

paper.pdf: paper.tex references.bib
	latexmk -pdf paper.tex

paper-no-appendix.pdf: paper.pdf
	p=$$($(APPENDIX_PAGE)); \
	$(GS_SPLIT) -dFirstPage=1 -dLastPage=$$((p - 1)) -sOutputFile=$@ paper.pdf

appendix.pdf: paper.pdf
	p=$$($(APPENDIX_PAGE)); \
	$(GS_SPLIT) -dFirstPage=$$p -sOutputFile=$@ paper.pdf

# Always re-tar: make cannot cheaply track every file under artifact/.
tarball: appendix.pdf
	tar czf supplimentary.tar.gz \
	  --exclude=artifact/_build \
	  --exclude=artifact/_opam \
	  artifact appendix.pdf

clean:
	latexmk -C paper.tex
	rm -f paper-no-appendix.pdf appendix.pdf artifact.tar.gz

.PHONY: all tarball clean
