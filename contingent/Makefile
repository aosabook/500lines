
DOTS=$(wildcard *.dot)
PNGS=$(addsuffix .png, $(basename $(DOTS)))
PYS=$(wildcard contingent/*.py)

all: chapter.html $(PNGS)
# diagram-example.png

chapter.html: chapter.rst $(PYS)
	python3 -m doctest -o ELLIPSIS -f chapter.rst
	rst2html.py chapter.rst chapter.html

$(PNGS): %.png: %.dot
	dot -Tpng $< > $@
