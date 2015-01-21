
DOTS=$(wildcard *.dot)
PNGS=$(addsuffix .png, $(basename $(DOTS)))
PYS=$(wildcard contingent/*.py)

all: .up-to-date~ $(PNGS)
# diagram-example.png

.up-to-date~: chapter.rst $(PYS)
	python3 -m doctest -o ELLIPSIS -f chapter.rst
	touch .up-to-date~

$(PNGS): %.png: %.dot
	dot -Tpng $< > $@

html: chapter.html
chapter.html: chapter.rst
	rst2html chapter.rst chapter.html
