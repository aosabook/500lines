
DOTS=$(wildcard *.dot)
PNGS=$(addsuffix .png, $(basename $(DOTS)))

all: .up-to-date~ $(PNGS)
# diagram-example.png

.up-to-date~: chapter.rst
	python3 -m doctest -o ELLIPSIS -f chapter.rst
	touch .up-to-date~

$(PNGS): %.png: %.dot
	dot -Tpng $< > $@
