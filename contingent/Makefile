
all: diagram1.png diagram2.png

diagram1.png diagram2.png: diagram%.png: diagram%.dot
	dot -Tpng $< > $@
