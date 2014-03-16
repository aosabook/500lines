
all: diagram1.png diagram2.png diagram3.png diagram-example.png

diagram1.png diagram2.png diagram3.png diagram-example.png: diagram%.png: diagram%.dot
	dot -Tpng $< > $@
