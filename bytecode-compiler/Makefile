all: tailbiter0.py tailbiter1.py tailbiter2.py

doc: ~/Desktop/ch1.pdf

~/Desktop/ch1.pdf: bytecompiler.rewrite.md
	pandoc bytecompiler.rewrite.md -o ~/Desktop/ch1.pdf

tailbiter0.py: handaxeweb.lua bytecompiler.rewrite.md
	./handaxeweb.lua <bytecompiler.rewrite.md tailbiter.py >tailbiter0.py

tailbiter1.py: handaxeweb.lua bytecompiler.rewrite.md
	./handaxeweb.lua <bytecompiler.rewrite.md tailbiter.py 1 >tailbiter1.py

tailbiter2.py: handaxeweb.lua bytecompiler.rewrite.md
	./handaxeweb.lua <bytecompiler.rewrite.md tailbiter.py 2 >tailbiter2.py


compiler.py non-compiler: extractcode.py bytecompiler.md
	python extractcode.py <bytecompiler.md
