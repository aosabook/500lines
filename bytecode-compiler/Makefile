all: bytecompile0.py bytecompile1.py bytecompile2.py compiler.py

compiler.py non-compiler: extractcode.py bytecompiler.md
	python extractcode.py <bytecompiler.md

bytecompile0.py: handaxeweb.lua bytecompiler.rewrite.md
	./handaxeweb.lua <bytecompiler.rewrite.md bytecompile.py >bytecompile0.py

bytecompile1.py: handaxeweb.lua bytecompiler.rewrite.md
	./handaxeweb.lua <bytecompiler.rewrite.md bytecompile.py 1 >bytecompile1.py

bytecompile2.py: handaxeweb.lua bytecompiler.rewrite.md
	./handaxeweb.lua <bytecompiler.rewrite.md bytecompile.py 2 >bytecompile2.py
