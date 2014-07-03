OBJECTS = dist/worker.js dist/main.js

all :: dist run

run ::
	@node extra/static-here.js

dist :: $(OBJECTS)

test :: dist
	@node extra/test.js

wc :: dist
	@grep -v '//' *.js *.html *css | wc -l

clean ::
	-@rm -rf dist/*

dist/% :: %
	traceur --validate --experimental --out tmp --script $<
	echo "// Generated File - Do Not Edit" | cat - lib/traceur-runtime.js tmp/$< > $@
	@rm -rf tmp

.PHONY: run dist test clean wc
