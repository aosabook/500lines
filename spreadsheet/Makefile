OBJECTS = dist/worker.js dist/main.js

run ::
	@node extra/static-here.js

dist :: $(OBJECTS)

test :: dist
	@node extra/test.js

wc :: dist
	@wc -l src/* index.html

clean ::
	-@rm -rf dist/*

dist/% :: %
	traceur --validate --experimental --out tmp --script $<
	cat lib/traceur-runtime.js tmp/$< > $@
	@rm -rf tmp

.PHONY: run dist test clean wc
