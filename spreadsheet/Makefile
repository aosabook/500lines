OBJECTS = es5/worker.js es5/main.js

all :: deps es5 wc run

deps ::
	@npm i

run ::
	@node node_modules/static-here/bin/static-here

es5 :: $(OBJECTS)
	@perl -lpi -e 's!"sources":\["\K[^"]*/!../!' es5/*.map
	@perl -lpi -e 's~vals;(?! )~vals; importScripts("../node_modules/traceur/bin/traceur-runtime.js");~' es5/worker.js

test :: es5
	@node node_modules/mocha/bin/mocha

wc ::
	@echo -n Lines:
	@grep -v '//\|^$$' *.js *.html *css | wc -l

clean ::
	-@rm -rf es5/*.js es5/*.map

es5/% :: %
	-@./node_modules/.bin/traceur --source-maps --validate --block-binding true --out es5 --script $<

.PHONY: deps run es5 test clean wc
