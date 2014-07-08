OBJECTS = es5/worker.js es5/main.js

all :: deps es5 wc run

deps ::
	@npm i

run ::
	@node node_modules/static-here/bin/static-here

es5 :: $(OBJECTS)

test :: es5
	@node node_modules/mocha/bin/mocha

wc ::
	@echo -n Lines:
	@grep -v '//\|^$$' *.js *.html *css | wc -l

clean ::
	-@rm -rf es5/*.js

es5/% :: %
	./node_modules/traceur/traceur --source-maps --validate --experimental --out tmp --script $<
	echo "// Generated File - Do Not Edit" | cat - ./node_modules/traceur/bin/traceur-runtime.js tmp/$< > $@
	@rm -rf tmp

.PHONY: deps run es5 test clean wc
