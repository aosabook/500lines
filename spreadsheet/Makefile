OBJECTS = es5/worker.js es5/main.js

all :: deps es5 wc run

deps ::
	@npm i

run ::
	@node_modules/static-here/bin/static-here

es5 :: $(OBJECTS)
	-@mkdir -p es5/lib
	@cp -Rf index.html styles.css es5/
	@cp -Rf lib/angular.js es5/lib/

test :: es5
	@node extra/test.js

wc ::
	@echo -n Lines:
	@grep -v '//\|^$$' *.js *.html *css | wc -l

clean ::
	-@rm -rf es5/*

es5/% :: %
	./node_modules/traceur/traceur --source-maps --validate --experimental --out tmp --script $<
	echo "// Generated File - Do Not Edit" | cat - ./node_modules/traceur/bin/traceur-runtime.js tmp/$< > $@
	@rm -rf tmp

.PHONY: deps run es5 test clean wc
