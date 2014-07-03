OBJECTS = es5/worker.js es5/main.js

all :: es5 run

run ::
	@node extra/static-here.js

es5 :: $(OBJECTS)
	-@mkdir -p es5/lib
	@cp -Rf index.html styles.css es5/
	@cp -Rf lib/angular.js es5/lib/

test :: es5
	@node extra/test.js

wc :: es5
	@grep -v '//\|^$$' *.js *.html *css | wc -l

clean ::
	-@rm -rf es5

es5/% :: %
	traceur --source-maps --validate --experimental --out tmp --script $<
	echo "// Generated File - Do Not Edit" | cat - lib/traceur-runtime.js tmp/$< > $@
	@rm -rf tmp

.PHONY: run es5 test clean wc
