wc ::
	@echo -n Lines:
	@grep -v '//\|^$$' *.js *.html ../*css | wc -l
all ::
	lsc -c *ls

run ::
	@node ../node_modules/static-here/bin/static-here
