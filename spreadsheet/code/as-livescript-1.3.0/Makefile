all ::
	lsc -c *ls

run ::
	@node ../node_modules/static-here/bin/static-here

wc ::
	@echo -n Lines:
	@grep -v '# \|^$$' *.ls *.html ../*css | wc -l
