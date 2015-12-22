all ::
	lsc -c worker.ls
	@webpack -h

run ::
	lsc -c worker.ls
	@node ../node_modules/webpack-dev-server/bin/webpack-dev-server.js -h

wc ::
	@echo -n Lines:
	@grep -v '# \|^$$' *.ls *.html ../*css | wc -l
