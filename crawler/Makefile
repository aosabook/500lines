PYTHON3=python3.4
test:
	$(PYTHON3) crawl.py -q what-if.xkcd.com

verbose:
	$(PYTHON3) crawl.py what-if.xkcd.com

xkcd:
	$(PYTHON3) crawl.py -q xkcd.com

dropbox:
	$(PYTHON3) crawl.py -q dropbox.com

pep8:
	pep8 *.py

wc:
	grep -v '^ *\(#.*\)\?$$' crawling.py | wc -l
