all : commands

## commands : show all commands
commands :
	@grep -E '^##' Makefile | sed -e 's/## //g'

## check    : build locally into _site directory for checking
check :
	jekyll -t build -d _site

## clean    : clean up
clean :
	rm -rf _site $$(find . -name '*~' -print)
