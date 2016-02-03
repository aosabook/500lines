all: handaxeweb.lua build_handaxeweb index.py skiplist.svg skipfiles.svg

# The dependencies of these two targets are a little bit bogus,
# because actually handaxeweb.lua has a circular dependency on itself
# and is also mutually dependent with build_handaxeweb, which also has
# a circular dependency on itself.  But really they are both rebuilt
# (bootstrapped using themselves!) from handaxeweb.md.  Just don't
# delete them as if they were normal derived objects, and occasionally
# touch handaxeweb.md and do a second rebuild if you're taking
# advantage of new handaxeweb features in build_handaxeweb, and you'll
# be fine.

handaxeweb.lua: handaxeweb.md
	./build_handaxeweb

build_handaxeweb: handaxeweb.md
	./$@

.PRECIOUS: handaxeweb.lua build_handaxeweb

index.py: README.md handaxeweb.lua
	./handaxeweb.lua $@ < $< > $@

skiplist.svg: skipdiagram.py
	./$< skiplist > $@

skipfiles.svg: skipdiagram.py
	./$< skipfiles > $@
