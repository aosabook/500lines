import os
import difflib

currdir = os.path.dirname(os.path.abspath(__file__))

dirs = [p for p in os.listdir(currdir) if os.path.isdir(p) and "0" in p]
dirs.sort()

total = 0

print dirs[0]
prev = {}
for fn in sorted(os.listdir(dirs[0])):
    fulln = os.path.join(dirs[0], fn)
    if not fn.endswith(".py") or not os.path.isfile(fulln):
        continue
    with file(fulln) as f:
        lines = f.readlines()
    print len(lines), fn
    total += len(lines)
    prev[fn] = lines

print
for d in dirs[1:]:
    print d
    for fn, prevlines in sorted(prev.items()):
        fulln = os.path.join(d, fn)
        with file(fulln) as f:
            lines = f.readlines()
        diffsize = len(list(difflib.unified_diff(prevlines, lines, n=1)))
        print diffsize, fn
        #print "".join(difflib.unified_diff(prevlines, lines))
        prev[fn] = lines
        total += diffsize
    print

print "------------"
print total, "total"
