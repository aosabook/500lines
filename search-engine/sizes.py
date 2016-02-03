#!/usr/bin/python

def xcumsum(xs):
    total = 0
    for xx in xs:
        yield total
        total += xx

def describe(sizes):
    print ' ' * 4 + ' '.join("%4d" % size for size in sizes)
    print ' ' * 4 + ' '.join("%4d" % tots for tots in xcumsum(sizes))

sizes = []
while True:
    line = raw_input("+ ")

    if line == '':
        sizes = []
    else:
        sizes.append(int(line))
        sizes.sort()

    describe(sizes)

    merge = [cumsum >= size for cumsum, size in zip(xcumsum(sizes), sizes)]
    if any(merge):
        max_merge = max(ii for ii in range(len(merge)) if merge[ii]) + 1
        sizes[:max_merge] = [sum(sizes[:max_merge])]
        sizes.sort()
        describe(sizes)
