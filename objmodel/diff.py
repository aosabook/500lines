import os

paths = sorted(p for p in os.listdir(".") if p.startswith("0"))
for i, d1 in enumerate(paths[:-1]):
    d2 = paths[i + 1]
    for fn in ["objmodel.py", "test_objmodel.py"]:
        fn1 = os.path.join(d1, fn)
        fn2 = os.path.join(d2, fn)
        os.system("diff -u %s %s | less" % (fn1, fn2))
