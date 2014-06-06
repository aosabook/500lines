import fleet

def test_lines():
    file = fleet.__file__.replace('.pyc', '.py')
    lines = len(list(open(file)))
    assert lines <= 500, "%r is %d lines" % (file, lines)
