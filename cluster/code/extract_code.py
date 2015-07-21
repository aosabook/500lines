import sys
import re
import shlex
import textwrap

extractors = {}

class LineIterator(object):

    used_lines_per_file = {}

    def __init__(self, filename):
        self.enum_iter = enumerate(open(filename))
        self.linenum = -1
        self.used_lines = self.used_lines_per_file.setdefault(filename, set())

    def __iter__(self):
        return self

    def next(self):
        self.linenum, rv = self.enum_iter.next()
        return rv

    def use_line(self):
        self.used_lines.add(self.linenum)

    @classmethod
    def print_unused_lines(cls, filename):
        sys.stderr.write("*** unused lines in %r ***\n" % filename)
        used_lines = cls.used_lines_per_file[filename]
        for l, line in enumerate(open(filename)):
            if line.strip() and l not in used_lines:
                sys.stderr.write("%4d %s" % (l+1, line))

file_lines_used = {}

def code_block(filename, args):
    assert args, "args is empty"
    lines = LineIterator(filename)

    result = []

    for arg in args:
        beginning_re = re.compile(' *' + arg)
        for line in lines:
            if beginning_re.match(line):
                result.append(line)
                lines.use_line()
                break
        else:
            print >>sys.stderr, "no match found for %r" % arg
            sys.exit(1)

    space = re.match('^ *', line).group(0)
    space_re = re.compile('^%s ' % space)
    whitespace = False
    for line in lines:
        if not line.strip():
            whitespace = True
            continue
        if not space_re.match(line):
            break
        if whitespace:
            result.append('\n')
            whitespace = False
        result.append(line)
        lines.use_line()

    # left-justify with a four-space leader
    return ['    ' + line + '\n'
            for line in textwrap.dedent(''.join(result)).split('\n')]
extractors['code_block'] = code_block

def from_to(filename, args):
    from_re, to_re = map(re.compile, args)
    lines = LineIterator(filename)

    result = []
    for line in lines:
        if from_re.match(line):
            break
    else:
        print >>sys.stderr, "no match found for %r" % from_re
        sys.exit(1)

    for line in lines:
        if to_re.match(line):
            break
        result.append(line)
        lines.use_line()

    # left-justify with a four-space leader
    return ['    ' + line + '\n'
            for line in textwrap.dedent(''.join(result)).split('\n')]
extractors['from_to'] = from_to

def scan_chapter():
    lines = open('CHAPTER.rst')
    result = []
    in_braces = False
    for line in lines:
        if in_braces:
            if not line.startswith('}}}'):
                continue
            else:
                in_braces = False
        if not in_braces:
            result.append(line)
            if line.startswith('{{{'):
                result.extend(['.. code-block:: python\n', '\n'])
                in_braces = True
                args = shlex.split(line[4:])
                extractor = extractors[args[0]]
                filename = args[1]
                result.extend(extractor(filename, args[2:]))
    open("CHAPTER.rst", "w").write(''.join(result))

scan_chapter()
LineIterator.print_unused_lines('cluster.py')
