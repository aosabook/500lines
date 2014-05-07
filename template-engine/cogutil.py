import textwrap

import cog

LAST_LINES = None
LAST_FILENAME = None

def include(filename, first=None, after=None, numlines=None, numblanks=None, dedent=True):
    """
    Include text from a file.
    """
    global LAST_LINES, LAST_FILENAME
    if filename != LAST_FILENAME:
        with open(filename) as f:
            lines = LAST_LINES = f.readlines()
        LAST_FILENAME = filename
    else:
        lines = LAST_LINES

    including = "".join(selected_lines(lines, first, after, numlines, numblanks))
    if dedent:
        including = textwrap.dedent(including)

    cog.outl("```")
    cog.out(including)
    cog.outl("```")


def selected_lines(lines, first=None, after=None, numlines=None, numblanks=None):
    ready = after is None
    including = False
    for line in lines:
        if not ready:
            if after in line:
                ready = True
        if ready and first is not None and first in line:
            including = True
        if including:
            if numblanks is not None and not line.strip():
                numblanks -= 1
                if not numblanks:
                    break
            yield line
            if numlines is not None:
                numlines -= 1
                if not numlines:
                    break
