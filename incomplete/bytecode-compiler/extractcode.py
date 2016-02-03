"""
Take Markdown and extract the code sections.
"""

import sys

outputs = {}

eg = sys.stdin.read()
paragraph_break = True
label = 'default'
lines = iter(eg.splitlines())
for line in lines:
    assert '\t' not in line
    if paragraph_break and line.startswith('    '):
        if line.strip().startswith('!!'):
            label = line.strip().strip('!')
            line = next(lines)
        while line.startswith('    '):
            outputs.setdefault(label, []).append(line[4:])
            line = next(lines)
        outputs.setdefault(label, []).append('')
    paragraph_break = (line.strip() == '')
    if not paragraph_break:
        label = 'default'

parts = 'top.py assembler.py default'.split()
with open('compiler.py', 'w') as f:
    f.write('\n'.join(sum(map(outputs.get, parts), [])))
for label in parts:
    del outputs[label]

with open('non-compiler', 'w') as f:
    for label, lines in outputs.items():
        for line in lines:
            f.write(label+': '+line+'\n')
