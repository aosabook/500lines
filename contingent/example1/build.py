"""Render a directory of blog posts as HTML."""

import os
import re
from IPython.nbconvert import HTMLExporter
from IPython.nbformat import current as nbformat
from docutils.core import publish_parts
from glob import glob
from jinja2 import DictLoader

dl = DictLoader({'full.tpl': """\
{%- extends 'display_priority.tpl' -%}
{% block input scoped %}<pre>{{ cell.input }}</pre>
{% endblock %}
{% block pyout scoped %}<pre>{{ output.text | ansi2html }}</pre>
{% endblock %}
{% block markdowncell scoped %}{{ cell.source  | markdown2html }}
{% endblock %}
"""})

def read_text_file(call, path):
    with open(path) as f:
        return f.read()

def parse(call, path):
    source = call(read_text_file, path)
    if path.endswith('.rst'):
        parts = publish_parts(source, writer_name='html')
        return {'body': parts['body'],
                'title': parts['title']}
    elif path.endswith('.ipynb'):
        notebook = nbformat.reads_json(source)
        exporter = HTMLExporter(config=None, extra_loaders=[dl])
        body, resources = exporter.from_notebook_node(notebook)
        # print body
        # print list(resources)
        # print resources['metadata']
        #print resources
        return {'body': body,
                'title': 'title'}

def title_of(call, path):
    info = call(parse, path)
    return info['title']

def render(call, path):
    dirname = os.path.dirname(path)
    info = call(parse, path)
    body = info['body']
    body = re.sub(r'title_of\(([^)]+)\)',
                  (lambda m: title_of(call, os.path.join(dirname, m.group(1)))),
                  body)
    print 'Title:', call(title_of, path)
    print
    print body

def main():
    thisdir = os.path.dirname(__file__)
    indir = os.path.normpath(os.path.join(thisdir, '..', 'posts'))
    outdir = os.path.normpath(os.path.join(thisdir, '..', 'output'))
    if not os.path.exists(outdir):
        os.mkdir(outdir)

    def call(f, *args):
        return f(call, *args)

    for path in (glob(os.path.join(indir, '*.rst'))
               + glob(os.path.join(indir, '*.ipynb'))):
        print '-' * 72
        render(call, path)

if __name__ == '__main__':
    main()
