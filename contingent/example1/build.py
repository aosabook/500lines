"""Render a directory of blog posts as HTML."""

import os
import re
from IPython.nbconvert import HTMLExporter
from IPython.nbformat import current as nbformat
from docutils.core import publish_doctree, publish_parts
from docutils import nodes
from glob import glob
from jinja2 import DictLoader

from contingent.builderlib import Builder
# from contingent.cachelib import Cache, _absent
from contingent.utils import looping_wait_on

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
        doctree = publish_doctree(source)
        docinfos = doctree.traverse(nodes.docinfo)
        docinfo = {c.tagname: str(c.children[0])
                   for i in docinfos for c in i.children}
        parts = publish_parts(source, writer_name='html')
        return {'body': parts['body'],
                'date': docinfo.get('date'),
                'title': parts['title']}
    elif path.endswith('.ipynb'):
        notebook = nbformat.reads_json(source)
        exporter = HTMLExporter(config=None, extra_loaders=[dl])
        body, resources = exporter.from_notebook_node(notebook)
        return {'body': body,
                'date': notebook['metadata']['date'],
                'title': notebook['metadata']['name']}

def title_of(call, path):
    info = call(parse, path)
    return info['title']

def date_of(call, path):
    info = call(parse, path)
    return info['date']

def body_of(call, path):
    info = call(parse, path)
    dirname = os.path.dirname(path)
    body = info['body']
    def format_title_reference(match):
        filename = match.group(1)
        title = title_of(call, os.path.join(dirname, filename))
        return '<i>{}</i>'.format(title)
    body = re.sub(r'title_of\(([^)]+)\)', format_title_reference, body)
    return body

def sorted_posts(call, paths):
    return sorted(paths, key=lambda path: call(date_of, path))

def previous_post(call, paths, path):
    paths = call(sorted_posts, paths)
    i = paths.index(path)
    return paths[i - 1] if i else None

def render(call, paths, path):
    previous = call(previous_post, paths, path)
    previous_title = 'NONE' if previous is None else call(title_of, previous)
    text = '<h1>{}</h1>\n<p>Date: {}</p>\n<p>Previous post: {}</p>\n{}'.format(
        call(title_of, path), call(date_of, path),
        previous_title, call(body_of, path))
    print('-' * 72)
    print(text)
    return text


class BlogBuilder:
    def __init__(self):
        self.builder = Builder(self.compute)
        self.verbose = False

    def get(self, fn, *args):
        return self.builder.get((fn, args))

    def invalidate(self, fn, *args):
        return self.builder.invalidate((fn, args))

    def __getattr__(self, name):
        return getattr(self.builder, name)

    def compute(self, task, _):
        "Compute a task by direct invocation."
        if self.verbose:
            print('Computing', task)

        fn, args = task
        return fn(self.get, *args)


def main():
    thisdir = os.path.dirname(__file__)
    indir = os.path.normpath(os.path.join(thisdir, '..', 'posts'))
    outdir = os.path.normpath(os.path.join(thisdir, '..', 'output'))
    if not os.path.exists(outdir):
        os.mkdir(outdir)

    builder = BlogBuilder()

    paths = tuple(glob(os.path.join(indir, '*.rst')) +
                  glob(os.path.join(indir, '*.ipynb')))

    for path in builder.get(sorted_posts, paths):
        builder.get(render, paths, path)

    builder.verbose = True
    while True:
        print('=' * 72)
        print('Watching for files to change')
        changed_paths = looping_wait_on(paths)
        print('=' * 72)
        print('Reloading:', ' '.join(changed_paths))
        for path in changed_paths:
            builder.invalidate(read_text_file, path)
        builder.rebuild()

if __name__ == '__main__':
    main()
