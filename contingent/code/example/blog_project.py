"""Render a directory of cross-referenced blog posts as HTML."""

import os
import re
from IPython.nbconvert import HTMLExporter
from IPython.nbformat import current as nbformat
from docutils.core import publish_doctree, publish_parts
from docutils import nodes
from glob import glob
from jinja2 import DictLoader

from contingent.projectlib import Project
from contingent.io import looping_wait_on

dl = DictLoader({'full.tpl': """\
{%- extends 'display_priority.tpl' -%}
{% block input scoped %}<pre>{{ cell.input }}</pre>
{% endblock %}
{% block pyout scoped %}<pre>{{ output.text | ansi2html }}</pre>
{% endblock %}
{% block markdowncell scoped %}{{ cell.source  | markdown2html }}
{% endblock %}
"""})

project = Project()
task = project.task

@task
def read_text_file(path):
    with open(path) as f:
        return f.read()

@task
def parse(path):
    source = read_text_file(path)
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

@task
def title_of(path):
    info = parse(path)
    return info['title']

@task
def date_of(path):
    info = parse(path)
    return info['date']

@task
def body_of(path):
    info = parse(path)
    dirname = os.path.dirname(path)
    body = info['body']
    def format_title_reference(match):
        filename = match.group(1)
        title = title_of(os.path.join(dirname, filename))
        return '<i>{}</i>'.format(title)
    body = re.sub(r'title_of\(([^)]+)\)', format_title_reference, body)
    return body

@task
def sorted_posts(paths):
    return sorted(paths, key=date_of)

@task
def previous_post(paths, path):
    paths = sorted_posts(paths)
    i = paths.index(path)
    return paths[i - 1] if i else None

@task
def render(paths, path):
    previous = previous_post(paths, path)
    previous_title = 'NONE' if previous is None else title_of(previous)
    text = '<h1>{}</h1>\n<p>Date: {}</p>\n<p>Previous post: {}</p>\n{}'.format(
        title_of(path), date_of(path),
        previous_title, body_of(path))
    print('-' * 72)
    print(text)
    return text

def main():
    thisdir = os.path.dirname(__file__)
    indir = os.path.normpath(os.path.join(thisdir, '..', 'posts'))
    outdir = os.path.normpath(os.path.join(thisdir, '..', 'output'))
    if not os.path.exists(outdir):
        os.mkdir(outdir)

    paths = tuple(glob(os.path.join(indir, '*.rst')) +
                  glob(os.path.join(indir, '*.ipynb')))

    for path in sorted_posts(paths):
        render(paths, path)

    project.verbose = True
    while True:
        print('=' * 72)
        print('Watching for files to change')
        changed_paths = looping_wait_on(paths)
        print('=' * 72)
        print('Reloading:', ' '.join(changed_paths))
        with project.cache_off():
            for path in changed_paths:
                read_text_file(path)
        project.rebuild()

if __name__ == '__main__':
    main()
