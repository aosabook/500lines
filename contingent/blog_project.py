"""Rough experiment from which to derive the design of `contingent`."""

import re
from builderlib import Builder
from datetime import datetime
from docutils.core import publish_parts
from functools import wraps
from glob import glob
from inspect import ismethod
from operator import attrgetter
from utils import pygmentize_pre_blocks

class Base:
    def __getattribute__(self, name):
        if name.startswith('_'):
            return object.__getattribute__(self, name)
        target = (self, name)
        return self._builder.get(target)

    def __setattr__(self, name, value):
        if not name.startswith('_'):
            target = (self, name)
            self._builder.set(target, value)
        return object.__setattr__(self, name, value)


def compute(target, get):
    if isinstance(target[1], str):
        obj, attribute_name = target
        value = object.__getattribute__(obj, attribute_name)
        if not ismethod(value):
            return value
        method = value
        @wraps(method)
        def wrapper(*args):
            target = (method, args)
            return get(target)
        return wrapper
    else:
        method, args = target
        return method(*args)


class Blog(Base):
    def __init__(self, builder):
        self._builder = builder
        self.posts = []

    def __repr__(self):
        return '<Blog>'

    @property
    def sorted_posts(self):
        print('Re-sorting posts')
        return sorted(self.posts, key=attrgetter('date'))


class Post(Base):
    def __init__(self, builder, source_path, output_path):
        self._builder = builder
        self.source_path = source_path
        self.output_path = output_path
        self._load()

    def __repr__(self):
        return '<Post {!r}>'.format(self.source_path.split('/')[-1])

    def _load(self):
        with open(self.source_path) as f:
            self.source = f.read()

    def render(self):
        print('Rendering', self)
        rst = self.source

        field_strings = re.findall(r'\n:([^:]+): +(.*)', rst)
        fields = {name.lower(): value for name, value in field_strings}

        if 'date' in fields:
            self.date = datetime.strptime(fields['date'], '%d %B %Y').date()

        if 'tags' in fields:
            self.tags = ['-'.join(tag.strip().lower().split())
                         for tag in fields['tags'].split(',')]

        # Generate HTML from the reStructuredText.

        p = publish_parts(rst, writer_name='html',
                          settings_overrides={'initial_header_level': 2})
        body = p['html_body']  # or 'body'?
        self.title = p['title']

        # My older blog posts included Blogofile metadata.

        # if is_legacy_blogofile(rst):
        #     parse_blogofile_yaml(info)

        body = pygmentize_pre_blocks(body)

        print('    Writing', self.output_path)
        with open(self.output_path, 'w') as f:
            f.write(body)


def main():
    builder = Builder(compute)
    source_directory = '/home/brandon/blog/texts/brandon/2013'
    output_directory = '/home/brandon/blog/output/brandon/2013'

    sources = glob(source_directory + '/*.rst')
    print(sources)
    posts = []
    for source_path in sources:
        output_path = (source_path.replace(source_directory, output_directory)
                       .replace('.rst', '/index.html'))
        post = Post(builder, source_path, output_path)
        post.render()
        posts.append(post)

    import time
    while True:
        time.sleep(1.0)
        print('-')
        for post in posts:
            post._load()
        builder.rebuild()


if __name__ == '__main__':
    main()
