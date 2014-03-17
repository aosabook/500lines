"""Rough experiment from which to derive the design of `contingent`."""

from builderlib import Builder
from functools import wraps
from inspect import ismethod
from operator import attrgetter


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
    def __init__(self, builder, blog, title, date):
        self._builder = builder
        self.blog = blog
        self.title = title
        self.date = date

    def __repr__(self):
        return '<Post {!r}>'.format(self.title)

    def prev(self):
        sorted_posts = self.blog.sorted_posts
        i = sorted_posts.index(self)
        return sorted_posts[i - 1] if i else None

    def next(self):
        sorted_posts = self.blog.sorted_posts
        i = sorted_posts.index(self)
        return sorted_posts[i + 1] if i + 1 < len(sorted_posts) else None

    def render(self):
        print('Rendering', self)
        prev = self.prev()
        next = self.next()
        prev_title = prev.title if prev else '-'
        next_title = next.title if next else '-'
        return template.format(post=self, prev=prev_title, next=next_title)


def main():
    builder = Builder(compute)

    blog = Blog(builder)
    postA = Post(builder, blog, 'A', '2014-01-01')
    postB = Post(builder, blog, 'B', '2014-02-02')
    postC = Post(builder, blog, 'C', '2014-03-03')
    postD = Post(builder, blog, 'D', '2014-04-04')
    postE = Post(builder, blog, 'E', '2014-05-05')
    blog.posts = [postA, postB, postC, postD, postE]

    display_posts(blog)

    with open('diagram-example.dot', 'w') as f:
        f.write(builder.graph.as_graphviz())

    print('Making slight change to C date'.center(72, '='))

    with builder.consequences():
        postC.date = '2014-03-15'

    display_posts(blog)

    print('Moving C date to after D date'.center(72, '='))

    with builder.consequences():
        postC.date = '2014-04-15'

    display_posts(blog)

    print('Adding "New" post right before E'.center(72, '='))

    postF = Post(builder, blog, 'New', '2014-04-30')
    with builder.consequences():
        blog.posts = blog.posts + [postF]

    display_posts(blog)

    print('=' * 60)


def display_posts(blog):
    for post in blog.sorted_posts:
        print(post.render())


template = """\
"{post.title}" on {post.date} [Previous: {prev} Next: {next}]"""

if __name__ == '__main__':
    main()
