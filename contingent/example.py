"""Rough experiment from which to derive the design of `contingent`."""

from builderlib import Builder
from functools import wraps
from inspect import ismethod
from operator import attrgetter
from pprint import pprint


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
    def __init__(self, builder, posts):
        self._builder = builder
        self.posts = posts

    @property
    def sorted_posts(self):
        return sorted(self.posts, key=attrgetter('date'))


class Post(Base):
    def __init__(self, builder, blog, title, date, content):
        self._builder = builder
        self.blog = blog
        self.title = title
        self.date = date
        self.content = content

    def prev_post(self):
        sorted_posts = self.blog.sorted_posts
        i = sorted_posts.index(self)
        return sorted_posts[i - 1] if i else None

    def next_post(self):
        sorted_posts = self.blog.sorted_posts
        i = sorted_posts.index(self)
        return sorted_posts[i + 1] if i < len(sorted_posts) - 1 else None

    def render(self):
        pp = self.prev_post()
        np = self.next_post()

        kw = dict(self.__dict__)
        kw['prev_title'] = pp.title if (pp is not None) else '-'
        kw['next_title'] = np.title if (np is not None) else '-'
        return template.format(**kw)


def main():
    builder = Builder(compute)

    posts = []
    blog = Blog(builder, posts)

    post1 = Post(builder, blog, 'A', '2014-01-01', 'Happy new year.')
    post2 = Post(builder, blog, 'B', '2014-01-15', 'Middle of January.')
    post3 = Post(builder, blog, 'C', '2014-02-01', 'Beginning of February.')

    posts.extend([post1, post2, post3])

    display_posts(blog)

    print('=' * 72)

    def present(value):
        obj = value[0]
        thing = value[1]
        s = '%s%s.%s' % (obj.__class__.__name__, id(obj), thing)
        if len(value) > 2:
            args = value[2]
            s += repr(args)
        return s

    # python example.py && dot -Tpng graph.dot > graph.png && geeqie graph.png

    with open('diagram-example.dot', 'w') as f:
        f.write(builder.graph.as_graphviz())

    #

    with builder.consequences():
        post2.date = '2014-01-15'

    with builder.consequences():
        post2.date = '2014-02-15'

    #pprint(builder.cache._results)
    print(builder.cache.todo())

    display_posts(blog)

    return

    key = (post2, 'date')
    graph.run_consequences_of(key)

    print('-' * 8)

    post2.date = '2014-02-15'

    key = (post2, 'date')
    graph.run_consequences_of(key)

    return

    print('=' * 72)

    post4 = Post(blog, 'New', '2014-01-25', 'Late January.')
    posts[2:2] = [post4]
    graph.add(post4)

    for post in blog.sorted_posts:
        print('-' * 8)
        print(post.render())


def display_posts(blog):
    for post in blog.sorted_posts:
        print('-' * 8)
        print(post.render())


template = """\
Previous story: {prev_title}
Next story: {next_title}

"{title}"
{date}
{content}
"""

if __name__ == '__main__':
    main()
