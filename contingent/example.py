"""Rough experiment from which to derive the design of `contingent`."""

from operator import attrgetter
from pprint import pprint
from watchlib import Graph, Thing

class Blog(Thing):
    def __init__(self, posts):
        self.posts = posts

    @property
    def sorted_posts(self):
        return sorted(self.posts, key=attrgetter('date'))

class Post(Thing):
    def __init__(self, blog, title, date, content):
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
    graph = Graph()

    posts = []
    blog = Blog(posts)
    graph.add(blog)

    post1 = Post(blog, 'A', '2014-01-01', 'Happy new year.')
    post2 = Post(blog, 'B', '2014-01-15', 'Middle of January.')
    post3 = Post(blog, 'C', '2014-02-01', 'Beginning of February.')

    posts.extend([post1, post2, post3])

    for post in posts:
        graph.add(post)

    for post in sorted(posts):
        print '-' * 8
        print post.render()

    print '=' * 72

    def present(value):
        obj = value[0]
        thing = value[1]
        s = '%s%s.%s' % (obj.__class__.__name__, id(obj), thing)
        if len(value) > 2:
            args = value[2]
            s += repr(args)
        return s

    # python example.py && dot -Tpng graph.dot > graph.png && geeqie graph.png

    with open('graph.dot', 'w') as f:
        print >>f, 'digraph { graph [rankdir="LR"];'
        for k, vset in graph.consequences.items():
            for v in vset:
                print >>f, '"%s" -> "%s";' % (present(k), present(v))
        print >>f, '}'

    # Predict what a slight change of date will do.  Avoid avalanches.

    # This would be fun:
    #
    # with graph.aftermath():
    #     post2.date = '2014-01-15'

    post2.date = '2014-01-15'

    key = (post2, 'date')
    graph.run_consequences_of(key)

    print '-' * 8

    post2.date = '2014-02-15'

    key = (post2, 'date')
    graph.run_consequences_of(key)

    return

    print '=' * 72

    post4 = Post(blog, 'New', '2014-01-25', 'Late January.')
    posts[2:2] = [post4]
    graph.add(post4)

    for post in blog.sorted_posts:
        print '-' * 8
        print post.render()

template = """\
Previous story: {prev_title}
Next story: {next_title}

"{title}"
{date}
{content}
"""

if __name__ == '__main__':
    main()
