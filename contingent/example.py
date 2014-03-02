"""Rough experiment from which to derive the design of `contingent`."""

from operator import attrgetter
# from pprint import pprint
from watchlib import Graph, Node

class Blog(Node):
    def __init__(self):
        self.posts = []

    @property
    def sorted_posts(self):
        return sorted(self.posts, key=attrgetter('date'))

class Post(Node):
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

    blog = Blog()
    graph.add(blog)

    post1 = Post(blog, 'A', '2014-01-01', 'Happy new year.')
    post2 = Post(blog, 'B', '2014-01-15', 'Middle of January.')
    post3 = Post(blog, 'C', '2014-02-01', 'Beginning of February.')

    blog.posts.extend([post1, post2, post3])

    for post in blog.posts:
        graph.add(post)

    for post in blog.sorted_posts:
        print '-' * 8
        print post.render()

    print '=' * 72

    #pprint(w.edges)
    #print len(w.edges)

    print '=' * 72

    post4 = Post(blog, 'New', '2014-01-25', 'Late January.')
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
