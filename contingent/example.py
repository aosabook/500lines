"""Rough experiment from which to derive the design of `contingent`."""

from operator import attrgetter
from pprint import pprint
from watchlib import Watcher

w = Watcher()

class Blog(object):
    def __init__(self):
        self.posts = []

    def sorted_posts(self):
        return sorted(self.posts, key=attrgetter('date'))

class Post(object):
    def __init__(self, blog, title, date, content):
        blog.posts.append(self)
        self.blog = blog
        self.title = title
        self.date = date
        self.content = content

    def __setattr__(self, name, value):
        self.__dict__[name] = value

    def __getattr__(self, name):
        return self.__dict[name]

    def prev_post(self):
        sorted_posts = self.blog.sorted_posts()
        i = sorted_posts.index(self)
        return sorted_posts[i - 1] if i else None

    def next_post(self):
        sorted_posts = self.blog.sorted_posts()
        i = sorted_posts.index(self)
        return sorted_posts[i + 1] if i < len(sorted_posts) - 1 else None

def main():
    blog = Blog()

    Post(blog, 'A', '2014-01-01', 'Happy new year.')
    Post(blog, 'B', '2014-01-15', 'Middle of January.')
    Post(blog, 'C', '2014-02-01', 'Beginning of February.')

    for post in blog.sorted_posts():
        print '-' * 8
        print render(post)

    print '=' * 72

    #pprint(w.edges)
    print len(w.edges)

    print '=' * 72

    Post(blog, 'New', '2014-01-25', 'Late January.')

    for post in blog.sorted_posts():
        print '-' * 8
        print render(post)

def render(post):
    pp = post.prev_post()
    np = post.next_post()

    kw = dict(post.__dict__)
    kw['prev_title'] = pp.title if (pp is not None) else '-'
    kw['next_title'] = np.title if (np is not None) else '-'
    return template.format(**kw)

template = """\
Previous story: {prev_title}
Next story: {next_title}

"{title}"
{date}
{content}
"""

if __name__ == '__main__':
    main()
