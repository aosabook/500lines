"""Rough experiment from which to derive the design of `contingent`."""

from operator import itemgetter
from watchlib import Watcher

w = Watcher()

post1 = {
    'title': 'A',
    'date': '2014-01-01',
    'content': 'Happy new year.',
    }

post2 = {
    'title': 'B',
    'date': '2014-01-15',
    'content': 'Middle of January.',
    }

post3 = {
    'title': 'C',
    'date': '2014-02-01',
    'content': 'Beginning of February.',
    }

new_post = {
    'title': 'New',
    'date': '2014-01-25',
    'content': 'Late January.',
    }

def main():
    all_posts = [post1, post2, post3]
    for post in sorted(all_posts, key=itemgetter('date')):
        print '-' * 8
        print render(post, all_posts)

    print '=' * 72

    all_posts.append(new_post)
    for post in sorted(all_posts, key=itemgetter('date')):
        print '-' * 8
        print render(post, all_posts)

@w.watch
def sorted_posts(all_posts):
    return sorted(all_posts, key=itemgetter('date'))

@w.watch
def prev_post(post, all_posts):
    s = sorted_posts(all_posts)
    i = s.index(post)
    return s[i-1] if i > 0 else None

@w.watch
def next_post(post, all_posts):
    s = sorted_posts(all_posts)
    i = s.index(post)
    return s[i+1] if i < len(s) - 1 else None

@w.watch
def render(post, all_posts):
    pp = prev_post(post, all_posts)
    np = next_post(post, all_posts)

    kw = dict(post)
    kw['prev_title'] = pp['title'] if (pp is not None) else '-'
    kw['next_title'] = np['title'] if (np is not None) else '-'
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
