



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
    print render(post1)

def render(post):
    kw = dict(post)
    return template.format(**kw)

template = """
{title}
{date}

{content}
"""

if __name__ == '__main__':
    main()
