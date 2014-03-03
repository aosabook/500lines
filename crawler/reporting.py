"""Reporting subsystem for web crawler."""

import time


class Stats:
    """Record stats of various sorts."""

    def __init__(self):
        self.stats = {}

    def add(self, key, count=1):
        self.stats[key] = self.stats.get(key, 0) + count

    def report(self, file=None):
        for key, count in sorted(self.stats.items()):
            print('%10d' % count, key, file=file)


def report(crawler, file=None):
    """Print a report on all completed URLs."""
    t1 = crawler.t1 or time.time()
    dt = t1 - crawler.t0
    if dt and crawler.max_tasks:
        speed = len(crawler.done) / dt / crawler.max_tasks
    else:
        speed = 0
    stats = Stats()
    print('*** Report ***', file=file)
    try:
        show = []
        show.extend(crawler.done.items())
        show.extend(crawler.busy.items())
        show.sort()
        for url, fetcher in show:
            fetcher_report(fetcher, stats, file=file)
    except KeyboardInterrupt:
        print('\nInterrupted', file=file)
    print('Finished', len(crawler.done),
          'urls in %.3f secs' % dt,
          '(max_tasks=%d)' % crawler.max_tasks,
          '(%.3f urls/sec/task)' % speed,
          file=file)
    stats.report(file=file)
    print('Todo:', len(crawler.todo), file=file)
    print('Busy:', len(crawler.busy), file=file)
    print('Done:', len(crawler.done), file=file)
    print('Date:', time.ctime(), 'local time', file=file)


def fetcher_report(fetcher, stats, file=None):
    """Print a report on the state for this URL.

    Also update the Stats instance.
    """
    if fetcher.task is not None:
        if not fetcher.task.done():
            stats.add('pending')
            print(fetcher.url, 'pending', file=file)
            return
        elif fetcher.task.cancelled():
            stats.add('cancelled')
            print(fetcher.url, 'cancelled', file=file)
            return
        elif fetcher.task.exception():
            stats.add('exception')
            exc = fetcher.task.exception()
            stats.add('exception_' + exc.__class__.__name__)
            print(fetcher.url, exc, file=file)
            return
    if len(fetcher.exceptions) == fetcher.tries:
        stats.add('fail')
        exc = fetcher.exceptions[-1]
        stats.add('fail_' + str(exc.__class__.__name__))
        print(fetcher.url, 'error', exc, file=file)
    elif fetcher.next_url:
        stats.add('redirect')
        print(fetcher.url, fetcher.response.status, 'redirect', fetcher.next_url,
              file=file)
    elif fetcher.ctype == 'text/html':
        stats.add('html')
        size = len(fetcher.body or b'')
        stats.add('html_bytes', size)
        print(fetcher.url, fetcher.response.status,
              fetcher.ctype, fetcher.encoding,
              size,
              '%d/%d' % (len(fetcher.new_urls or ()), len(fetcher.urls or ())),
              file=file)
    elif fetcher.response is None:
        print(fetcher.url, 'no response object')
    else:
        size = len(fetcher.body or b'')
        if fetcher.response.status == 200:
            stats.add('other')
            stats.add('other_bytes', size)
        else:
            stats.add('error')
            stats.add('error_bytes', size)
            stats.add('status_%s' % fetcher.response.status)
        print(fetcher.url, fetcher.response.status,
              fetcher.ctype, fetcher.encoding,
              size,
              file=file)
