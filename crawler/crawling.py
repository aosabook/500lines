"""A simple web crawler -- classes implementing crawling logic."""

import asyncio
import cgi
import logging
import re
import time
import urllib.parse

import aiohttp  # Install with "pip install aiohttp".

logger = logging.getLogger(__name__)


def unescape(s):
    """The inverse of cgi.escape()."""
    s = s.replace('&quot;', '"').replace('&gt;', '>').replace('&lt;', '<')
    return s.replace('&amp;', '&')  # Must be last.


class Fetcher:
    """Logic and state for one URL.

    When found in crawler.busy, this represents a URL to be fetched or
    in the process of being fetched; when found in crawler.done, this
    holds the results from fetching it.

    This is usually associated with a task.  This references the
    crawler for the TCP connector and to add more URLs to its todo
    list.

    Call fetch() to do the fetching; results are in instance variables.
    """

    def __init__(self, url, crawler, max_redirect=10, max_tries=4):
        self.url = url
        self.crawler = crawler
        # We don't loop resolving redirects here -- we just use this
        # to decide whether to add the redirect URL to crawler.todo.
        self.max_redirect = max_redirect
        # But we do loop to retry on errors a few times.
        self.max_tries = max_tries
        # Everything we collect from the response goes here.
        self.task = None
        self.exceptions = []
        self.tries = 0
        self.status = None
        self.body = None
        self.next_url = None
        self.ctype = None
        self.pdict = None
        self.encoding = None
        self.urls = None
        self.new_urls = None

    @asyncio.coroutine
    def fetch(self):
        """Attempt to fetch the contents of the URL.

        If successful, and the data is HTML, extract further links and
        add them to the crawler.  Redirects are also added back there.
        """
        while self.tries < self.max_tries:
            self.tries += 1
            try:
                response = yield from aiohttp.request(
                    'get',
                    self.url,
                    connector=self.crawler.connector,
                    allow_redirects=False
                    )

                if self.tries > 1:
                    logger.info('try %r for %r success', self.tries, self.url)
                break

            except aiohttp.ClientError as exc:
                self.exceptions.append(exc)
                logger.info('try %r for %r raised %r',
                            self.tries, self.url, exc)
        else:
            # We never broke out of the while loop, i.e. all tries failed.
            logger.error('no success for %r in %r tries',
                         self.url, self.max_tries)
            return

        if response.status in (300, 301, 302, 303, 307) and response.headers.get('location'):
            next_url = response.headers['location']
            self.next_url = urllib.parse.urljoin(self.url, next_url)
            if self.max_redirect > 0:
                logger.info('redirect to %r from %r', self.next_url, self.url)
                self.crawler.add_url(self.next_url, self.max_redirect-1)
            else:
                logger.error('redirect limit reached for %r from %r',
                             self.next_url, self.url)
        else:
            self.status = response.status
            if self.status == 200:
                self.ctype = response.headers.get('content-type')
                self.pdict = {}

                self.body = yield from response.read()

                if self.ctype:
                    self.ctype, self.pdict = cgi.parse_header(self.ctype)

                self.encoding = self.pdict.get('charset')
                if self.ctype in ('text/html', 'application/xml'):
                    text = yield from response.text()

                    # Replace href with (?:href|src) to follow image links.
                    self.urls = set(re.findall(r'(?i)href=["\']?([^\s"\'<>]+)',
                                               text))
                    if self.urls:
                        logger.info('got %r distinct urls from %r',
                                    len(self.urls), self.url)
                    self.new_urls = set()
                    for url in self.urls:
                        url = unescape(url)
                        url = urllib.parse.urljoin(self.url, url)
                        url, frag = urllib.parse.urldefrag(url)
                        if self.crawler.add_url(url):
                            self.new_urls.add(url)


class Crawler:
    """Crawl a set of URLs.

    This manages three disjoint sets of URLs (todo, busy, done).  The
    data structures actually store dicts -- the values in todo give
    the redirect limit, while the values in busy and done are Fetcher
    instances.
    """
    def __init__(self, roots,
                 exclude=None, strict=True,  # What to crawl.
                 max_redirect=10, max_tries=4,  # Per-url limits.
                 max_tasks=10,  # Global limits.
                 ):
        self.roots = roots
        self.exclude = exclude
        self.strict = strict
        self.max_redirect = max_redirect
        self.max_tries = max_tries
        self.max_tasks = max_tasks
        self.todo = {}
        self.busy = {}
        self.done = {}
        self.connector = aiohttp.TCPConnector()
        self.root_domains = set()
        for root in roots:
            parts = urllib.parse.urlparse(root)
            host, port = urllib.parse.splitport(parts.netloc)
            if not host:
                continue
            if re.match(r'\A[\d\.]*\Z', host):
                self.root_domains.add(host)
            else:
                host = host.lower()
                if self.strict:
                    self.root_domains.add(host)
                else:
                    host = host.split('.')[-2:]
                    self.root_domains.add(host)
        for root in roots:
            self.add_url(root)
        self.governor = asyncio.Semaphore(max_tasks)
        self.termination = asyncio.Condition()
        self.t0 = time.time()
        self.t1 = None

    def close(self):
        """Close resources."""
        self.connector.close()

    def host_okay(self, host):
        """Check if a host should be crawled.

        A literal match (after lowercasing) is always good.  For hosts
        that don't look like IP addresses, some approximate matches
        are okay depending on the strict flag.
        """
        host = host.lower()
        if host in self.root_domains:
            return True
        if re.match(r'\A[\d\.]*\Z', host):
            return False
        if self.strict:
            return self._host_okay_strictish(host)
        else:
            return self._host_okay_lenient(host)

    def _host_okay_strictish(self, host):
        """Check if a host should be crawled, strict-ish version.

        This checks for equality modulo an initial 'www.' component.
        """
        host = host[4:] if host.startswith('www.') else 'www.' + host
        return host in self.root_domains

    def _host_okay_lenient(self, host):
        """Check if a host should be crawled, lenient version.

        This compares the last two components of the host.
        """
        host = host.split('.')[-2:]
        return host in self.root_domains

    def add_url(self, url, max_redirect=None):
        """Add a URL to the todo list if not seen before."""
        if self.exclude and re.search(self.exclude, url):
            return False
        parts = urllib.parse.urlparse(url)
        if parts.scheme not in ('http', 'https'):
            logger.debug('skipping non-http scheme in %r', url)
            return False
        host, port = urllib.parse.splitport(parts.netloc)
        if not self.host_okay(host):
            logger.debug('skipping non-root host in %r', url)
            return False
        if max_redirect is None:
            max_redirect = self.max_redirect
        if url in self.todo or url in self.busy or url in self.done:
            return False
        logger.debug('adding %r %r', url, max_redirect)
        self.todo[url] = max_redirect
        return True

    @asyncio.coroutine
    def crawl(self):
        """Run the crawler until all finished."""
        with (yield from self.termination):
            while self.todo or self.busy:
                if self.todo:
                    url, max_redirect = self.todo.popitem()
                    fetcher = Fetcher(url,
                                      crawler=self,
                                      max_redirect=max_redirect,
                                      max_tries=self.max_tries,
                                      )
                    self.busy[url] = fetcher
                    fetcher.task = asyncio.Task(self.fetch(fetcher))
                else:
                    yield from self.termination.wait()
        self.t1 = time.time()

    @asyncio.coroutine
    def fetch(self, fetcher):
        """Call the Fetcher's fetch(), with a limit on concurrency.

        Once this returns, move the fetcher from busy to done.
        """
        url = fetcher.url
        with (yield from self.governor):
            try:
                yield from fetcher.fetch()  # Fetcher gonna fetch.
            finally:
                # Force GC of the task, so the error is logged.
                fetcher.task = None
        with (yield from self.termination):
            self.done[url] = fetcher
            del self.busy[url]
            self.termination.notify()
