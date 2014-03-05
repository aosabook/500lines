"""A simple web crawler -- classes implementing crawling logic."""

# TODO:
# - More organized logging (with task ID or URL?).
# - KeyboardInterrupt in HTML parsing may hang or report unretrieved error.
# - Support gzip encoding.
# - Close connection if HTTP/1.0 response.
# - Add timeouts.  (E.g. when switching networks, all seems to hang.)
# - Skip reading large non-text/html files?
# - Use ETag and If-Modified-Since?
# - Handle out of file descriptors directly?  (How?)

import asyncio
import cgi
import collections
from http.client import BadStatusLine
import logging
import re
import sys
import time
import urllib.parse

logger = logging.getLogger(__name__)


ESCAPES = [('quot', '"'),
           ('gt', '>'),
           ('lt', '<'),
           ('amp', '&')  # Must be last.
           ]


def unescape(url):
    """Turn &amp; into &, and so on.

    This is the inverse of cgi.escape().
    """
    for name, char in ESCAPES:
        url = url.replace('&' + name + ';', char)
    return url


class ConnectionPool:
    """A connection pool.

    To open a connection, use reserve().  To recycle it, use unreserve().

    The pool is mostly just a mapping from (host, port, ssl) tuples to
    lists of Connections.  The currently active connections are *not*
    in the data structure; get_connection() takes the connection out,
    and recycle_connection() puts it back in.  To recycle a
    connection, call conn.close(recycle=True).

    There are limits to both the overall pool and the per-key pool.
    """

    def __init__(self, max_pool=10, max_tasks=5):
        self.max_pool = max_pool  # Overall limit.
        self.max_tasks = max_tasks  # Per-key limit.
        self.loop = asyncio.get_event_loop()
        self.connections = {}  # {(host, port, ssl): [Connection, ...], ...}
        self.queue = []  # [Connection, ...]

    def close(self):
        """Close all connections available for reuse."""
        for conns in self.connections.values():
            for conn in conns:
                conn.close()
        self.connections.clear()
        self.queue.clear()

    @asyncio.coroutine
    def get_connection(self, host, port, ssl):
        """Create or reuse a connection."""
        port = port or (443 if ssl else 80)
        try:
            ipaddrs = yield from self.loop.getaddrinfo(host, port)
        except Exception as exc:
            logger.error('Exception %r for (%r, %r)', exc, host, port)
            raise
        logger.warn('* %s resolves to %s',
                    host, ', '.join(ip[4][0] for ip in ipaddrs))

        # Look for a reusable connection.
        for _, _, _, _, (h, p, *_) in ipaddrs:
            key = h, p, ssl
            conn = None
            conns = self.connections.get(key)
            while conns:
                conn = conns.pop(0)
                self.queue.remove(conn)
                if not conns:
                    del self.connections[key]
                if conn.stale():
                    logger.warn('closing stale connection %r', key)
                    conn.close()  # Just in case.
                else:
                    logger.warn('* Reusing pooled connection %r', key)
                    return conn

        # Create a new connection.
        conn = Connection(self, host, port, ssl)
        yield from conn.connect()
        logger.warn('* New connection %r', conn.key)
        return conn

    def recycle_connection(self, conn):
        """Make a connection available for reuse.

        This also prunes the pool if it exceeds the size limits.
        """
        if conn.stale():
            conn.close()
            return

        conns = self.connections.setdefault(conn.key, [])
        conns.append(conn)
        self.queue.append(conn)

        if len(conns) > self.max_tasks:
            victims = conns  # Prune one connection for this key.
        elif len(self.queue) > self.max_pool:
            victims = self.queue  # Prune one connection for any key.
        else:
            return

        for victim in victims:
            if victim.stale():  # Prefer pruning the oldest stale connection.
                logger.warn('closing stale connection %r', victim.key)
                break
        else:
            victim = victims[0]
            logger.warn('closing oldest connection %r', victim.key)

        conns = self.connections[victim.key]
        conns.remove(victim)
        if not conns:
            del self.connections[victim.key]
        self.queue.remove(victim)
        victim.close()


class Connection:

    def __init__(self, pool, host, port, ssl):
        self.pool = pool
        self.host = host
        self.port = port
        self.ssl = ssl
        self.reader = None
        self.writer = None
        self.key = None

    def stale(self):
        return self.reader is None or self.reader.at_eof()

    @asyncio.coroutine
    def connect(self):
        self.reader, self.writer = yield from asyncio.open_connection(
            self.host, self.port, ssl=self.ssl)
        peername = self.writer.get_extra_info('peername')
        if peername:
            self.host, self.port = peername[:2]
        else:
            logger.warn('NO PEERNAME %r %r %r', self.host, self.port, self.ssl)
        self.key = self.host, self.port, self.ssl

    def close(self, recycle=False):
        if recycle and not self.stale():
            self.pool.recycle_connection(self)
        else:
            self.writer.close()
            self.pool = self.reader = self.writer = None


@asyncio.coroutine
def open_http_conn(url, pool, *, method='GET', headers=None, version='1.1'):
    parts = urllib.parse.urlparse(url)
    assert parts.scheme in ('http', 'https'), repr(url)
    ssl = parts.scheme == 'https'
    port = parts.port or (443 if ssl else 80)
    path = parts.path or '/'
    path = '%s?%s' % (path, parts.query) if parts.query else path

    logger.warn('* Connecting to %s:%s using %s for %s',
                parts.hostname, port, 'ssl' if ssl else 'tcp', url)
    conn = yield from pool.get_connection(parts.hostname, port, ssl)

    headers = dict(headers) if headers else {}  # Must use Cap-Words.
    headers.setdefault('User-Agent', 'asyncio-example-crawl/0.0')
    headers.setdefault('Host', parts.netloc)
    headers.setdefault('Accept', '*/*')
    lines = ['%s %s HTTP/%s' % (method, path, version)]
    lines.extend('%s: %s' % kv for kv in headers.items())
    for line in lines + ['']:
        logger.info('> %s', line)
    # TODO: close conn if this fails.
    conn.writer.write('\r\n'.join(lines + ['', '']).encode('latin-1'))

    return conn  # Caller must send body if desired, then call get_response().


@asyncio.coroutine
def get_response(conn):

    @asyncio.coroutine
    def getline():
        line = (yield from conn.reader.readline()).decode('latin-1').rstrip()
        logger.info('< %s', line)
        return line

    status_line = yield from getline()
    status_parts = status_line.split(None, 2)
    if len(status_parts) != 3:
        logger.error('bad status_line %r', status_line)
        raise BadStatusLine(status_line)
    http_version, status, reason = status_parts
    status = int(status)

    headers = {}
    while True:
        header_line = yield from getline()
        if not header_line:
            break
        key, value = header_line.split(':', 1)
        # TODO: Continuation lines; multiple header lines per key..
        headers[key.lower()] = value.lstrip()

    if 'content-length' in headers:
        nbytes = int(headers['content-length'])
        output = asyncio.StreamReader()
        asyncio.async(length_handler(nbytes, conn.reader, output))
    elif headers.get('transfer-encoding') == 'chunked':
        output = asyncio.StreamReader()
        asyncio.async(chunked_handler(conn.reader, output))
    else:
        output = conn.reader

    return http_version[5:], status, reason, headers, output


@asyncio.coroutine
def length_handler(nbytes, input, output):
    while nbytes > 0:
        buffer = yield from input.read(min(nbytes, 256*1024))
        if not buffer:
            logger.error('premature end for content-length')
            output.set_exception(EOFError())
            return
        output.feed_data(buffer)
        nbytes -= len(buffer)
    output.feed_eof()


@asyncio.coroutine
def chunked_handler(input, output):
    logger.info('parsing chunked response')
    nblocks = 0
    nbytes = 0
    while True:
        size_header = yield from input.readline()
        if not size_header:
            logger.error('premature end of chunked response')
            output.set_exception(EOFError())
            return
        logger.debug('size_header = %r', size_header)
        parts = size_header.split(b';')
        size = int(parts[0], 16)
        nblocks += 1
        nbytes += size
        if size:
            logger.debug('reading chunk of %r bytes', size)
            block = yield from input.readexactly(size)
            assert len(block) == size, (len(block), size)
            output.feed_data(block)
        crlf = yield from input.readline()
        assert crlf == b'\r\n', repr(crlf)
        if not size:
            break
    logger.warn('chunked response had %r bytes in %r blocks', nbytes, nblocks)
    output.feed_eof()


class Fetcher:
    """Logic and state for one URL.

    When found in crawler.busy, this represents a URL to be fetched or
    in the process of being fetched; when found in crawler.done, this
    holds the results from fetching it.

    This is usually associated with a task.  This references the
    crawler for the connection pool and to add more URLs to its todo
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
        self.conn = None
        self.status = None
        self.headers = None
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
            conn = None
            try:
                conn = yield from open_http_conn(self.url, self.crawler.pool)
                _, status, _, headers, output = yield from get_response(conn)
                self.status, self.headers = status, headers
                self.body = yield from output.read()
                h_conn = headers.get('connection', '').lower()
                if h_conn != 'close':
                    conn.close(recycle=True)
                    conn = None
                if self.tries > 1:
                    logger.warn('try %r for %r success', self.tries, self.url)
                break
            except (BadStatusLine, OSError) as exc:
                self.exceptions.append(exc)
                logger.warn('try %r for %r raised %r',
                            self.tries, self.url, exc)
            finally:
                if conn is not None:
                    conn.close()
        else:
            # We never broke out of the while loop, i.e. all tries failed.
            logger.error('no success for %r in %r tries',
                         self.url, self.max_tries)
            return
        if status in (300, 301, 302, 303, 307) and headers.get('location'):
            next_url = headers['location']
            self.next_url = urllib.parse.urljoin(self.url, next_url)
            if self.max_redirect > 0:
                logger.warn('redirect to %r from %r', self.next_url, self.url)
                self.crawler.add_url(self.next_url, self.max_redirect-1)
            else:
                logger.error('redirect limit reached for %r from %r',
                             self.next_url, self.url)
        else:
            if status == 200:
                self.ctype = headers.get('content-type')
                self.pdict = {}
                if self.ctype:
                    self.ctype, self.pdict = cgi.parse_header(self.ctype)
                self.encoding = self.pdict.get('charset', 'utf-8')
                if self.ctype == 'text/html':
                    body = self.body.decode(self.encoding, 'replace')
                    # Replace href with (?:href|src) to follow image links.
                    self.urls = set(re.findall(r'(?i)href=["\']?([^\s"\'<>]+)',
                                               body))
                    if self.urls:
                        logger.warn('got %r distinct urls from %r',
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
                 max_tasks=10, max_pool=10,  # Global limits.
                 ):
        self.roots = roots
        self.exclude = exclude
        self.strict = strict
        self.max_redirect = max_redirect
        self.max_tries = max_tries
        self.max_tasks = max_tasks
        self.max_pool = max_pool
        self.todo = {}
        self.busy = {}
        self.done = {}
        self.pool = ConnectionPool(max_pool, max_tasks)
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
                    if host.startswith('www.'):
                        self.root_domains.add(host[4:])
                    else:
                        self.root_domains.add('www.' + host)
                else:
                    parts = host.split('.')
                    if len(parts) > 2:
                        host = '.'.join(parts[-2:])
                    self.root_domains.add(host)
        for root in roots:
            self.add_url(root)
        self.governor = asyncio.Semaphore(max_tasks)
        self.termination = asyncio.Condition()
        self.t0 = time.time()
        self.t1 = None

    def close(self):
        """Close resources (currently only the pool)."""
        self.pool.close()

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
        if host.startswith('www.'):
            if host[4:] in self.root_domains:
                return True
        else:
            if 'www.' + host in self.root_domains:
                return True
        return False

    def _host_okay_lenient(self, host):
        """Check if a host should be crawled, lenient version.

        This compares the last two components of the host.
        """
        parts = host.split('.')
        if len(parts) > 2:
            host = '.'.join(parts[-2:])
        return host in self.root_domains

    def add_url(self, url, max_redirect=None):
        """Add a URL to the todo list if not seen before."""
        if self.exclude and re.search(self.exclude, url):
            return False
        parts = urllib.parse.urlparse(url)
        if parts.scheme not in ('http', 'https'):
            logger.info('skipping non-http scheme in %r', url)
            return False
        host, port = urllib.parse.splitport(parts.netloc)
        if not self.host_okay(host):
            logger.info('skipping non-root host in %r', url)
            return False
        if max_redirect is None:
            max_redirect = self.max_redirect
        if url in self.todo or url in self.busy or url in self.done:
            return False
        logger.warn('adding %r %r', url, max_redirect)
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
