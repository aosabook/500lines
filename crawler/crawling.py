#!/usr/bin/env python3.4

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
from http.client import BadStatusLine
import logging
import re
import signal
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

    There are limits to both the overal pool and the per-key pool.
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
            logging.error('Exception %r for (%r, %r)' % (exc, host, port))
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
                    logger.warn('* Reusing pooled connection %r FD=%s',
                                key, conn.fileno())
                    return conn

        # Create a new connection.
        conn = Connection(self, host, port, ssl)
        yield from conn.connect()
        logger.warn('* New connection %r FD=%s', conn.key, conn.fileno())
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

    def fileno(self):
        writer = self.writer
        if writer is not None:
            transport = writer.transport
            if transport is not None:
                sock = transport.get_extra_info('socket')
                if sock is not None:
                    return sock.fileno()
        return None

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


class Request:
    """HTTP request.

    Use connect() to open a connection; send_request() to send the
    request; get_response() to receive the response headers.
    """

    def __init__(self, url, pool):
        self.url = url
        self.pool = pool
        self.parts = urllib.parse.urlparse(self.url)
        self.scheme = self.parts.scheme
        assert self.scheme in ('http', 'https'), repr(url)
        self.ssl = self.parts.scheme == 'https'
        self.netloc = self.parts.netloc
        self.hostname = self.parts.hostname
        self.port = self.parts.port or (443 if self.ssl else 80)
        self.path = (self.parts.path or '/')
        self.query = self.parts.query
        if self.query:
            self.full_path = '%s?%s' % (self.path, self.query)
        else:
            self.full_path = self.path
        self.http_version = 'HTTP/1.1'
        self.method = 'GET'
        self.headers = []
        self.conn = None

    @asyncio.coroutine
    def connect(self):
        """Open a connection to the server."""
        logger.warn('* Connecting to %s:%s using %s for %s',
                    self.hostname, self.port,
                    'ssl' if self.ssl else 'tcp',
                    self.url)
        self.conn = yield from self.pool.get_connection(self.hostname,
                                                        self.port, self.ssl)

    def close(self, recycle=False):
        """Close the connection, recycle if requested."""
        if self.conn is not None:
            if not recycle:
                logger.warn('closing connection %r', self.conn.key)
            self.conn.close(recycle)
            self.conn = None

    @asyncio.coroutine
    def putline(self, line):
        """Write a line to the connection.

        Used for the request line and headers.
        """
        logging.info('>', line)
        self.conn.writer.write(line.encode('latin-1') + b'\r\n')

    @asyncio.coroutine
    def send_request(self):
        """Send the request."""
        request_line = '%s %s %s' % (self.method, self.full_path,
                                     self.http_version)
        yield from self.putline(request_line)
        # TODO: What if a header is already set?
        self.headers.append(('User-Agent', 'asyncio-example-crawl/0.0'))
        self.headers.append(('Host', self.netloc))
        self.headers.append(('Accept', '*/*'))
        ##self.headers.append(('Accept-Encoding', 'gzip'))
        for key, value in self.headers:
            line = '%s: %s' % (key, value)
            yield from self.putline(line)
        yield from self.putline('')

    @asyncio.coroutine
    def get_response(self):
        """Receive the response."""
        response = Response(self.conn.reader)
        yield from response.read_headers()
        return response


class Response:
    """HTTP response.

    Call read_headers() to receive the request headers.  Then check
    the status attribute and call get_header() to inspect the headers.
    Finally call read() to receive the body.
    """

    def __init__(self, reader):
        self.reader = reader
        self.http_version = None  # 'HTTP/1.1'
        self.status = None  # 200
        self.reason = None  # 'Ok'
        self.headers = []  # [('Content-Type', 'text/html')]

    @asyncio.coroutine
    def getline(self):
        """Read one line from the connection."""
        line = (yield from self.reader.readline()).decode('latin-1').rstrip()
        logging.info('<', line)
        return line

    @asyncio.coroutine
    def read_headers(self):
        """Read the response status and the request headers."""
        status_line = yield from self.getline()
        status_parts = status_line.split(None, 2)
        if len(status_parts) != 3:
            logger.error('bad status_line %r', status_line)
            raise BadStatusLine(status_line)
        self.http_version, status, self.reason = status_parts
        self.status = int(status)
        while True:
            header_line = yield from self.getline()
            if not header_line:
                break
            # TODO: Continuation lines.
            key, value = header_line.split(':', 1)
            self.headers.append((key, value.strip()))

    def get_redirect_url(self, default=''):
        """Inspect the status and return the redirect url if appropriate."""
        if self.status not in (300, 301, 302, 303, 307):
            return default
        return self.get_header('Location', default)

    def get_header(self, key, default=''):
        """Get one header value, using a case insensitive header name."""
        key = key.lower()
        for k, v in self.headers:
            if k.lower() == key:
                return v
        return default

    @asyncio.coroutine
    def read(self):
        """Read the response body.

        This honors Content-Length and Transfer-Encoding: chunked.
        """
        nbytes = None
        for key, value in self.headers:
            if key.lower() == 'content-length':
                nbytes = int(value)
                break
        if nbytes is None:
            if self.get_header('transfer-encoding').lower() == 'chunked':
                logging.info('parsing chunked response')
                blocks = []
                while True:
                    size_header = yield from self.reader.readline()
                    if not size_header:
                        logger.error('premature end of chunked response')
                        break
                    logger.debug('size_header = %r', size_header)
                    parts = size_header.split(b';')
                    size = int(parts[0], 16)
                    if size:
                        logger.debug('reading chunk of %r bytes', size)
                        block = yield from self.reader.readexactly(size)
                        assert len(block) == size, (len(block), size)
                        blocks.append(block)
                    crlf = yield from self.reader.readline()
                    assert crlf == b'\r\n', repr(crlf)
                    if not size:
                        break
                body = b''.join(blocks)
                logger.warn('chunked response had %r bytes in %r blocks',
                            len(body), len(blocks))
            else:
                logger.debug('reading until EOF')
                body = yield from self.reader.read()
                # TODO: Should make sure not to recycle the connection
                # in this case.
        else:
            body = yield from self.reader.readexactly(nbytes)
        return body


class Fetcher:
    """Logic and state for one URL.

    When found in crawler.busy, this represents a URL to be fetched or
    in the process of being fetched; when found in crawler.done, this
    holds the results from fetching it.

    This is usually associated with a task.  This references the
    crawler for the connection pool and to add more URLs to its todo
    list.

    Call fetch() to do the fetching, then report() to print the results.
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
        self.request = None
        self.response = None
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
            self.request = None
            try:
                self.request = Request(self.url, self.crawler.pool)
                yield from self.request.connect()
                yield from self.request.send_request()
                self.response = yield from self.request.get_response()
                self.body = yield from self.response.read()
                h_conn = self.response.get_header('connection').lower()
                h_t_enc = self.response.get_header('transfer-encoding').lower()
                if h_conn != 'close':
                    self.request.close(recycle=True)
                    self.request = None
                if self.tries > 1:
                    logger.warn('try %r for %r success', self.tries, self.url)
                break
            except (BadStatusLine, OSError) as exc:
                self.exceptions.append(exc)
                logger.warn('try %r for %r raised', self.tries, self.url, exc)
                ##import pdb; pdb.set_trace()
                # Don't reuse the connection in this case.
            finally:
                if self.request is not None:
                    self.request.close()
        else:
            # We never broke out of the while loop, i.e. all tries failed.
            logger.error('no success for %r in %r tries',
                         self.url, self.max_tries)
            return
        next_url = self.response.get_redirect_url()
        if next_url:
            self.next_url = urllib.parse.urljoin(self.url, next_url)
            if self.max_redirect > 0:
                logger.warn('redirect to %r from %r', self.next_url, self.url)
                self.crawler.add_url(self.next_url, self.max_redirect-1)
            else:
                logger.error('redirect limit reached for %r from %r',
                             self.next_url, self.url)
        else:
            if self.response.status == 200:
                self.ctype = self.response.get_header('content-type')
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

    def report(self, stats, file=None):
        """Print a report on the state for this URL.

        Also update the Stats instance.
        """
        if self.task is not None:
            if not self.task.done():
                stats.add('pending')
                print(self.url, 'pending', file=file)
                return
            elif self.task.cancelled():
                stats.add('cancelled')
                print(self.url, 'cancelled', file=file)
                return
            elif self.task.exception():
                stats.add('exception')
                exc = self.task.exception()
                stats.add('exception_' + exc.__class__.__name__)
                print(self.url, exc, file=file)
                return
        if len(self.exceptions) == self.tries:
            stats.add('fail')
            exc = self.exceptions[-1]
            stats.add('fail_' + str(exc.__class__.__name__))
            print(self.url, 'error', exc, file=file)
        elif self.next_url:
            stats.add('redirect')
            print(self.url, self.response.status, 'redirect', self.next_url,
                  file=file)
        elif self.ctype == 'text/html':
            stats.add('html')
            size = len(self.body or b'')
            stats.add('html_bytes', size)
            print(self.url, self.response.status,
                  self.ctype, self.encoding,
                  size,
                  '%d/%d' % (len(self.new_urls or ()), len(self.urls or ())),
                  file=file)
        else:
            size = len(self.body or b'')
            if self.response.status == 200:
                stats.add('other')
                stats.add('other_bytes', size)
            else:
                stats.add('error')
                stats.add('error_bytes', size)
                stats.add('status_%s' % self.response.status)
            print(self.url, self.response.status,
                  self.ctype, self.encoding,
                  size,
                  file=file)


class Stats:
    """Record stats of various sorts."""

    def __init__(self):
        self.stats = {}

    def add(self, key, count=1):
        self.stats[key] = self.stats.get(key, 0) + count

    def report(self, file=None):
        for key, count in sorted(self.stats.items()):
            print('%10d' % count, key, file=file)


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
            logging.info('skipping non-http scheme in', url)
            return False
        host, port = urllib.parse.splitport(parts.netloc)
        if not self.host_okay(host):
            logging.info('skipping non-root host in', url)
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

    def report(self, file=None):
        """Print a report on all completed URLs."""
        if self.t1 is None:
            self.t1 = time.time()
        dt = self.t1 - self.t0
        if dt and self.max_tasks:
            speed = len(self.done) / dt / self.max_tasks
        else:
            speed = 0
        stats = Stats()
        print('*** Report ***', file=file)
        try:
            show = []
            show.extend(self.done.items())
            show.extend(self.busy.items())
            show.sort()
            for url, fetcher in show:
                fetcher.report(stats, file=file)
        except KeyboardInterrupt:
            print('\nInterrupted', file=file)
        print('Finished', len(self.done),
              'urls in %.3f secs' % dt,
              '(max_tasks=%d)' % self.max_tasks,
              '(%.3f urls/sec/task)' % speed,
              file=file)
        stats.report(file=file)
        print('Todo:', len(self.todo), file=file)
        print('Busy:', len(self.busy), file=file)
        print('Done:', len(self.done), file=file)
        print('Date:', time.ctime(), 'local time', file=file)
