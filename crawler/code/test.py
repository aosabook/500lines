import asyncio
from contextlib import contextmanager
import io
import logging
import socket
import unittest

from aiohttp import ClientError, web

import crawling


@contextmanager
def capture_logging():
    """See test_redirect for usage."""
    logger = logging.getLogger('crawling')
    level = logger.level
    logger.setLevel(logging.DEBUG)
    handler = logging.StreamHandler(io.StringIO())
    logger.addHandler(handler)

    class Messages:
        def __contains__(self, item):
            return item in handler.stream.getvalue()

        def __repr__(self):
            return repr(handler.stream.getvalue())

    try:
        yield Messages()
    finally:
        logger.removeHandler(handler)
        logger.setLevel(level)


class TestCrawler(unittest.TestCase):
    # Jesse gratefully copied some of this from asyncio's and aiohttp's tests.

    def setUp(self):
        self.loop = asyncio.new_event_loop()
        asyncio.set_event_loop(None)

        def close_loop():
            self.loop.stop()
            self.loop.run_forever()
            self.loop.close()
        self.addCleanup(close_loop)

        self.port = self._find_unused_port()
        self.app_url = "http://127.0.0.1:{}".format(self.port)
        self.app = self.loop.run_until_complete(self._create_server())
        self.crawler = None

    def _find_unused_port(self):
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.bind(('127.0.0.1', 0))
        port = s.getsockname()[1]
        s.close()
        return port

    @asyncio.coroutine
    def _create_server(self):
        app = web.Application(loop=self.loop)
        handler_factory = app.make_handler(debug=True)
        srv = yield from self.loop.create_server(
            handler_factory, '127.0.0.1', self.port)

        # Prevent "Task destroyed but it is pending" warnings.
        self.addCleanup(lambda: self.loop.run_until_complete(
            handler_factory.finish_connections()))

        self.addCleanup(srv.close)
        return app

    def add_handler(self, url, handler):
        self.app.router.add_route('GET', url, handler)

    def add_page(self, url='/', links=None, body=None, content_type=None):
        if not body:
            text = ''.join('<a href="{}"></a>'.format(link)
                           for link in links or [])
            body = text.encode('utf-8')

        if content_type is None:
            content_type = 'text/html; charset=utf-8'

        @asyncio.coroutine
        def handler(req):
            yield from req.read()
            return web.Response(body=body, headers=[
                ('CONTENT-TYPE', content_type)])

        self.add_handler(url, handler)
        return self.app_url + url

    def add_redirect(self, url, link):
        @asyncio.coroutine
        def handler(_):
            raise web.HTTPFound(link)

        self.add_handler(url, handler)
        return self.app_url + url

    def assertDoneCount(self, n):
        self.assertEqual(n, len(self.crawler.done),
                         "Expected {} URLs done, got {}".format(
                             n, len(self.crawler.done)))

    def assertStat(self, stat_index=0, **kwargs):
        stat = self.crawler.done[stat_index]
        for name, value in kwargs.items():
            msg = '{}.{} not equal to {!r}'.format(stat, name, value)
            self.assertEqual(getattr(stat, name), value, msg)

    def crawl(self, urls=None, *args, **kwargs):
        if self.crawler:
            self.crawler.close()
        if urls is None:
            urls = [self.app_url]
        self.crawler = crawling.Crawler(urls, *args, loop=self.loop, **kwargs)
        self.addCleanup(self.crawler.close)
        self.loop.run_until_complete(self.crawler.crawl())

    def test_link(self):
        # "/" links to foo, which is missing.
        self.add_page('/', ['/foo'])
        self.crawl()
        self.assertDoneCount(2)
        self.assertStat(url=self.app_url + '/',
                        num_urls=1,
                        num_new_urls=1)

        self.assertStat(1, url=self.app_url + '/foo', status=404)

    def test_link_cycle(self):
        # foo and bar link to each other.
        url = self.add_page('/foo', ['/bar'])
        self.add_page('/bar', ['/foo'])
        self.crawl([url])
        self.assertDoneCount(2)
        self.assertStat(url=self.app_url + '/foo',
                        num_urls=1,
                        num_new_urls=1)

        self.assertStat(1,
                        url=self.app_url + '/bar',
                        num_urls=1,
                        num_new_urls=0)

    def test_prohibited_host(self):
        # Link to example.com.
        self.add_page('/', ['http://example.com'])
        self.crawl()
        self.assertStat(num_urls=0)

    def test_strict_host_checking(self):
        crawler = crawling.Crawler(['http://example.com'], loop=self.loop)
        self.addCleanup(crawler.close)
        self.assertTrue(crawler.url_allowed("http://www.example.com"))
        self.assertFalse(crawler.url_allowed("http://foo.example.com"))

    def test_lenient_host_checking(self):
        crawler = crawling.Crawler(['http://example.com'], strict=False,
                                   loop=self.loop)
        self.addCleanup(crawler.close)
        self.assertTrue(crawler.url_allowed("http://www.example.com"))
        self.assertTrue(crawler.url_allowed("http://foo.example.com"))

    def test_exclude(self):
        crawler = crawling.Crawler(['http://example.com'],
                                   exclude=r'.*pattern', loop=self.loop)
        self.addCleanup(crawler.close)
        self.assertTrue(crawler.url_allowed("http://example.com"))
        self.assertFalse(crawler.url_allowed("http://example.com/pattern"))

    def test_roots(self):
        crawler = crawling.Crawler(['http://a', 'http://b', 'not-a-host'],
                                   loop=self.loop)
        self.addCleanup(crawler.close)
        self.assertTrue(crawler.url_allowed("http://a/a"))
        self.assertTrue(crawler.url_allowed("http://b/b"))
        self.assertFalse(crawler.url_allowed("http://c/c"))
        self.assertFalse(crawler.url_allowed("http://127.0.0.1"))

    def test_deep_root(self):
        # Make sure 'a' is a root domain if the root is a link deep in 'a'.
        crawler = crawling.Crawler(['http://a/a#fragment'], loop=self.loop)
        self.addCleanup(crawler.close)
        self.assertTrue(crawler.url_allowed("http://a/b"))

    def test_redirect(self):
        # "/" redirects to "/foo", and "/foo" redirects to "/bar".
        foo = self.app_url + '/foo'
        bar = self.app_url + '/bar'

        url = self.add_redirect('/', foo)
        self.add_redirect('/foo', bar)
        self.crawl([url])
        self.assertStat(0, status=302, next_url=foo)
        self.assertStat(1, status=302, next_url=bar)
        self.assertStat(2, status=404)

        with capture_logging() as messages:
            self.crawl([url], max_redirect=1)
            self.assertDoneCount(2)
            self.assertStat(status=302, next_url=foo)

        self.assertIn('redirect limit reached', messages)

    def test_redirect_cycle(self):
        foo = self.app_url + '/foo'
        bar = self.app_url + '/bar'

        url = self.add_redirect('/bar', foo)
        self.add_redirect('/foo', bar)
        self.crawl([url])
        self.assertStat(0, status=302, next_url=foo)
        self.assertStat(1, status=302, next_url=bar)
        self.assertDoneCount(2)

    def test_redirect_join(self):
        # Set up redirects:
        #   foo -> baz
        #   bar -> baz -> quux
        foo = self.app_url + '/foo'
        bar = self.app_url + '/bar'
        baz = self.app_url + '/baz'
        quux = self.app_url + '/quux'

        self.add_redirect('/foo', baz)
        self.add_redirect('/bar', baz)
        self.add_redirect('/baz', quux)

        # Start crawling foo and bar. We follow the foo -> baz redirect but
        # not bar -> baz, since by then baz is already seen.
        self.crawl([foo, bar])
        import pprint
        pprint.pprint(self.crawler.done)
        self.assertStat(0, url=foo, status=302, next_url=baz)

        # We fetched bar and saw it redirected to baz.
        self.assertStat(1, url=bar, status=302, next_url=baz)

        # But we only fetched baz once.
        self.assertStat(2, url=baz, status=302, next_url=quux)
        self.assertStat(3, url=quux, status=404)
        self.assertDoneCount(4)

    def test_max_tasks(self):
        n_tasks = 0
        max_tasks = 0

        @asyncio.coroutine
        def handler(_):
            nonlocal n_tasks, max_tasks
            n_tasks += 1
            max_tasks = max(n_tasks, max_tasks)
            yield from asyncio.sleep(0.01, loop=self.loop)
            n_tasks -= 1
            return web.Response(body=b'')

        urls = ['/0', '/1', '/2']
        for url in urls:
            self.add_handler(url, handler)
        home = self.add_page('/', urls)
        self.crawl([home], max_tasks=2)
        self.assertEqual(2, max_tasks)
        self.assertDoneCount(4)

    def test_max_tries(self):
        n_tries = 0

        @asyncio.coroutine
        def handler(req):
            nonlocal n_tries
            n_tries += 1
            if n_tries <= 2:
                req.transport.close()  # Network failure.
            return web.Response(body=b'')

        self.add_handler('/', handler)
        with capture_logging() as messages:
            self.crawl([self.app_url])
        self.assertDoneCount(1)
        self.assertStat(status=200)
        self.assertIn('try 1', messages)
        self.assertIn('try 2', messages)

        n_tries = 0
        with capture_logging() as messages:
            self.crawl([self.app_url], max_tries=1)
        self.assertDoneCount(1)
        self.assertStat(status=None)
        stat = self.crawler.done[0]
        self.assertIsInstance(stat.exception, ClientError)
        self.assertIn('failed after 1 tries', messages)

    def test_encoding(self):
        def test_charset(charset, encoding):
            if charset:
                content_type = 'text/html; charset={}'.format(charset)
            else:
                content_type = 'text/html'
            url = '/' + charset
            self.add_page(url, content_type=content_type)
            self.crawl([self.app_url + url])
            self.assertStat(encoding=encoding)

        test_charset('', 'utf-8')
        test_charset('utf-8', 'utf-8')
        test_charset('ascii', 'ascii')

    def test_content_type(self):
        self.add_page(content_type='foo')
        self.crawl([self.app_url])
        self.assertStat(content_type='foo')

    def test_non_html(self):
        # Should search only XML and HTML for links, not other content types.
        body = ('<a href="{}">'.format(self.app_url)).encode('utf-8')

        self.add_page('/xml', body=body, content_type='application/xml')
        self.crawl([self.app_url + '/xml'])
        self.assertStat(0, content_type='application/xml', num_urls=1)
        self.assertStat(1, url=self.app_url + '/')

        self.add_page('/image', content_type='image')
        self.crawl([self.app_url + '/image'])
        self.assertStat(content_type='image', num_urls=0)

    def test_non_http(self):
        body = '<a href="ftp://example.com">'.encode('utf-8')
        self.add_page(body=body)
        self.crawl()
        self.assertStat(num_urls=0)


if __name__ == '__main__':
    unittest.main()
