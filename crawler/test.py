import asyncio
from contextlib import contextmanager
import io
import logging
import socket
import unittest

from aiohttp import web

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
        self.addCleanup(self.loop.close)
        self.port = self._find_unused_port()
        self.app_url = "http://127.0.0.1:{}".format(self.port)
        self.app = self.loop.run_until_complete(self._create_server())

    def _find_unused_port(self):
        s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        s.bind(('127.0.0.1', 0))
        port = s.getsockname()[1]
        s.close()
        return port

    @asyncio.coroutine
    def _create_server(self):
        app = web.Application(loop=self.loop, debug=True)
        srv = yield from self.loop.create_server(
            app.make_handler(), '127.0.0.1', self.port)

        self.addCleanup(srv.close)
        return app

    def add_page(self, url, links):
        @asyncio.coroutine
        def handler(req):
            yield from req.read()
            text = ''.join('<a href="{}"></a>'.format(link) for link in links)
            body = text.encode('utf-8')
            return web.Response(body=body, headers=[
                ('CONTENT-TYPE', 'text/html; charset=utf-8')])

        self.app.router.add_route('GET', url, handler)
        return self.app_url + url

    def add_redirect(self, url, link):
        @asyncio.coroutine
        def handler(_):
            raise web.HTTPFound(link)

        self.app.router.add_route('GET', url, handler)
        return self.app_url + url

    def assertStat(self, stat, **kwargs):
        for name, value in kwargs.items():
            msg = '{}.{} not equal to {!r}'.format(stat, name, value)
            self.assertEqual(getattr(stat, name), value, msg)

    def crawl(self, *args, **kwargs):
        crawler = crawling.Crawler(*args, loop=self.loop, **kwargs)
        self.loop.run_until_complete(crawler.crawl())
        return crawler

    def test_link(self):
        # foo links to bar, which is missing.
        url = self.add_page('/foo', [self.app_url + '/bar'])
        crawler = self.crawl([url])
        self.assertEqual(2, len(crawler.done))
        self.assertStat(crawler.done[0],
                        url=self.app_url + '/foo',
                        num_urls=1,
                        num_new_urls=1)

        self.assertStat(crawler.done[1],
                        url=self.app_url + '/bar',
                        status=404)

    def test_link_cycle(self):
        # foo and bar link to each other.
        url = self.add_page('/foo', [self.app_url + '/bar'])
        self.add_page('/bar', [self.app_url + '/foo'])
        crawler = self.crawl([url])
        self.assertEqual(2, len(crawler.done))
        self.assertStat(crawler.done[0],
                        url=self.app_url + '/foo',
                        num_urls=1,
                        num_new_urls=1)

        self.assertStat(crawler.done[1],
                        url=self.app_url + '/bar',
                        num_urls=1,
                        num_new_urls=0)

    def test_prohibited_host(self):
        # Link to example.com.
        url = self.add_page('/', ['http://example.com'])
        crawler = self.crawl([url])
        self.assertStat(crawler.done[0], num_urls=0)

    def test_strict_host_checking(self):
        crawler = crawling.Crawler(['http://example.com'], loop=self.loop)
        self.assertTrue(crawler.url_allowed("http://www.example.com"))
        self.assertFalse(crawler.url_allowed("http://foo.example.com"))

    def test_lenient_host_checking(self):
        crawler = crawling.Crawler(['http://example.com'], strict=False,
                                   loop=self.loop)
        self.assertTrue(crawler.url_allowed("http://www.example.com"))
        self.assertTrue(crawler.url_allowed("http://foo.example.com"))

    # * test max_tries
    def test_roots(self):
        crawler = crawling.Crawler(['http://a', 'http://b'], loop=self.loop)
        self.assertTrue(crawler.url_allowed("http://a/a"))
        self.assertTrue(crawler.url_allowed("http://b/b"))
        self.assertFalse(crawler.url_allowed("http://c/c"))

    def test_redirect(self):
        # "/" redirects to "/foo", and "/foo" redirects to "/bar".
        foo = self.app_url + '/foo'
        bar = self.app_url + '/bar'

        url = self.add_redirect('/', foo)
        self.add_redirect('/foo', bar)
        crawler = self.crawl([url])
        self.assertStat(crawler.done[0], status=302, next_url=foo)
        self.assertStat(crawler.done[1], status=302, next_url=bar)
        self.assertStat(crawler.done[2], status=404)

        with capture_logging() as messages:
            crawler2 = self.crawl([url], max_redirect=1)
            self.assertEqual(2, len(crawler2.done))
            self.assertStat(crawler.done[0], status=302, next_url=foo)

        self.assertIn('redirect limit reached', messages)

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
            self.app.router.add_route('GET', url, handler)
        home = self.add_page('/', urls)
        crawler = self.crawl([home], max_tasks=2)
        self.assertEqual(2, max_tasks)
        self.assertEqual(4, len(crawler.done))

    # * test that hosts are properly parsed from deep roots
    # * test default and custom encoding
    # * test content-types
    # * test that content-types html and xml are followed, not others
    # * test non-http URLs
    # * test 'exclude'

if __name__ == '__main__':
    unittest.main()
