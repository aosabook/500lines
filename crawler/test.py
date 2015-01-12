import asyncio
import socket
import unittest

from aiohttp import web

import crawling


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

    # TODO:
    # * test multiple roots
    # * test redirects, max_redirect
    # * test max_tasks
    # * test max_tries
    # * test that hosts are properly parsed from deep roots
    # * test default and custom encoding
    # * test content-types
    # * test that content-types html and xml are followed, not others
    # * test non-http URLs
    # * test 'exclude'

if __name__ == '__main__':
    unittest.main()
