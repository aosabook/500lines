#!/usr/bin/env python3.4

"""Sloppy little crawler, demonstrates a hand-made event loop and callbacks."""

from selectors import *
import socket
import re
import urllib.parse
import time


urls_seen = set(['/'])
urls_todo = set(['/'])
concurrency_achieved = 0
selector = DefaultSelector()
stopped = False


class Fetcher:
    def __init__(self, url):
        self.response = b''
        self.url = url
        self.sock = None

    def fetch(self):
        global concurrency_achieved
        concurrency_achieved = max(concurrency_achieved, len(urls_todo))

        self.sock = socket.socket()
        self.sock.setblocking(False)
        try:
            self.sock.connect(('xkcd.com', 80))
        except BlockingIOError:
            pass
        selector.register(self.sock.fileno(), EVENT_WRITE, self.connected)

    def connected(self, key, mask):
        selector.unregister(key.fd)
        self.sock.sendall(
            'GET {} HTTP/1.0\r\n\r\n'.format(self.url).encode('ascii'))
        selector.register(key.fd, EVENT_READ, self.read_response)

    def read_response(self, key, mask):
        global stopped

        chunk = self.sock.recv(4096)  # 4k buffer size.
        if chunk:
            self.response += chunk
        else:
            selector.unregister(key.fd)  # Done reading.
            self._process_response()
            urls_todo.remove(self.url)
            if not urls_todo:
                stopped = True
            print(self.url)

    def body(self):
        body = self.response.split(b'\r\n\r\n', 1)[1]
        return body.decode('utf-8')

    def _process_response(self):
        if not self.response:
            print('error: {}'.format(self.url))
            return
        if not self._is_html():
            return
        urls = set(re.findall(r'''(?i)href=["']?([^\s"'<>]+)''',
                              self.body()))

        for url in urls:
            normalized = urllib.parse.urljoin(self.url, url)
            parts = urllib.parse.urlparse(normalized)
            if parts.scheme not in ('', 'http', 'https'):
                continue
            host, port = urllib.parse.splitport(parts.netloc)
            if host and host.lower() not in ('xkcd.com', 'www.xkcd.com'):
                continue
            defragmented, frag = urllib.parse.urldefrag(parts.path)
            if defragmented not in urls_seen:
                urls_todo.add(defragmented)
                urls_seen.add(defragmented)
                Fetcher(defragmented).fetch()

    def _is_html(self):
        head, body = self.response.split(b'\r\n\r\n', 1)
        headers = dict(h.split(': ') for h in head.decode().split('\r\n')[1:])
        return headers.get('Content-Type', '').startswith('text/html')


start = time.time()
fetcher = Fetcher('/')
fetcher.fetch()

while not stopped:
    events = selector.select()
    for event_key, event_mask in events:
        callback = event_key.data
        callback(event_key, event_mask)

print('{} URLs fetched in {:.1f} seconds, achieved concurrency = {}'.format(
    len(urls_seen), time.time() - start, concurrency_achieved))
