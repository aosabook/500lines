# An Async Web Crawler

Classical computer science focuses on efficient algorithms that complete a computation as quickly as possible. But many networked programs spend their time not computing, but waiting for responses from remote machines. These I/O-bound programs present a very different challenge: wait for a huge number of network responses efficiently. The modern answer to this problem is asynchronous I/O, or "async".

This chapter presents a simple web crawler. The crawler is an archetypal async application because it waits for many responses, but does little computation. The more pages it can fetch at once, the sooner it completes. If it devotes a thread per page it will run out of memory or some other thread-related resource before it runs out of sockets, so it uses asynchronous I/O to avoid the need for threads.

We present the example in three stages. First, we show an async event loop and sketch a crawler that uses the loop with callbacks: it is very efficient but difficult to extend. We digress to explain Python generators, and how they are used as asynchronous coroutines, a graceful and robust alternative to callbacks. In the third stage, we show how coroutines use an async queue to coordinate with each other.

## The Task

Download all pages on a site. Beginning with a root URL, fetch the page, extract its URLs, and add all new URLs to a queue. When we fetch a page that contains no new URLs, and our queue is empty, then stop.

This is easily parallelizable: as we add many URLs to our queue we can launch many HTTP fetch operations on separate sockets. Process the responses as we receive them, adding the resulting URLs to the queue. There's a point of diminishing returns: too many connections degrades performance, so we cap the number of concurrent operations, and leave the remaining URLs in the queue until some have completed.

## The Traditional Approach

Traditionally we'd start a thread pool: one thread per in-flight I/O operation. On Jesse's system, a Python thread costs 22k of memory, and starting more than (TODO: max #?) threads leads to errors. If we want to scale up to a large number of simultaneous operations on these thousands of connections, the hard limit or the memory usage will be a bottleneck. How can we get past it?

## Async

### Single-Threaded I/O Concurrency

Quote c10k paper. Guido: "We'll run out threads before we run out of sockets." We need some way to use less memory and avoid other system limits on threads.

Async: use epoll or kqueue to wait for many responses with just one thread. Async's heart is a simple event loop:

```python
from selectors import DefaultSelector

selector = DefaultSelector()
stopped = False

while not stopped:
    events = selector.select()
    for event_key, event_mask in events:
        callback = event_key.data
        callback()
```

`DefaultSelector` uses the best notification mechanism available on your system. The call to `select` awaits the next I/O events, and the loop then runs the callbacks waiting for them. We register for notifications about network I/O by creating a non-blocking socket and registering it. For example, to connect asynchronously to xkcd.com:

```python
def connected():
    print('connected!')

sock = socket.socket()
sock.setblocking(False)
try:
    sock.connect(('xkcd.com', 80))
except BlockingIOError:
    pass
selector.register(sock.fileno(), EVENT_WRITE, connected)
```

TODO: explain BlockingIOError.

### Async's Problem

Given a runty async framework like this, how would we build a web crawler? The `connected` callback must send an HTTP request to the server and await a response, so we need a series of callbacks. Let us collect these callbacks into an object:

```python
class Fetcher:
    def __init__(self, url):
        self.response = b''
        self.url = url
        self.sock = None

    def fetch(self):
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
```

The `_process_response` method, not shown, parses URLs from the response and adds new ones to `urls_todo`.

Async's problem is it leads to spaghetti code. Need some way to schedule concurrent processing logic. Node uses callbacks, which are notorious. They're worse in Python because we have no anonymous closures. Yuck!

Even worse, what happens if a callback throws an exception, before it calls the next callback in the chain? The chain is broken on both ends. We forgot where we were going and whence we came: the stack trace shows only that the event loop was running a callback. We know nothing of how we reached the point where the exception was thrown. We can't easily install an exception handler for a chain of callbacks, the way a try / except block wraps a tree of function calls.[^7]

### Solution: Asynchronous Coroutines

We'll jump ahead to show coroutine example in crawling.py. It uses the full stack: asyncio and aiohttp. It's very nice!

```python
@asyncio.coroutine
def fetch(self, url, max_redirect):
    tries = 0
    while tries < max_tries:
        try:
            response = yield from aiohttp.request('get', url)
            break  # Success!
        except aiohttp.ClientError:
            tries += 1
    else:
        # We never broke out of the loop: all tries failed.
        self.record_statistic("failure")
        return

    stat = yield from self.process_response(response)
    self.record_statistic(stat)
```

Compared to the 22k memory per thread and OS's limits on threads, coroutines take barely 3k of memory on Jesse's system. Python can easily start hundreds of thousands of coroutines.

We digress to describe how coroutines are implemented, then return to show how they're used.

#### How Python Generators Work

##### yield and send

This is a generator function:

```python
>>> def gen():
...     print('a')
...     result = yield 1
...     print('result: {}'.format(result))
...     result2 = yield 2
...     print('result2: {}'.format(result2))
...     
```

TODO: rename from gen() to gen_fn().

When Python compiles `gen` to bytecode, it sees the `yield` statement and knows that `gen` is a generator function. It sets a particular flag to remember this fact:

```python
>>> generator_flag = 1 << 5
>>> bool(gen.__code__.co_flags & generator_flag)
True
```

When you call a generator function, Python sees that the generator flag is set, and it doesn't actually run the function in the normal way. Instead, it produces a generator:

```python
>>> g = gen()
>>> type(g)
<class 'generator'>
```

The generator has a pointer to the code, the body of `gen`:

```python
>>> g.gi_code.co_name
'gen'
```

All generators from calls to `gen` point to this same code. But each generator has its own stack frame, containing its local variables and its last instruction pointer. This stack frame is not on any actual stack, it sits in heap memory waiting to be used. In the beginning, the last instruction pointer is -1, meaning the generator has not begun:

```python
>>> g.gi_frame.f_lasti
-1
```

When we call `send`, the generator reaches its first `yield`, and pauses. The return value of `send` is 1, since that was passed to the `yield` expression. The generator's last instruction is 3 bytecodes from the start, part way through the 56 bytes of compiled Python:

```python
>>> g.send(None)
1
>>> g.gi_frame.f_lasti
3
>>> len(g.gi_code.co_code)
56
```

The generator can be resumed at any time, from any function, because its stack frame is not actually on the stack: it's on the heap. TODO: explain more clearly.

We can send the value "hello" into the generator and it becomes the result of the `yield` expression, and the generator continues until it yields 2:

```python
>>> g.send('hello')
result: hello
2
```

Its stack frame now contains the local variable `result`:

```python
>>> g.gi_frame.f_locals
{'result': 'hello'}
```

If we call `send` again, the generator continues from its second `yield`, and finishes by raising the special `StopIteration` exception:

```python
>>> g.send('goodbye')
result2: goodbye
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
StopIteration
```

##### coroutines

So we have a function that can pause, and be resumed with a value. Sounds like a good primitive upon which to build an async programming model, without spaghetti callbacks. This type of function is a "coroutine": it's a routine that's cooperatively scheduled (TODO: interleaved?) with other routines in the program. Let's see how coroutines are implemented in asyncio.

First we need a way to represent a future result, which a coroutine is waiting for. A very simple version:

```python
class Future:
    def __init__(self):
        self.result = None
        self._callbacks = []

    def result(self):
        return self.result

    def add_done_callback(self, fn):
        self._callbacks.append(fn)

    def set_result(self, result):
        self.result = result
        for fn in self._callbacks:
            fn(self)
```

Review, above, our `fetch` and `connected` methods. `fetch` starts connecting a socket, then registers the callback, `connected`, to be executed when the socket is ready. Now we can combine these two steps into one coroutine:

```python
    def fetch(self):
        sock = socket.socket()
        sock.setblocking(False)
        try:
            sock.connect(('xkcd.com', 80))
        except BlockingIOError:
            pass

        f = Future()

        def on_connected():
            selector.unregister(sock.fileno())
            f.set_result(None)

        selector.register(sock.fileno(), EVENT_WRITE, on_connected)
        yield f
        print('connected!')
```

Now `fetch` is a generator, rather than a regular method, because it contains a `yield` statement. The `yield` pauses `fetch` until the socket is ready, then the inner function `on_connected` marks the Future as completed. So what resumes the generator? We need a coroutine driver. Let's call it Task:

```python
class Task:
    def __init__(self, coro):
        self.coro = coro
        f = Future()
        f.set_result(None)
        self.step(f)

    def step(self, future):
        try:
            next_future = self.coro.send(future.result)
        except StopIteration:
            return

        next_future.add_done_callback(self.step)

fetcher = Fetcher('/')
Task(fetcher.fetch())
```

The Task class gets the `fetch` generator started by sending `None` into it. Then `fetch` runs until it yields a Future, which the Task captures as `next_future`. When the socket is connected, the Future runs its callback, which calls `step`, which resumes `fetch`.

##### Factoring coroutines with `yield from`

Just connecting the socket isn't enough, let's read from it, too:

```python
    def fetch(self):

		 # ... connection logic from above, then:

        while True:
            f = Future()

            def on_readable():
                selector.unregister(sock.fileno())
                f.set_result(sock.recv(4096))  # Read 4k at a time.

            selector.register(sock.fileno(), EVENT_READ, on_readable)
            chunk = yield f
            if chunk:
                self.response += chunk
            else:
                # Done reading.
                break
```

This code, which reads a whole message from a socket, seems generally useful. How can we factor it out from `fetch` into a subroutine? Here is where Python 3's `yield from` statement comes in: it lets one generator *delegate* to another. Let's return to our simple generator example:

```python
>>> def gen():
...     print('a')
...     result = yield 1
...     print('result: {}'.format(result))
...     result2 = yield 2
...     print('result2: {}'.format(result2))
...     
```

If we want to call this generator from another generator, we can delegate to it with `yield from`:

```python
>> def caller():
    yield from gen()
>>> c = caller()
>>> c.send(None)
1
>>> c.gi_frame.f_lasti
9
>>> c.send(None)
result: None
2
>>> c.gi_frame.f_lasti  # c hasn't advanced.
9
>>> c.send(None)
Traceback (most recent call last):
result2: None
  File "<input>", line 1, in <module>
StopIteration
```

While `caller` yields from `gen`, `caller` does not advance. Notice that its instruction pointer remains at 9, the site of its `yield from` statement, even while the inner generator `gen` advances from one `yield` statement to the next. From our perspective outside `caller`, we cannot tell if the values it yields are from `caller` or from the generator it delegates to. And from inside `gen`, we cannot tell if values are sent in from `caller` or from outside it. The `yield from` statement acts as transparent channel, through which values flow in and out of `gen` until it completes.

We use `yield from` to build subroutines:

```python
def read(sock):
    f = Future()

    def on_readable():
        selector.unregister(sock.fileno())
        f.set_result(sock.recv(4096))  # Read 4k at a time.

    selector.register(sock.fileno(), EVENT_READ, on_readable)
    chunk = yield f
    return chunk

def read_all(sock):
    response = []
    chunk = yield from read(sock)
    while chunk:
        response.append(chunk)
        chunk = yield from read(sock)

    return b''.join(response)

class Fetcher:
    def fetch(self):
		 # ... connection logic from above, then:
        sock.sendall('GET {} HTTP/1.0\r\n\r\n'.format(self.url).encode('ascii'))
        self.response = yield from read_all(sock)
```

The fetcher yields from our `read_all` method, which in turn yields from `read`. Miraculously, the Task class needs no modification: Task drives the outer `fetch` coroutine. When `read` yields a Future, the Task receives it through the chain of `yield from` statements, just the same as if the Future were yielded directly from `fetch`. And when a Future is resolved and Task sends its result into `fetch`, the value is received by `read`, just the same as if the Task were driving `read` directly.

That's coroutines! This sketch of Future and Task outlines how asyncio attains the best of both worlds: concurrent I/O that's more efficient than threads and more legible than callbacks.

Zero-copy I/O, fair scheduling, exception / traceback handling, task cancelation are much more sophisticated in asyncio than here.

Real coding with coroutines is much simpler than you saw here. We've dived into the low-level implementation of coroutines here, and so you saw Futures and explicit callbacks, non-blocking sockets and the call to ``select``. When you're actually building an application with asyncio none of this appears in your code. E.g. to read from an HTTP response:

```python
    @asyncio.coroutine
    def fetch(self):
        body = yield from response.read()
```

The `coroutine` decorator does nothing magical. In fact it's not even required, it merely aids reading comprehension and includes some features to catch programming mistakes.

If you ignore the ``yield from`` you can imagine your coroutine truly blocks while awaiting I/O. In fact, ``read`` is a coroutine, and yielding from it pauses ``fetch`` until the I/O completes. While ``fetch`` is paused, asyncio's event loop does other work and awaits other I/O events; ``fetch`` is resumed with the result of ``read`` on the next loop iteration after the I/O is ready.

TODO: Exception handling works normally, unlike with callbacks....

So coroutines are much more familiar, robust, and factorable than callbacks. How do they compare to threads? Coroutines aren't merely as elegant as threads: they're *more* elegant. They task-switch explicitly at `yield from` statements, otherwise everything's completely deterministic. TODO: Link to and quote from Glyph's "Unyielding"[^4].

Python threads are the worst of both worlds: the GIL means they're not parallel, but non-deterministic switching makes them prone to races. Coroutines are the best of both.

### Coordinating Coroutines

We've seen how asyncio's event loop and coroutines are implemented, now we can finally return to our original example, the crawler.

(TODO: come up with a consistent vocab for "work" and "workers" and use it throughout this chapter.) The crawler fetches its first page, extracts links, and adds them to a work queue. After this things get complicated: we want our maximum number of workers running, but no more. Whenever a worker finishes its fetching a page, it should immediately pull the next link from the work queue. We'll pass through periods when there isn't enough work to go around, so some workers must sleep. When a worker hits a link-rich page, then all the workers should wake to work on the suddenly grown queue.

And finally, we need to know as soon as all work is complete so we can print a report and exit.

If the workers were threads, we'd use a synchronized queue[^5] from the Python standard library. Every time an item is put in the queue, it increments a count of "tasks". Worker threads call `task_done` after completing work on an item. The main thread blocks on `Queue.join` until each item put in the queue is matched by a `task_done` call, then it exits.

Coroutines use the exact same pattern with a `JoinableQueue` from asyncio.[^6]

```python
class Crawler:
    def __init__(self, root_url, max_redirect):
        self.max_redirect = max_redirect
        self.q = asyncio.JoinableQueue(loop=self.loop)
        self.q.put((root_url, self.max_redirect))

    @asyncio.coroutine
    def crawl(self):
        """Run the crawler until all finished."""
        workers = [asyncio.Task(self.work())
                   for _ in range(self.max_tasks)]

        # When all work is done, exit.
        yield from self.q.join()
        for w in workers:
            w.cancel()

    @asyncio.coroutine
    def work(self):
        while True:
            url, max_redirect = yield from self.q.get()
            yield from self.fetch(url, max_redirect)
            for url in self.extract_urls():
                if url not in self.urls:
                    self.add_url(url, self.max_redirect)

            self.q.task_done()

	@asyncio.coroutine
	def fetch(self, url, max_redirect):
	    tries = 0
	    while tries < max_tries:
	        try:
	            response = yield from aiohttp.request('get', url)
	            break  # Success!
	        except aiohttp.ClientError:
	            tries += 1
	    else:
	        # We never broke out of the loop: all tries failed.
	        self.record_statistic("failure")
	        return

        # TODO: various edits of this "fetch" method are inconsistent,
        # the reader won't get a clear picture of it. Refactor the
        # actual method and make one edit of it to show in the chapter.
        if response.status in (300, 301, 302, 303, 307):
            if max_redirect > 0:
                self.add_url(next_url, max_redirect - 1)
	     else if response.status == 200:
            stat = yield from self.process_response(response)
            self.record_statistic(stat)

    def add_url(self, url, max_redirect=None):
        if max_redirect is None:
            max_redirect = self.max_redirect
        self.urls.add(url)
        self.q.put_nowait((url, max_redirect))
```

In the beginning, the crawler is configured with a root URL and `max_redirect`, the number of redirects it is willing to follow to resolve any one URL. It puts them in its queue. The number of unfinished tasks in the queue is one.

The `crawl` coroutine kicks off the workers. `crawl` is like the main thread: it blocks on `join` until all tasks are finished, while the workers run in the background.

Each worker runs the `work` coroutine independently. When a worker finishes fetching a page it extracts all the links and puts new ones in the queue, then calls `task_done` to decrement the counter. Eventually, a worker fetches a page whose URLs have all been fetched already, and there's no work left in the queue. Thus this worker's call to `task_done` decrements the counter to zero and `crawl` is unblocked.

TODO: describe how max_redirects works.

TODO: `join` is a Future that's resolved by `task_done`, show it.

So, we coordinate coroutines with classic patterns from multi-threaded programming. There's no need to reinvent.

Finally we print a report and quit. TODO: describe `loop.run_until_complete(crawler.crawl())`.

`crawl` needs to cancel all the coroutines, otherwise their destructors log warnings as the interpreter shuts down.

# Conclusion

Modern programs are increasingly I/O bound instead of CPU-bound, so async I/O is more often the right pattern. Async coroutines are the best of all worlds: more memory-efficient than threads for I/O-bound programs, and less prone to races than threads are because yield points are explicit. More legible than callbacks, too, and with sane exception handling and stack traces.

Now that you know how async I/O and coroutines work, you can largely forget the details. Async frameworks like Python's asyncio handle them for you. But knowing the fundamentals empowers you to code correctly and efficiently in modern async environments.

[^1]: https://mail.python.org/pipermail/python-ideas/2012-September/016192.html

[^2]: http://pyvideo.org/video/1667/keynote

[^3]: https://groups.google.com/d/msg/python-tulip/bmphRrryuFk/aB45sEJUomYJ

[^4]: https://glyph.twistedmatrix.com/2014/02/unyielding.html

[^5]: https://docs.python.org/3/library/queue.html

[^6]: https://docs.python.org/3/library/asyncio-sync.html#joinablequeue

[^7]: For a complex solution to this problem, see http://www.tornadoweb.org/en/stable/stack_context.html