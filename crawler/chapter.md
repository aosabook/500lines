# An Async Web Crawler

Classical computer science focuses on efficient algorithms that complete a computation as quickly as possible. But many networked programs spend their time not computing, but waiting for responses from remote machines. These I/O-bound programs present a very different challenge: wait for a huge number of network responses efficiently. The modern answer to this problem is asynchronous I/O, or "async".

This chapter presents a simple web crawler. The crawler is an archetypal async application because it waits for many responses, but does little computation. The more pages it can fetch at once, the sooner it completes. If it devotes a thread per page it will run out of memory or other thread-related resource before it runs out of sockets, so it uses asynchronous I/O to avoid the need for threads.

We present the example in three stages. First, we show an async event loop and sketch a crawler that uses the loop with callbacks: it is very efficient but difficult to extend. We digress to explain Python generators, and how they are used as asynchronous coroutines, a stylish and robust alternative to callbacks. In the third stage, we show how coroutines use an async queue to coordinate with each other.

## The Task

A web crawler downloads all pages on a website. Beginning with a root URL, it fetches the page, parses it for links to pages it has not seen, and adds the links to a queue. When it fetches a page with no unseen links and the queue is empty, stop.

This is easily parallelizable: as we add links to our queue we can launch many HTTP fetch operations on separate sockets. Process the responses as we receive them, adding the resulting URLs to the queue. There's a point of diminishing returns: too many connections degrades performance, so we cap the number of concurrent operations, and leave the remaining URLs in the queue until some have completed.

## The Traditional Approach

Traditionally we would start a thread pool: one thread per in-flight I/O operation. But threads are expensive, and operating systems enforce a variety of hard caps on the number of threads a process, user, or machine may have. On Jesse's system, a Python thread costs 22k of memory, and starting tens of thousands of threads causes failures. If we scale up to tens of thousands of simultaneous operations on concurrent connections, we run out of threads before we run out of connections. Per-thread overhead or system limits on threads are the bottleneck.

In his influential article "The C10K problem", Dan Kegel outlines the limitations of multithreading for I/O concurrency. He begins,

> It's time for web servers to handle ten thousand clients simultaneously, don't you think? After all, the web is a big place now.

Ten thousand concurrent connections seems dainty now, but the problem has not changed in kind, only size. Back then, using a thread per connection for C10K was impractical. Now the threshold is orders of magnitude higher, yet the threshold remains: there is a limit beyond which most systems can still create sockets, but have run out of threads. How can we overcome this?

## Async

### Single-Threaded I/O Concurrency

Since the olden days, operating systems have offered ways for one thread to await I/O on multiple connections. Originally, they used functions like `select` and `poll`. In modern times, the demand for Internet applications with huge numbers of connections has advanced the state of our discipline regarding single-threaded I/O concurrency. The APIs `kqueue` on BSD and `epoll` on Linux provide the same feature more scalably for very large numbers of connections.

Python 3.4's `DefaultSelector` uses the best notification mechanism available on your system. We can write a simple loop that processes I/O notifications as we receive them:

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

The call to `select` awaits the next I/O events, then the loop runs the callbacks waiting for them.

We register for notifications about network I/O by creating a non-blocking socket and registering it. For example, to connect asynchronously to xkcd.com:

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

Irritatingly, a non-blocking socket always throws an exception from `connect`. This exception simply replicates the irritating behavior of the underlying C function, which sets `errno` to the number `EINPROGRESS` to tell you it has begun and you should use an event-notification system to wait for it.

We disregard the spurious error and call `register`, passing in the socket's file descriptor and a Python function to run when the socket becomes writable. This function--the callback--is stored as `event_key.data`, which we retrieve and execute in the event loop above.

### Async's Problem

Given a runty async framework like this, how would we build a web crawler? The `connected` callback must send an HTTP request to the server and await a response, so we need a series of callbacks. Let us collect these callbacks into a `Fetcher` object. It will need a URL, a socket object, and a place to collect the response bytes:

```python
class Fetcher:
    def __init__(self, url):
        self.response = b''
        self.url = url
        self.sock = None
```

We begin fetching xkcd.com by calling `Fetcher.fetch`:

```python
    # Fetcher method.
    def fetch(self):
        self.sock = socket.socket()
        self.sock.setblocking(False)
        try:
            self.sock.connect(('xkcd.com', 80))
        except BlockingIOError:
            pass
        selector.register(self.sock.fileno(), EVENT_WRITE, self.connected)
```

The `fetch` method begins connecting a socket. But notice, the method must return before the connection completes. This is because it must return control to the event loop in order to await the connection. Imagine our total application was structured so:

```python
# Begin fetching http://xkcd.com/353/
fetcher = Fetcher('/353/')
fetcher.fetch()

# Run the event loop.
while not stopped:
    events = selector.select()
    for event_key, event_mask in events:
        callback = event_key.data
        callback()
```

The event loop processes I/O notifications, so `fetch` must return, and allow the event loop to run, so that the application knows when the socket has connected. Only then does it run the `connected` callback, which was registered at the end of `fetch` above. Here is the implementation of the `Fetcher.connected` callback:

```python
    # Fetcher method.
    def connected(self, key, mask):
        print('connected!')
        selector.unregister(key.fd)
        self.sock.sendall(
            'GET {} HTTP/1.0\r\n\r\n'.format(self.url).encode('ascii'))
        selector.register(key.fd, EVENT_READ, self.read_response)
```

The method sends a GET request. The system is responsible for buffering the request locally while it is sent in full, so our application can simply call `sendall`, then wait for a response. Of course, it must register yet another callback and return control to the event loop. The final callback processes the server's response:

```python
    # Fetcher method.
    def read_response(self, key, mask):
        global stopped

        chunk = self.sock.recv(4096)
        if chunk:
            self.response += chunk
        else:
            selector.unregister(key.fd)  # Done reading.
            self._process_response()
            links_todo.remove(self.url)
            if not links_todo:
                stopped = True
```

TODO: replace URLs with "links" here and in code. Show the launch of new Fetchers and mention in prose below.

The callback is executed each time `DefaultSelector` sees that the socket is readable, so the socket either has some data or it is closed. It asks for up to four kilobytes of data from the socket. If there is less than four kilobytes available, `chunk` will contain the available data. If there is more, `chunk` will be 4k long, and the socket remains readable, so the event loop will execute this callback again on the next iteration. If the response is complete, the socket is closed and `chunk` is empty.

The `Fetcher._process_response` method, not shown, parses links from the response and adds new ones to `links_todo`. Once all pages are downloaded the fetcher stops the global event loop.

This example makes async's problem plain: it leads to spaghetti code. We need some way to schedule concurrent processing logic, but without threads to ORGANIZE OUR STEPS we are left with a tangle of callbacks. Node uses callbacks, which are notorious. They're worse in Python because we have no anonymous closures. Yuck! REWRITE.

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
...     print('result of yield: {}'.format(result))
...     result2 = yield 2
...     print('result 2nd yield: {}'.format(result2))
...     
```

TODO: rename from gen() to gen_fn().

When Python compiles `gen` to bytecode, it sees the `yield` statement and knows that `gen` is a generator function. It sets a flag to remember this fact:

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

A Python generator encapsulates a stack frame plus a reference to some code, the body of `gen`:

```python
>>> g.gi_code.co_name
'gen'
```

All generators from calls to `gen` point to this same code. But each generator has its own stack frame. This stack frame is not on any actual stack, it sits in heap memory waiting to be used. The frame has a "last instruction" pointer, the instruction it executed most recently. In the beginning, the last instruction pointer is -1, meaning the generator has not begun:

```python
>>> g.gi_frame.f_lasti
-1
```

When we call `send`, the generator reaches its first `yield`, and pauses. The return value of `send` is 1, since that was passed to the `yield` expression:

```python
>>> g.send(None)
1
```

The generator's instruction pointer is now 3 bytecodes from the start, part way through the 56 bytes of compiled Python:

```python
>>> g.gi_frame.f_lasti
3
>>> len(g.gi_code.co_code)
56
```

The generator can be resumed at any time, from any function, because its stack frame is not actually on the stack: it's on the heap. TODO: explain more clearly.

We can send the value "hello" into the generator and it becomes the result of the `yield` expression, and the generator continues until it yields 2:

```python
>>> g.send('hello')
result of yield: hello
2
```

Its stack frame now contains the local variable `result`:

```python
>>> g.gi_frame.f_locals
{'result': 'hello'}
```

Other generators created from `gen_fn` will have their own stack frames and their own local variables.

When we call `send` again, the generator continues from its second `yield`, and finishes by raising the special `StopIteration` exception:

```python
>>> g.send('goodbye')
result of 2nd yield: goodbye
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

Review our `fetch` and `connected` methods that we wrote using callbacks:

```python
class Fetcher:
    def fetch(self):
        self.sock = socket.socket()
        self.sock.setblocking(False)
        try:
            self.sock.connect(('xkcd.com', 80))
        except BlockingIOError:
            pass
        selector.register(self.sock.fileno(), EVENT_WRITE, self.connected)

    def connected(self, key, mask):
        print('connected!')
        # And so on....
```

`fetch` starts connecting a socket, then registers the callback, `connected`, to be executed when the socket is ready. Now we can combine these two steps into one coroutine:

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

# Begin fetching http://xkcd.com/353/
fetcher = Fetcher('/353/')
Task(fetcher.fetch())
```

The task starts the `fetch` generator by sending `None` into it. Then `fetch` runs until it yields a future, which the task captures as `next_future`. When the socket is connected, the future runs its callback, which calls `step`, which resumes `fetch`.

##### Factoring coroutines with `yield from`

Connecting the socket isn't enough. We read from it, too:

TODO: don't register / unregister unnecessarily? Or comment why we do?

```python
    def fetch(self):
        # ... connection logic from above, then:
        while True:
            f = Future()

            def on_readable():
                selector.unregister(sock.fileno())
                f.set_result(sock.recv(4096))

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
...     print('result from yield: {}'.format(result))
...     result2 = yield 2
...     print('result from 2nd yield: {}'.format(result2))
...     
```

If we want to call this generator from another generator, we can delegate to it with `yield from`:

TODO: rename caller_fn() and caller

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

While `caller` yields from `gen`, `caller` does not advance. Notice that its instruction pointer remains at 9, the site of its `yield from` statement, even while the inner generator `gen` advances from one `yield` statement to the next. From our perspective outside `caller`, we cannot tell if the values it yields are from `caller` or from the generator it delegates to. And from inside `gen`, we cannot tell if values are sent in from `caller` or from outside it. The `yield from` statement acts as a frictionless channel, through which values flow in and out of `gen` until it completes.

TODO: generators with return values and `yield from`

We use `yield from` to build subroutines. Let us make a `read` subroutine to receive one chunk. `read_all` receives a whole message by yielding from `read`, and `fetch` yields from `read_all`.

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

The fetcher yields from our `read_all` method, which in turn yields from `read`. Miraculously, the Task class needs no modification: Task drives the outer `fetch` coroutine. When `read` yields a future, the task receives it through the chain of `yield from` statements, just the same as if the future were yielded directly from `fetch`. And when a future is resolved and the task sends its result into `fetch`, the value is received by `read`, just the same as if the task were driving `read` directly.

That's coroutines! This sketch of future and task outlines how asyncio attains the best of both worlds: concurrent I/O that's more efficient than threads and more legible than callbacks.

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

The crawler fetches its first page, extracts links, and adds them to a queue. After this things get complicated: we want our maximum number of workers running, but no more. Whenever a worker finishes fetching a page, it should immediately pull the next link from the queue. We'll pass through periods when there isn't enough work to go around, so some workers must sleep. When a worker hits a page rich with new links, then the queue suddenly grows and any sleeping workers should wake.

And finally, we need to know as soon as all work is complete so we can print a report and exit.

If the workers were threads, we'd use a synchronized queue[^5] from the Python standard library. Every time an item is put in the queue, it increments a count of "tasks". Worker threads call `task_done` after completing work on an item. The main thread blocks on `Queue.join` until each item put in the queue is matched by a `task_done` call, then it exits.

Coroutines use the exact same pattern with a queue from asyncio.[^6] First we import asyncio's queue:

```python
# In Python 3.5's asyncio, JoinableQueue is merged into Queue.
try:
    # Python 3.4.
    from asyncio import JoinableQueue as Queue
except ImportError:
    # Python 3.5.
    from asyncio import Queue
```

We'll collect the state for coordinating workers into a crawler class, and the main logic in its `crawl` method. We start `crawl` on a coroutine and run the asyncio event loop until `crawl` finishes:

```python
crawler = crawling.Crawler('http://xkcd.com',
                           max_redirect=10)

loop = asyncio.get_event_loop()
loop.run_until_complete(crawler.crawl())
```

In the beginning, the crawler is configured with a root URL and `max_redirect`, the number of redirects it is willing to follow to resolve any one URL. It puts them in its queue. The number of unfinished tasks in the queue is one.

```
class Crawler:
    def __init__(self, root_url, max_redirect):
        self.max_redirect = max_redirect
        self.q = Queue(loop=self.loop)
        self.q.put((root_url, self.max_redirect))
```

The `crawl` coroutine kicks off the workers. `crawl` is like the main thread: it blocks on `join` until all tasks are finished, while the workers run in the background.

```python
    @asyncio.coroutine
    def crawl(self):
        """Run the crawler until all finished."""
        workers = [asyncio.Task(self.work())
                   for _ in range(self.max_tasks)]

        # When all work is done, exit.
        yield from self.q.join()
        for w in workers:
            w.cancel()
```

We shall see momentarily the body of the `work` function, which each task runs above. But first it is interesting to note how we terminate the process when all work completes. When the `join` future resolves, the worker tasks are alive but suspended: they await more work but none comes. The main coroutine cancels them before exiting. Otherwise, as the Python interpreter shuts down and calls all objects' destructors, living tasks cry out:

```
ERROR:asyncio:Task was destroyed but it is pending!
```

Once `crawl` has canceled the workers, it exits. The event loop sees that the coroutine is complete, and it too exits:

```python
loop.run_until_complete(crawler.crawl())
```

That is all our main coroutine must do. How do the worker coroutines get links from the queue, fetch them, and parse them for more new links? Each runs the `work` method:

```python
    @asyncio.coroutine
    def work(self):
        while True:
            url, max_redirect = yield from self.q.get()
            yield from self.fetch(url, max_redirect)
            for url in self.extract_urls():
                if url not in self.urls:
                    self.add_url(url, self.max_redirect)

            self.q.task_done()
```

The worker coordinates with its fellows via the queue. It waits for links to appear in the queue with `yield from self.q.get()`. The queue's `get` method is itself a coroutine, which pauses until someone puts an item in the queue. Then `get` resumes and returns the item to the caller.

In our case, what we are putting in the queue is pairs, like:

```python
('http://xkcd.com/353', 10)
```

New links have ten redirects remaining. If fetching a URL results in a redirect, we decrement the redirects remaining and put the next location in the queue. Say the URL redirects to a URL that ends with a slash. We put the next URL in the queue, with nine redirects to go:

```python
('http://xkcd.com/353/', 10)
```

If we have already seen the destination URL with the slash, "http://xkcd.com/353/", we do not fetch it again.

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

Each worker runs the `work` coroutine independently. When a worker finishes fetching a page it extracts all the links and puts new ones in the queue, then calls `task_done` to decrement the counter. Eventually, a worker fetches a page whose URLs have all been fetched already, and there's no work left in the queue. Thus this worker's call to `task_done` decrements the counter to zero and `crawl` is unblocked.

TODO: `join` is a Future that's resolved by `task_done`, show it.

So, we coordinate coroutines with classic patterns from multi-threaded programming. There's no need to reinvent.

TODO: describe `loop.run_until_complete(crawler.crawl())`.

# Conclusion

Modern programs are increasingly I/O bound instead of CPU-bound, so async I/O is more often the right pattern. Async coroutines are the best of all worlds: more memory-efficient than threads for I/O-bound programs, and less prone to races than threads are because yield points are explicit. More legible than callbacks, too, and with sane exception handling and stack traces.

Now that you know how async I/O and coroutines work, you can largely forget the details. Async frameworks like Python's asyncio handle them for you. But knowing the fundamentals empowers you to code correctly and efficiently in modern async environments.

[^1]: https://mail.python.org/pipermail/python-ideas/2012-September/016192.html

[^2]: http://pyvideo.org/video/1667/keynote

[^3]: https://groups.google.com/d/msg/python-tulip/bmphRrryuFk/aB45sEJUomYJ

[^4]: https://glyph.twistedmatrix.com/2014/02/unyielding.html

[^5]: https://docs.python.org/3/library/queue.html

[^6]: https://docs.python.org/3/library/asyncio-sync.html

[^7]: For a complex solution to this problem, see http://www.tornadoweb.org/en/stable/stack_context.html
