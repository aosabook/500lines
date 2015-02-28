# An Async Web Crawler

Classical computer science focuses on efficient algorithms that complete a computation as quickly as possible. But many networked programs spend their time not computing, but waiting for responses from remote machines. These I/O-bound programs present a very different challenge: wait for a huge number of network responses efficiently. The modern answer to this problem is asynchronous I/O, or "async".

This chapter presents a simple web crawler. The crawler is an archetypal async application because it waits for many responses, but does little computation. The more pages it can fetch at once, the sooner it completes. If it devotes a thread per page it will run out of memory or other thread-related resource before it runs out of sockets, so it uses asynchronous I/O to avoid the need for threads.

We present the example in three stages. First, we show an async event loop and sketch a crawler that uses the loop with callbacks: it is very efficient but difficult to extend. We digress to explain Python generators, and how they are used as asynchronous coroutines, a stylish and robust alternative to callbacks. In the third stage, we show how coroutines use an async queue to coordinate with each other.

## The Task

A web crawler downloads all pages on a website. Beginning with a root URL, it fetches the page, parses it for links to pages it has not seen, and adds the links to a queue. When it fetches a page with no unseen links and the queue is empty, it stops.

This is easy to parallelize: as the crawler finds new links it launches many fetch operations on separate sockets. It parses responses as they arrive, adding new links to the queue. There may come some point of diminishing returns where too many connections degrades performance, so we cap the number of concurrent operations, and leave the remaining links in the queue until some have completed.

## The Traditional Approach

If a crawler is so parallelizable, how do we parallelize it? Traditionally we would start a thread pool: one thread per in-flight I/O operation. But threads are expensive, and operating systems enforce a variety of hard caps on the number of threads a process, user, or machine may have. On Jesse's system, a Python thread costs 22k of memory, and starting tens of thousands of threads causes failures. If we scale up to tens of thousands of simultaneous operations on concurrent connections, we run out of threads before we run out of connections. Per-thread overhead or system limits on threads are the bottleneck.

In his influential article "The C10K problem"[^8], Dan Kegel outlines the limitations of multithreading for I/O concurrency. He begins,

> It's time for web servers to handle ten thousand clients simultaneously, don't you think? After all, the web is a big place now.

Kegel coined the term "C10K" in 1999. Ten thousand connections sounds dainty now, but the problem has changed only in size, not in kind. Back then, using a thread per connection for C10K was impractical. Now the cap is orders of magnitude higher. Indeed, our toy web crawler would work just fine with threads. Yet for very large scale applications with hundreds of thousands of connections, the cap remains: there is a limit beyond which most systems can still create sockets, but have run out of threads. How can we overcome this?

## Async

### Single-Threaded I/O Concurrency

Asynchronous I/O frameworks schedule multiple operations on a single thread. How is this possible?

Operating systems have long offered ways for one thread to await I/O on multiple connections. Originally, they used functions like `select` and `poll`. In modern times, the demand for Internet applications with huge numbers of connections has led to `kqueue` on BSD and `epoll` on Linux. These APIs are similar to `poll`, but perform better with very large numbers of connections.

Python 3.4's `DefaultSelector` uses the best notification mechanism available on your system. We can write a loop that processes I/O notifications as we receive them:

```python
from selectors import DefaultSelector

selector = DefaultSelector()

def loop():
    while True:
        events = selector.select()
        for event_key, event_mask in events:
            callback = event_key.data
            callback()
```

The call to `select` awaits the next I/O events, then the loop runs the callbacks that are waiting for these events. Operations that have not completed yet remain pending until some future tick of the event loop.

To register for notifications about network I/O, we create a non-blocking socket and register it. For example, to connect asynchronously to xkcd.com:

```python
sock = socket.socket()
sock.setblocking(False)
try:
    sock.connect(('xkcd.com', 80))
except BlockingIOError:
    pass

def connected():
    selector.unregister(sock.fileno())
    print('connected!')

selector.register(sock.fileno(), EVENT_WRITE, connected)

loop()
```

Irritatingly, a non-blocking socket always throws an exception from `connect`. This exception simply replicates the irritating behavior of the underlying C function, which sets `errno` to the number `EINPROGRESS` to tell you it has begun and you should use an event-notification system to wait for it.

We disregard the spurious error and call `register`, passing in the socket's file descriptor and a Python function to run when the socket becomes writable. This function--the callback--is stored as `event_key.data`, which we retrieve and execute in the event loop above.

What have we demonstrated already? We showed how to begin an operation and execute a callback when it is ready. An async *framework* builds on the two features we have shown--non-blocking sockets and the event loop--to efficiently run concurrent operations on a single thread.

### Async's Problem

With the runty async framework we have built so far, how can we build a web crawler? Even a simple URL-fetcher is painful to write.

#### Demonstration

We begin with global sets of the URLs we have yet to fetch, and the URLs we have seen:

```python
urls_todo = set(['/'])
seen_urls = set(['/'])
```

The `seen_urls` set includes `urls_todo` plus completed URLs. The two sets are initialized the root URL "/".

Fetching a page will require a series of callbacks. The `connected` callback fires when a socket is connected, but then it must send an HTTP GET request to the server and await a response, so it will register another callback to read bytes from the server.

Let us collect these callbacks into a `Fetcher` object. It needs a URL, a socket object, and a place to accumulate the response bytes:

```python
class Fetcher:
    def __init__(self, url):
        self.response = b''
        self.url = url
        self.sock = None
```

We begin by calling `Fetcher.fetch`:

```python
    # Method on Fetcher class.
    def fetch(self):
        self.sock = socket.socket()
        self.sock.setblocking(False)
        try:
            self.sock.connect(('xkcd.com', 80))
        except BlockingIOError:
            pass
        selector.register(self.sock.fileno(),
                          EVENT_WRITE,
                          self.connected)
```

The `fetch` method begins connecting a socket. But notice the method returns before the connection completes. It must yield control to the event loop in order to await the connection. Imagine our total application was structured so:

```python
# Begin fetching http://xkcd.com/353/
fetcher = Fetcher('/353/')
fetcher.fetch()

loop()
```

All I/O notifications are processed in the event loop. Therefore `fetch` must return and allow the event loop to run, so that the application knows when the socket has connected. Only then does it run the `connected` callback, which was registered at the end of `fetch` above. Here is the implementation of `connected`:

```python
    # Method on Fetcher class.
    def connected(self, key, mask):
        print('connected!')
        selector.unregister(key.fd)
        request = 'GET {} HTTP/1.0\r\n\r\n'.format(self.url)
        self.sock.sendall(request.encode('ascii'))
        selector.register(key.fd,
                          EVENT_READ,
                          self.read_response)
```

The method sends a GET request. A more sophisticated application would be careful not to block if the whole request cannot be sent at once. But our request is small and our application unsophisticated. It blithely calls `sendall`, then waits for a response. Of course, it must register yet another callback and return control to the event loop. The final callback processes the server's response:

```python
    # Method on Fetcher class.
    def read_response(self, key, mask):
        global stopped

        chunk = self.sock.recv(4096)  # 4k chunk size.
        if chunk:
            self.response += chunk
        else:
            selector.unregister(key.fd)  # Done reading.
            links = self.parse_links()
            
            # Python set-logic:
            for link in links.difference(seen_urls):
                urls_todo.add(link)
                Fetcher(link).fetch()

            seen_urls.update(links)
            urls_todo.remove(self.url)
            if not urls_todo:
                stopped = True
```

The callback is executed each time `DefaultSelector` sees that the socket is readable, meaning the socket either has some data or it is closed.

The callback asks for up to four kilobytes of data from the socket. If less is ready, `chunk` contains whatever data is available. If there is more, `chunk` is four kilobytes long and the socket remains readable, so the event loop will run this callback again on the next tick. When the response is complete, the server has closed the socket and `chunk` is empty.

The `parse_links` method, not shown, returns a set of URLs. We start a new fetcher for each new URL, with no concurrency cap.

We add a global `stopped` variable and use it to control the loop:

```python
stopped = False

def loop():
    while not stopped:
        events = selector.select()
        for event_key, event_mask in events:
            callback = event_key.data
            callback()
```

Once all pages are downloaded the fetcher stops the global event loop.

#### The Trouble With Callbacks

This example makes async's problem plain: spaghetti code.

We need some way to express a series of computations and I/O operations, and schedule multiple such series of operations to run concurrently. But without threads, a series of operations cannot be collected into a single method: whenever a method begins an I/O operation it explicitly saves whatever state will be needed in the future, then exits. You are responsible for thinking about and writing this state-saving code.

Consider how simply we fetch a URL on a thread, using a conventional blocking socket:

```python
def blocking_fetch():
    sock = socket.socket()
    sock.connect(('xkcd.com', 80))
    request = 'GET {} HTTP/1.0\r\n\r\n'.format(self.url)
    sock.sendall(request.encode('ascii'))
    response = b''
    chunk = sock.recv(4096)
    while chunk:
        response += chunk
        chunk = sock.recv(4096)
    
    print(response)
```

A method that runs on a thread uses basic features of the programming language: it stores its temporary state, like `sock` and `response`, in local variables. The runtime remembers the method's "continuation", that is, what it planned to do after I/O completes, by remembering the thread's instruction pointer. You need not think about restoring its state after I/O.

But with a callback-based async framework, these language features are no help. While waiting for I/O, a callback must save its state explicitly, because it returns and loses its stack frame before I/O completes. In lieu of local variables, our callback-based example stores `sock` and `response` by setting attributes on `self`, the fetcher instance. And it register callbacks like `connected` and `read_response` in lieu of the instruction pointer. As the application's features grow, so does the complexity of the state that is manually saved across callbacks. Such onerous bookkeeping makes the programmer prone to migraines.

Even worse, what happens if a callback throws an exception, before it schedules the next callback in the chain? Say we did a poor job on the `parse_links` method and it throws an exception when it encounters misformatted HTML:

```
Traceback (most recent call last):
  File "loop-with-callbacks.py", line 112, in <module>
    callback(event_key, event_mask)
  File "loop-with-callbacks.py", line 55, in read_response
    links = self.parse_links()
  File "loop-with-callbacks.py", line 82, in parse_links
    raise ParseError
__main__.ParseError
```

The stack trace shows only that the event loop was running a callback. We do not remember what led to the error. The chain is broken on both ends: we forgot where we were going and whence we came. This loss of context is called "stack ripping", and in many cases it confounds the investigator. Stack ripping also prevents us from installing an exception handler for a chain of callbacks, the way a "try / except" block wraps a function call and its tree of descendents.[^7]

Even apart from the long debate about the relative efficiencies of multithreading and async, there is this other tension: threads are susceptible to data races if you make a mistake synchronizing them, but callbacks are stubborn to debug due to stack ripping. 

### Solution: Asynchronous Coroutines

We entice you with a promise. It is possible to write asynchronous code that combines the efficiency of callbacks with the understated elegance of traditional programming. This combination is achieved with a pattern called "coroutines". Using Python's standard asyncio package, and a library called "aiohttp", fetching a URL in a coroutine is very direct[^10]:

```python
    @asyncio.coroutine
    def fetch(self, url):
        response = yield from aiohttp.request('get', url)
        body = yield from response.read()
```

It is also scalable. Compared to the 22k memory per thread and the operating system's hard limits on threads, coroutines take barely 3k of memory on Jesse's system. Python can easily start hundreds of thousands of coroutines.

We digress to describe how coroutines are implemented, and trust you will enjoy reading the digression as much as we enjoyed writing it. When we return from the digression we shall show how coroutines are used in a web crawler.

#### How Python Generators Work

##### yield and send

Coroutines are built on a Python language feature called generator functions. This is a generator function:

```python
>>> def gen_fn():
...     result = yield 1
...     print('result of yield: {}'.format(result))
...     result2 = yield 2
...     print('result of 2nd yield: {}'.format(result2))
...     return 'done'
...     
```

When Python compiles `gen_fn` to bytecode, it sees the `yield` statement and knows that `gen_fn` is a generator function, not a regular one. It sets a flag to remember this fact:

```python
>>> # The generator flag is bit position 5.
>>> generator_bit = 1 << 5
>>> bool(gen_fn.__code__.co_flags & generator_bit)
True
```

When you call a generator function, Python sees the generator flag, and it does not actually run the function. Instead, it produces a generator:

```python
>>> gen = gen_fn()
>>> type(gen)
<class 'generator'>
```

A Python generator encapsulates a stack frame plus a reference to some code, the body of `gen_fn`:

```python
>>> gen.gi_code.co_name
'gen_fn'
```

All generators from calls to `gen_fn` point to this same code. But each has its own stack frame. This stack frame is not on any actual stack, it sits in heap memory waiting to be used. The frame has a "last instruction" pointer, the instruction it executed most recently. In the beginning, the last instruction pointer is -1, meaning the generator has not begun:

```python
>>> gen.gi_frame.f_lasti
-1
```

When we call `send`, the generator reaches its first `yield`, and pauses. The return value of `send` is 1, since that was passed to the `yield` expression:

```python
>>> gen.send(None)
1
```

The generator's instruction pointer is now 3 bytecodes from the start, part way through the 56 bytes of compiled Python:

```python
>>> gen.gi_frame.f_lasti
3
>>> len(gen.gi_code.co_code)
56
```

The generator can be resumed at any time, from any function, because its stack frame is not actually on the stack: it is on the heap. Its position in the call hierarchy is not fixed, and it need not obey the first-in, last-out order of execution that regular functions do. It is liberated, floating free like a cloud.

We can send the value "hello" into the generator and it becomes the result of the `yield` expression, and the generator continues until it yields 2:

```python
>>> gen.send('hello')
result of yield: hello
2
```

Its stack frame now contains the local variable `result`:

```python
>>> gen.gi_frame.f_locals
{'result': 'hello'}
```

Other generators created from `gen_fn` will have their own stack frames and their own local variables.

When we call `send` again, the generator continues from its second `yield`, and finishes by raising the special `StopIteration` exception:

```python
>>> gen.send('goodbye')
result of 2nd yield: goodbye
Traceback (most recent call last):
  File "<input>", line 1, in <module>
StopIteration: done
```

The exception has a value, which is the return value of the generator: the string "done".

##### coroutines

So a generator can pause, and be resumed with a value, and it has a return value. Sounds like a good primitive upon which to build an async programming model, without spaghetti callbacks! This programming model is a "coroutine": a routine that is cooperatively scheduled with other routines in the program. The distinction from threads is that threads are preemptively scheduled by the operating system, which can interrupt a thread at any time, whereas coroutines cooperate by voluntarily ceding control to the event loop so it can schedule other coroutines to run.

Let us see how coroutines are implemented in asyncio.

First we need a way to represent a future result, which a coroutine is waiting for. A very simple version:

```python
class Future:
    def __init__(self):
        self.result = None
        self._callbacks = []

    def add_done_callback(self, fn):
        self._callbacks.append(fn)

    def set_result(self, result):
        self.result = result
        for fn in self._callbacks:
            fn(self)
```

A future is initially "pending". It is "resolved" by a call to `set_result`.

We will adapt our callback-based fetcher to use futures and coroutines. Review our `fetch` and `connected` methods:

```python
class Fetcher:
    def fetch(self):
        self.sock = socket.socket()
        self.sock.setblocking(False)
        try:
            self.sock.connect(('xkcd.com', 80))
        except BlockingIOError:
            pass
        selector.register(self.sock.fileno(),
                          EVENT_WRITE,
                          self.connected)

    def connected(self, key, mask):
        print('connected!')
        # And so on....
```

The `fetch` method begins connecting a socket, then registers the callback, `connected`, to be executed when the socket is ready. Now we can combine these two steps into one coroutine:

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

        selector.register(self.sock.fileno(),
                          EVENT_WRITE,
                          on_connected)
        yield f
        print('connected!')
```

Now `fetch` is a generator, rather than a regular method, because it contains a `yield` statement. We create a pending future, then yield it to pause `fetch` until the socket is ready. The inner function `on_connected` resolves the future. But what resumes the generator? We need a coroutine driver. Let us call it "task":

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

loop()
```

The task starts the `fetch` generator by sending `None` into it. Then `fetch` runs until it yields a future, which the task captures as `next_future`. When the socket is connected, the future runs its callback `on_connected`, which calls `step`, which resumes `fetch`.

##### Factoring coroutines with `yield from`

Once the socket is connected, we send the HTTP GET request and read the server response. These steps need no longer be scattered among callbacks; we gather them into the same generator function:

```python
    def fetch(self):
        # ... connection logic from above, then:

        request = 'GET {} HTTP/1.0\r\n\r\n'.format(self.url)
        sock.sendall(request.encode('ascii'))

        while True:
            f = Future()

            def on_readable():
                selector.unregister(sock.fileno())
                f.set_result(sock.recv(4096))

            selector.register(sock.fileno(),
                              EVENT_READ,
                              on_readable)
            chunk = yield f
            if chunk:
                self.response += chunk
            else:
                # Done reading.
                break
```

This code, which reads a whole message from a socket, seems generally useful. How can we factor it out from `fetch` into a subroutine? Here is where Python 3's `yield from` statement comes in: it lets one generator *delegate* to another. Let us return to our simple generator example:

```python
>>> def gen_fn():
...     result = yield 1
...     print('result of yield: {}'.format(result))
...     result2 = yield 2
...     print('result of 2nd yield: {}'.format(result2))
...     return 'done'
...     
```

If we want to call this generator from another generator, we can delegate to it with `yield from`:

```python
>>> # Generator function:
>>> def caller_fn():
...     gen = gen_fn()
...     rv = yield from gen
...     print('return value: {}'.format(rv))
...
>>> # Generator from the generator function.
>>> caller = caller_fn()
```

The `caller` generator acts as if it were `gen`, the generator to which it delegates:

```python
>>> caller.send(None)
1
>>> caller.gi_frame.f_lasti
15
>>> caller.send('hello')
result of yield: hello
2
>>> caller.gi_frame.f_lasti  # Hasn't advanced.
15
>>> caller.send('goodbye')
result 2nd yield: goodbye
return value: done
Traceback (most recent call last):
  File "<input>", line 1, in <module>
StopIteration
```

While `caller` yields from `gen`, `caller` does not advance. Notice that its instruction pointer remains at 15, the site of its `yield from` statement, even while the inner generator `gen` advances from one `yield` statement to the next. From our perspective outside `caller`, we cannot tell if the values it yields are from `caller` or from the generator it delegates to. And from inside `gen`, we cannot tell if values are sent in from `caller` or from outside it. The `yield from` statement is a frictionless channel, through which values flow in and out of `gen` until it `gen` completes.

TODO: show how awesome exception handling & tracebacks are with `yield from`

Once `gen` does complete, its return value is the value of the `yield from` statement in `caller`. Notice that `caller` printed "return value: done" before it exited. Thus a coroutine can delegate work to a sub-coroutine with `yield from` and receive the result of the work when the sub-coroutine finishes.

Let us make a `read` subroutine to receive one chunk:

```python
def read(sock):
    f = Future()

    def on_readable():
        selector.unregister(sock.fileno())
        f.set_result(sock.recv(4096))  # Read 4k at a time.

    selector.register(sock.fileno(), EVENT_READ, on_readable)
    chunk = yield f
    return chunk
```

We will build on `read` with a `read_all` coroutine that receives a whole message:

```python
def read_all(sock):
    response = []
    chunk = yield from read(sock)
    while chunk:
        response.append(chunk)
        chunk = yield from read(sock)

    return b''.join(response)
```

If you ignore the ``yield from`` statement, you can imagine this is a conventional function that blocks while awaiting I/O. But in fact, ``read`` and ``read_all`` are coroutines. Yielding from ``read`` pauses ``read_all`` until the I/O completes. While ``read_all`` is paused, asyncio's event loop does other work and awaits other I/O events; ``read_all`` is resumed with the result of ``read`` on the next loop iteration after the I/O is ready.

Finally, we call `read_all` from `fetch`:

```python
class Fetcher:
    def fetch(self):
		 # ... connection logic from above, then:
        sock.sendall(request.encode('ascii'))
        self.response = yield from read_all(sock)
```

Miraculously, the Task class needs no modification. The task drives the outer `fetch` coroutine:

```python
Task(fetcher.fetch())
loop()
```

When `read` yields a future, the task receives it through the channel of `yield from` statements, precisely as if the future were yielded directly from `fetch`. When the loop resolves a future, the task sends its result into `fetch`, and the value is received by `read`, precisely as if the task were driving `read` directly.

To perfect our coroutine implementation, we polish out one mar: our code uses `yield` when it waits for a future, but `yield from` when it delegates to a sub-coroutine. It would be more refined if we used `yield from` whenever a coroutine pauses. Then a coroutine need not concern itself with what type of thing it awaits.

We can take advantage of the deep correspondence in Python between generators and iterators. Advancing a generator is, to the caller, the same as advancing an iterator. So we make our Future class iterable by implementing a special method:

```python
    # Method on Future class.
    def __iter__(self):
        # Tell Task to wait for completion.
        yield self
        return self.result
```

Now when we replace code like this:

```python
# f is a Future.
yield f
```

...with this:

```python
# f is a Future.
yield from f
```

...the outcome is precisely the same: the driving task receives the future from its call to `self.coro.send(result)`, and when the future is resolved it sends the new result back into the coroutine.

What is the advantage of using `yield from` everywhere instead of a mix of `yield` when waiting for futures and `yield from` when waiting for coroutines? It means a method can freely change its internal implementation without affecting the caller: it might be a normal method that returns a future that will resolve to a value, or it might be a coroutine that contains `yield from` statements and returns a value. In either case, the caller need only `yield from` the method in order to wait for it.

Gentle reader, we have reached the end of our enjoyable digression. This digression sketched an implementation of futures and tasks. It outlined how asyncio attains the best of both worlds: concurrent I/O that is more efficient than threads and more legible than callbacks. Of course, the real asyncio is much more sophisticated regarding zero-copy I/O, fair scheduling, canceling tasks, and handling exceptions.

To an asyncio user, coding with coroutines is much simpler than you saw here. In the code above we implemented coroutines from first principles, so you saw callbacks, tasks, and futures, non-blocking sockets and the call to ``select``. But when it comes time to build an application with asyncio, none of this appears in your code. As we promised, you can fetch a URL with code as sleek as this:

```python
    @asyncio.coroutine
    def fetch(self, url):
        response = yield from aiohttp.request('get', url)
        body = yield from response.read()
```

Satisfied with this digression, we return to our original assignment: to write an async web crawler.

### Coordinating Coroutines

The crawler fetches its first page, parses links, and adds them to a queue. After this, matters become complex: we want our maximum number of workers running, but no more. Whenever a worker finishes fetching a page, it should immediately pull the next link from the queue. We'll pass through periods when there isn't enough work to go around, so some workers must sleep. When a worker hits a page rich with new links, then the queue suddenly grows and any sleeping workers should wake.

And finally, we need to know as soon as all work is complete so we can print a report and exit.

If the workers were threads, we'd use a synchronized queue[^5] from the Python standard library. Every time an item is put in the queue, it increments a count of "tasks". Worker threads call `task_done` after completing work on an item. The main thread blocks on `Queue.join` until each item put in the queue is matched by a `task_done` call, then it exits.

Coroutines use the exact same pattern with a queue from asyncio.[^6] First we import asyncio's queue:

```python
# In Python 3.5, asyncio.JoinableQueue is merged into Queue.
try:
    from asyncio import JoinableQueue as Queue
except ImportError:
    from asyncio import Queue
```

We collect the state for coordinating workers into a crawler class, and the main logic in its `crawl` method. We start `crawl` on a coroutine and run asyncio's event loop until `crawl` finishes:

```python
crawler = crawling.Crawler('http://xkcd.com',
                           max_redirect=10)

loop = asyncio.get_event_loop()
loop.run_until_complete(crawler.crawl())
```

In the beginning, the crawler is configured with an initial URL and `max_redirect`, the number of redirects it is willing to follow to fetch any one URL. It puts the pair `(URL, max_redirect)` in the queue. (For the reason why, stay tuned.)

```python
class Crawler:
    def __init__(self, root_url, max_redirect):
        self.max_tasks = 10
        self.max_redirect = max_redirect
        self.q = Queue()
        self.seen_urls = set()
        
        # Put (URL, max_redirect) in the queue.
        self.q.put((root_url, self.max_redirect))
```

The number of unfinished tasks in the queue is now one. Back in our main script, we launch the event loop and the `crawl` method:

```python
loop.run_until_complete(crawler.crawl())
```

The `crawl` coroutine kicks off the workers. It is like a main thread: it blocks on `join` until all tasks are finished, while the workers run in the background.

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

It is interesting to note how we shut down the crawler. When the `join` future resolves, the worker tasks are alive but suspended: they wait for more URLs but none come. So, the main coroutine cancels them before exiting. Otherwise, as the Python interpreter shuts down and calls all objects' destructors, living tasks cry out:

```
ERROR:asyncio:Task was destroyed but it is pending!
```

Once `crawl` has canceled the workers, it exits. The event loop sees that the coroutine is complete, and it too exits:

```python
loop.run_until_complete(crawler.crawl())
```

The `crawl` method comprises all that our main coroutine must do. It is the worker coroutines that get links from the queue, fetch them, and parse them for more new links. Each worker runs the `work` coroutine independently:

```python
    @asyncio.coroutine
    def work(self):
        while True:
            url, max_redirect = yield from self.q.get()
            yield from self.fetch(url, max_redirect)
            self.q.task_done()
```

Python sees that this code contains `yield from` statements, and compiles it into a generator function. So in `crawl`, when the main coroutine calls `self.work` ten times, it does not actually execute this method: it only creates ten generator objects with references to this code. It wraps each in a Task. The Task receives each future the generator yields, and drives the generator by calling `send` with each future's result when the future resolves. Because the generators have their own stack frames, they run independently, with separate local variables and instruction pointers.

The worker coordinates with its fellows via the queue. It waits for new URLs with `yield from self.q.get()`. The queue's `get` method is itself a coroutine: it pauses until someone puts an item in the queue, then resumes and returns the item.

When a worker fetches a page it parses the links and puts new ones in the queue, then calls `task_done` to decrement the counter. Eventually, a worker fetches a page whose URLs have all been fetched already, and there is also no work left in the queue. Thus this worker's call to `task_done` decrements the counter to zero. Then `crawl`, which is waiting for the queue's `join` method, is unpaused and finishes.

We promised to explain why the items in the queue are pairs, like:

```python
# URL to fetch, and the number of redirects left.
('http://xkcd.com/353', 10)
```

New URLs have ten redirects remaining. Fetching this particular URL results in a redirect to a new location with a trailing slash. We decrement the number of redirects remaining, and put the next location in the queue:

```python
# URL with a trailing slash. Nine redirects left.
('http://xkcd.com/353/', 9)
```

The `aiohttp` library we use would follow redirects by default and give us the final response. We tell it not to, however, and handle redirects in the crawler, so it can coalesce redirect paths leading to the same destination: if we have already seen this URL, it is in ``self.seen_urls`` and we have already started on this path from a different entry point.

TODO: diagram here.

```python
    @asyncio.coroutine
    def fetch(self, url, max_redirect):
        response = yield from aiohttp.request(
            'get', url, allow_redirects=False)

        if is_redirect(response) and max_redirect > 0:
            next_url = response.headers['location']

            # Remember we have seen this URL.
            self.seen_urls.add(next_url)
            self.q.put_nowait((next_url, max_redirect - 1))
	     else:
	         links = yield from self.parse_links(response)
	         # Python set-logic:
	         for link in links.difference(self.seen_urls):
                self.q.put_nowait((link, self.max_redirect))
            self.seen_urls.update(links)
```

If the response is a page, rather than a redirect, `fetch` parses it for links and puts new ones in the queue.

If this were multithreaded code, it would be lousy with race conditions. For example, in the last few lines the worker checks if a link is in `seen_urls`, and if not the work puts it in the queue and adds it to `seen_urls`. If it were interrupted between the two operations, then another worker might parse the same link from a different page, observe that it is not in `seen_urls`, and add it to the queue. Now that same link is in the queue twice, leading (at best) to duplicated work.

However, a coroutine is only vulnerable to interruption at `yield from` statements. This is a key difference that makes coroutine code far less prone to races than multithreaded code: multithreaded code must enter a critical section explicitly, by grabbing a lock, otherwise it is interruptible. A Python coroutine, however, is uninterruptible by default, and only cedes control when it explicitly yields.

When `fetch` finishes processing the server response it returns to the caller, `work`. The `work` method calls `task_done` on the queue and then gets the next URL from the queue to be fetched.

When `fetch` puts new links in the queue it increments the count of unfinished tasks and keeps the main coroutine, which is waiting for `q.join`, paused. If, however, there are no unseen links and this was the last URL in the queue, then when `work` calls `task_done` the count of unfinished tasks falls to zero. That event unpauses `join` and the main coroutine completes, terminating the process.

The queue code that coordinates the workers and the main coroutine is like this[^9]:

```python
class Queue:
    def __init__(self):
        self._join_future = Future()
        self._unfinished_tasks = 0
        # ... other initialization ...
    
    def put_nowait(self, item):
        self._unfinished_tasks += 1
        # ... store the item ...

    def task_done(self):
        self._unfinished_tasks -= 1
        if self._unfinished_tasks == 0:
            self._join_future.set_result(None)

    @asyncio.coroutine
    def join(self):
        if self._unfinished_tasks > 0:
            yield from self._join_future
```

TODO: describe how `loop.run_until_complete(crawler.crawl())` works?

# Conclusion

We coordinate coroutines with classic patterns from multi-threaded programming. There is no need for reinvention. If we squint so that the `yield from` statements blur, a coroutine looks like a thread doing traditional blocking I/O. Compared to callbacks, coroutines provide an inviting idiom to the coder experienced with multithreading.

But when we open our eyes and focus on the `yield from` statements, we see they mark points when the coroutine cedes control and allows others to run. Unlike threads, coroutines display where our code can be interrupted and where it cannot. In his illuminating essay "Unyielding"[^4], Glyph Lefkowitz writes, "Threads make local reasoning difficult, and local reasoning is perhaps the most important thing in software development." Explicitly yielding, however, makes it possible to "understand the behavior (and thereby, the correctness) of a routine by examining the routine itself rather than examining the entire system."

So Python threads are the worst of both worlds: Python's global interpreter lock prevents them from actually executing computations in parallel, but non-deterministic switching makes them prone to races. Coroutines are the best of both: they are more memory-efficient than threads for I/O-bound programs, and more legible than callbacks, with sane exception handling and stack traces.

Now that you know how asyncio coroutines work, you can largely forget the details. The machinery is neatly tucked into a dapper interface. But understanding the fundamentals empowers you to code correctly and efficiently in modern async environments.

[^1]: https://mail.python.org/pipermail/python-ideas/2012-September/016192.html

[^2]: http://pyvideo.org/video/1667/keynote

[^3]: https://groups.google.com/d/msg/python-tulip/bmphRrryuFk/aB45sEJUomYJ

[^4]: https://glyph.twistedmatrix.com/2014/02/unyielding.html

[^5]: https://docs.python.org/3/library/queue.html

[^6]: https://docs.python.org/3/library/asyncio-sync.html

[^7]: For a complex solution to this problem, see http://www.tornadoweb.org/en/stable/stack_context.html

[^8]: http://www.kegel.com/c10k.html

[^9]: The actual asyncio.Queue implementation uses an asyncio.Event in place of the Future shown here. The difference is an Event can be reset, whereas a Future cannot transition from resolved back to pending.

[^10]: The `@asyncio.coroutine` decorator is not magical. In fact, if it decorates a generator function and the `PYTHONASYNCIODEBUG` environment variable is not set, the decorator does nothing but set an attribute, `_is_coroutine`, for the convenience of other parts of the framework.
