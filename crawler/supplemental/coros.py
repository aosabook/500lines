import asyncio
import os
import subprocess
import sys


NCOROS = int(sys.argv[1])


lock = asyncio.Lock()


@asyncio.coroutine
def f():
    yield from lock.acquire()


@asyncio.coroutine
def main():
    with (yield from lock):
        coros = []
        for i in range(NCOROS):
            coros.append(asyncio.Task(f()))

        yield from asyncio.sleep(2)

        ps = subprocess.Popen(['ps', '-orss', str(os.getpid())],
                              stdout=subprocess.PIPE).communicate()[0]

        rss = ps.decode('ascii').split()[1]
        print('%d\t%s' % (NCOROS, rss))
        for c in coros:
            c.cancel()

asyncio.get_event_loop().run_until_complete(asyncio.Task(main()))
