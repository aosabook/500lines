import os
import subprocess
import threading
import sys
import time


NTHREADS = int(sys.argv[1])
n_started = 0
n_started_lock = threading.Lock()
all_started = threading.Event()
lock = threading.Lock()


def f():
    global n_started
    with n_started_lock:
        n_started += 1
        if n_started == NTHREADS:
            all_started.set()
    lock.acquire()

with lock:
    for i in range(NTHREADS):
        t = threading.Thread(target=f)
        t.daemon = True
        t.start()

    all_started.wait(timeout=300)
    time.sleep(10)
    ps = subprocess.Popen(['ps', '-orss', str(os.getpid())],
                          stdout=subprocess.PIPE).communicate()[0]

    rss = ps.decode('ascii').split()[1]
    print('%d\t%s' % (NTHREADS, rss))
