import ctypes
import os
import struct
import time

# Experimental inotify support for the sake of illustration.

IN_MODIFY = 0x02
_libc = None

def _setup_libc():
    global _libc
    if _libc is not None:
        return
    ctypes.cdll.LoadLibrary('libc.so.6')
    _libc = ctypes.CDLL('libc.so.6', use_errno=True)
    _libc.inotify_add_watch.argtypes = [
        ctypes.c_int, ctypes.c_char_p, ctypes.c_uint32]
    _libc.inotify_add_watch.restype = ctypes.c_int

def wait_on(paths):
    # TODO: auto-detect when the OS does not offer libc or libc does not
    # offer inotify_wait, and fall back to looping_wait_on().
    _setup_libc()
    return inotify_wait_on(paths)

def looping_wait_on(paths):
    start = time.time()
    changed_paths = []
    while not changed_paths:
        time.sleep(0.5)
        changed_paths = [path for path in paths
                         if os.stat(path).st_mtime > start]
    return changed_paths

def inotify_wait_on(paths):
    paths = [path.encode('ascii') for path in paths]
    fd = _libc.inotify_init()
    descriptors = {}
    if fd == -1:
        raise OSError('inotify_init() error: {}'.format(
            os.strerror(ctypes.get_errno())))
    try:
        for path in paths:
            rv = _libc.inotify_add_watch(fd, path, 0x2)
            if rv == -1:
                raise OSError('inotify_add_watch() error: {}'.format(
                    os.strerror(ctypes.get_errno())))
            descriptors[rv] = path
        buf = os.read(fd, 1024)
        # TODO: continue with some more reads with 0.1 second timeouts
        # to empty the list of roughly-simultaneous events before
        # closing our file descriptor and returning?
    finally:
        pass #os.close(fd)
    time.sleep(0.1)  # until above TODO is done
    wd, mask, cookie, name_length = struct.unpack('iIII', buf)
    return [descriptors[wd].decode('ascii')]
