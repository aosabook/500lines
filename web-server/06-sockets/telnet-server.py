'''Server that echoes data back until there is no more.'''

import sys, socket

size, host, port = 1024, '', int(sys.argv[1])

s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((host, port))
s.listen(True)
conn, addr = s.accept()
print 'Connected by', addr

result = ''
while True:
    data = conn.recv(size)
    print '...server:', `data`
    if not data:
        break
    result += data
print 'server saw', `result`

conn.close()
