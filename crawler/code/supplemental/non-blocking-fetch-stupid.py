import socket


sock = socket.socket()
sock.setblocking(False)
try:
    sock.connect(('aosabook.org', 80))
except BlockingIOError:
    pass

request = 'GET /en/index.html HTTP/1.0\r\nHost: aosabook.org\r\n\r\n'
encoded = request.encode('ascii')

while True:
    try:
        sock.send(encoded)
        break
    except OSError as e:
        pass

print('sent')
