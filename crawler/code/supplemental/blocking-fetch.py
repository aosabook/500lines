import socket


def threaded_method():
    sock = socket.socket()
    sock.connect(('aosabook.org', 80))
    request = 'GET /en/index.html HTTP/1.0\r\nHost: aosabook.org\r\n\r\n'
    sock.send(request.encode('ascii'))
    response = b''
    chunk = sock.recv(4096)
    while chunk:
        response += chunk
        chunk = sock.recv(4096)

    print('Got {} bytes'.format(len(response)))

threaded_method()
