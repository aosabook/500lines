import socket


def threaded_method():
    sock = socket.socket()
    sock.connect(('xkcd.com', 80))
    request = 'GET /353/ HTTP/1.0\r\nHost: xkcd.com\r\n\r\n'
    sock.send(request.encode('ascii'))
    response = b''
    chunk = sock.recv(4096)
    while chunk:
        response += chunk
        chunk = sock.recv(4096)

    print(response)

threaded_method()
