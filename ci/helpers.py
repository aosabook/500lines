import socket

def communicate(host, port, req):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((host, port))
    s.send(req)
    response = s.recv(1024)
    s.close()
    return response
