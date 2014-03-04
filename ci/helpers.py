import socket

def communicate(server, req):
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
    s.connect((server["host"], int(server["port"])))
    s.send(req)
    response = s.recv(1024)
    s.close()
    return response
