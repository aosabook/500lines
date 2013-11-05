'''Simple client that sends data read from standard input.'''

import sys
import socket

# Who to talk to, and what to say?
host, port = '', int(sys.argv[1])

# Create a socket, and connect to a server.
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((host, port))

# Send message.
data = sys.stdin.read()
s.send(data)

# Put our toys away.
s.close()
