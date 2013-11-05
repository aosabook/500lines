'''Simple client that sends data, and waits for an echo.'''

import sys, socket

# How much data to receive at one time?
size = 1024

# Who to talk to, and what to say?
host, port, message = sys.argv[1], int(sys.argv[2]), sys.argv[3]

# Create a socket, and connect to a server.
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.connect((host, port))

# Send message.
s.send(sys.argv[3])

# Wait for it to be echoed back.
data = s.recv(size)
print 'client received', `data`

# Put our toys away.
s.close()
