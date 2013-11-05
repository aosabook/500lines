'''Simple server that echoes data back.'''

import sys, socket

size = 1024

# Empty string for host means 'this machine'.
host, port = '', int(sys.argv[1])

# Create and bind a socket.
s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
s.bind((host, port))

# Wait for a connection request.
s.listen(True)
conn, addr = s.accept()
print 'Connected by', addr

# Receive and display a message.
data = conn.recv(size)
print 'server saw', `data`

# Echo it back to the sender.
conn.send(data)

# Put our toys away.
conn.close()
